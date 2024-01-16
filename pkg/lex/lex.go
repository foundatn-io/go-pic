package lex

import (
	"fmt"
	"log"
	"unicode"
)

const (
	substituteHexElement = '\U0000001A'
	singleQuoteElement   = rune(39) // rune of '
	alphaNumericElements = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
)

// lexStatementTokens lexes tokens within a statement.
func lexStatementTokens(lexer *lexerState) stateFunction { //nolint:gocyclo // good luck simplifying this
	switch currentRune := lexer.next(); {
	case isEOL(currentRune):
		lexer.emit(tokenKindEOL)
	case currentRune == endOfFile:
		lexer.emit(tokenKindEOF)
		return nil
	case isSpace(currentRune):
		lexer.backup()
		return lexSpace
	case currentRune == picLeft:
		// special look-ahead for "PIC" so we don't break lexerState.backup().
		if lexer.pos < Pos(len(lexer.input)) {
			// Look for PIC
			if (currentRune < '0' || '9' < currentRune) && lexer.peek() == 'I' && lexer.lookAhead(2) == 'C' { //nolint:gomnd // obvious meaning
				return lexPICToken
			}
			return lexIdentifier
		}
	case currentRune == 'O':
		return lexOccursToken(lexer)
	case currentRune == 'R':
		return lexRedefinesToken(lexer)
	case currentRune == '+' || currentRune == '-' || ('0' <= currentRune && currentRune <= '9'):
		lexer.backup()
		return lexNumber
	case isAlphaNumeric(currentRune):
		lexer.backup()
		return lexIdentifier
	case currentRune == '.':
		lexer.emit(tokenKindDot)
	case currentRune == singleQuoteElement:
		return lexEnum
	case currentRune <= unicode.MaxASCII && unicode.IsPrint(currentRune):
		lexer.emit(tokenKindChar)
	case currentRune == substituteHexElement:
		log.Printf("found SUBSTITUTE rune")
		lexer.emit(tokenKindEOF)
	default:
		errorMessage := fmt.Errorf("unrecognized character in action: %#U", currentRune)
		log.Println(errorMessage)
		return lexer.errorf(errorMessage.Error())
	}
	return lexStatementTokens(lexer)
}

// lexPICToken lexes a PIC token.
//
// TODO: revise this function, flow control is a mess.
func lexPICToken(lexerState *lexerState) stateFunction {
	var currentRune rune
	for {
		currentRune = lexerState.next()
		if !isPICChar(currentRune) {
			// if after a pic character, we get a space it is likely
			// there may be an OCCURS definition to follow, e.g.
			// PIC X(10) OCCURS 12.
			if isSpace(currentRune) {
				switch nextRune := lexerState.peek(); {
				case isPICChar(nextRune), isPICType(nextRune), nextRune == '.':
					continue

				default:
					lexerState.backup()
					lexerState.emit(tokenKindPIC)
					return lexSpace(lexerState)
				}
			}
			// if we've just reached the terminator '.'
			// let's peek and see if it's actually an explicit decimal point
			// e.g. PIC 9(9).9(2) -> 123456789.50
			if isPICChar(lexerState.peek()) {
				continue
			}
			if !lexerState.atPICTerminator() {
				lexerState.backup()
				break
			}
		}
	}

	if !lexerState.atPICTerminator() {
		errorMessage := fmt.Errorf("bad character %#U", currentRune)
		log.Println(errorMessage)
		return lexerState.errorf(errorMessage.Error())
	}
	lexerState.emit(tokenKindPIC)
	return lexStatementTokens(lexerState)
}

// lexRedefinesToken is a state function for the lexer. It scans the input for a
// REDEFINES token. If it encounters a REDEFINES token, it emits a tokenREDEFINES.
func lexRedefinesToken(lexer *lexerState) stateFunction {
	if lexer.scanRedefinesToken() {
		lexer.emit(tokenKindREDEFINES)
	}
	return lexStatementTokens(lexer)
}

// lexOccursToken is a state function for the lexer. It scans the input for an
// OCCURS token. If it encounters an OCCURS token, it emits a tokenOCCURS. If
// it encounters an error while scanning, it returns an error.
func lexOccursToken(lexer *lexerState) stateFunction {
	valid, err := lexer.scanOccursToken()
	if err != nil {
		return lexer.errorf(err.Error())
	}
	if valid {
		lexer.emit(tokenKindOCCURS)
	}
	return lexStatementTokens(lexer)
}

// lexIdentifier is a state function for the lexer. It scans the input for alphanumeric
// characters and emits a tokenBool when it encounters the words "true" or "false",
// and emits a tokenIdentifier for other words. If it encounters a character that is
// not a terminator when finished globbing alphanumeric characters, it returns an error.
func lexIdentifier(lexer *lexerState) stateFunction {
	for isAlphaNumeric(lexer.next()) {
	}
	lexer.backup()

	// When finished globbing alphanumeric characters, evaluate the word.
	word := lexer.input[lexer.start:lexer.pos]
	if !lexer.atTerminator() {
		err := fmt.Errorf("bad character %#U", lexer.current())
		log.Println(err)
		return lexer.errorf(err.Error())
	}

	toEmit := tokenKindIdentifier
	if word == "true" || word == "false" {
		toEmit = tokenKindBool
	}
	lexer.emit(toEmit)
	return lexStatementTokens(lexer)
}

// lexEnum is a state function for the lexer. It scans the input for alphanumeric
// characters and emits a tokenEnum when it encounters a sequence of alphanumeric
// characters followed by an enum terminator. If it encounters a character that is
// not alphanumeric and not an enum terminator, it returns an error.
func lexEnum(lexer *lexerState) stateFunction {
	for isAlphaNumeric(lexer.peek()) {
		lexer.acceptRun(alphaNumericElements)
	}

	if !lexer.atEnumTerminator() {
		err := fmt.Errorf("bad character %#U", lexer.current())
		log.Println(err)
		return lexer.errorf(err.Error())
	}

	lexer.next()
	lexer.emit(tokenKindEnum)
	return lexStatementTokens(lexer)
}

// lexNumber is a state function for the lexer. It scans the input for numbers
// and emits a tokenNumber when it encounters a number. If it encounters a complex
// number (a number followed by '+' or '-' and another number ending with 'i'),
// it emits a tokenComplex. If the number syntax is incorrect, it returns an error.
//
// This isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(lexer *lexerState) stateFunction {
	errMsg := "bad number syntax: %q"
	if !lexer.scanNumber() {
		return lexer.errorf(errMsg, lexer.input[lexer.start:lexer.pos])
	}
	toEmit := tokenKindNumber
	if sign := lexer.peek(); sign == '+' || sign == '-' {
		// Complex: 1+2i. No spaces, must end in 'i'.
		if !lexer.scanNumber() || lexer.input[lexer.pos-1] != 'i' {
			return lexer.errorf(errMsg, lexer.input[lexer.start:lexer.pos])
		}
		toEmit = tokenKindComplex
	}
	lexer.emit(toEmit)
	return lexStatementTokens(lexer)
}

// lexSpace is a state function for the lexer. It scans the input for spaces
// and emits a tokenSpace when it encounters one or more spaces. After emitting,
// it transitions the lexer to the lexStatementTokens state.
func lexSpace(lexer *lexerState) stateFunction {
	for isSpace(lexer.peek()) {
		lexer.next()
	}
	lexer.emit(tokenKindSpace)
	return lexStatementTokens(lexer)
}

// isSpace reports whether the currentChar is a space character.
// It checks if the currentChar is either a space (' ') or a tab ('\t').
func isSpace(currentChar rune) bool {
	return currentChar == ' ' || currentChar == '\t'
}

// isEOL reports whether the currentChar is an end-of-line character.
// It checks if the currentChar is either a carriage return ('\r') or a newline ('\n').
func isEOL(currentChar rune) bool {
	return currentChar == '\r' || currentChar == '\n'
}

// isAlphaNumeric reports whether the currentChar is an alphabetic, digit, or underscore.
// It checks if the currentChar is an underscore ('_'), a hyphen ('-'), a letter, or a digit.
func isAlphaNumeric(currentChar rune) bool {
	return currentChar == '_' || currentChar == '-' || unicode.IsLetter(currentChar) || unicode.IsDigit(currentChar)
}

// isPICChar reports whether the currentChar is a PIC character.
// It checks if the currentChar is a PIC character or a number.
func isPICChar(currentChar rune) bool {
	_, ok := picChars[currentChar]
	return ok || unicode.IsNumber(currentChar)
}

// isPICType reports whether the currentChar is a PIC type.
// It checks if the currentChar is a PIC type.
func isPICType(currentChar rune) bool {
	_, ok := picTypes[currentChar]
	return ok
}
