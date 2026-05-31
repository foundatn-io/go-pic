package lex

import (
	"unicode"
)

const (
	substituteHexElement = '\U0000001A'
	singleQuoteElement   = rune(39) // rune of '
	alphaNumericElements = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
)

// lexStatementTokens is the lexer's central dispatcher: it consumes one rune,
// emits or routes it to the matching sub-state, and returns the next
// stateFunction for run() to drive. Returning the next state (rather than
// calling it) keeps the scan iterative and the stack flat.
func lexStatementTokens(lexer *lexerState) stateFunction { //nolint:gocyclo // dispatch switch: one arm per token shape
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
		return lexOccursToken
	case currentRune == 'R':
		return lexRedefinesToken
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
		lexer.emit(tokenKindEOF)
	default:
		return lexer.errorf("unrecognized character in action: %#U", currentRune)
	}
	return lexStatementTokens
}

// lexPICToken consumes a full PIC clause token (e.g. "PIC X(10)" or
// "PIC 9(9).9(2)") and emits a tokenKindPIC.
//
// Invariant on entry: the lexer has already consumed the 'P' that begins the
// "PIC" keyword; the start position therefore includes "PIC …".
//
// Termination rules:
//  1. A space followed by a non-PIC character (e.g. OCCURS) ends the token:
//     backup past the space, emit, then hand off to lexSpace.
//  2. A non-PIC character that is followed by another PIC character is an
//     embedded separator (the explicit '.' in "9(9).9(2)"): skip it.
//  3. Any other non-PIC character: backup, confirm we are sitting before the
//     terminating '.', emit the token, and continue scanning.
//  4. EOF or an unexpected character inside the PIC body: return an error.
func lexPICToken(l *lexerState) stateFunction {
	for {
		r := l.next()
		switch {
		case isPICChar(r):
			// Normal PIC character — keep accumulating.

		case isSpace(r):
			// Space inside a PIC: look ahead to decide whether it is part of
			// the PIC body or a boundary before OCCURS / REDEFINES.
			next := l.peek()
			if isPICChar(next) || isPICType(next) || next == '.' {
				// Space is interior (e.g. "PIC X(10) .9(2)" is unusual but
				// valid).  Skip and continue accumulating.
				continue
			}
			// Space is a boundary — token ends before the space.
			l.backup()
			l.emit(tokenKindPIC)
			return lexSpace

		default:
			// Non-PIC, non-space character (e.g. an explicit decimal '.' or
			// an unexpected char).
			if isPICChar(l.peek()) {
				// The separator between two PIC groups (e.g. the '.' in
				// "9(9).9(2)") — continue accumulating.
				continue
			}
			// We have reached the end of the PIC body.  Back up so that the
			// next state function sees this character (typically the
			// terminating '.').
			l.backup()
			if !l.atPICTerminator() {
				return l.errorf("unexpected character %#U in PIC definition", r)
			}
			l.emit(tokenKindPIC)
			return lexStatementTokens
		}
	}
}

// lexRedefinesToken scans a REDEFINES keyword and emits tokenKindREDEFINES.
func lexRedefinesToken(lexer *lexerState) stateFunction {
	if lexer.scanRedefinesToken() {
		lexer.emit(tokenKindREDEFINES)
	}
	return lexStatementTokens
}

// lexOccursToken scans an OCCURS keyword and its count, emitting
// tokenKindOCCURS, or returns a lexer error if the clause is malformed.
func lexOccursToken(lexer *lexerState) stateFunction {
	valid, err := lexer.scanOccursToken()
	if err != nil {
		return lexer.errorf("%s", err.Error())
	}
	if valid {
		lexer.emit(tokenKindOCCURS)
	}
	return lexStatementTokens
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
		return lexer.errorf("bad character %#U", lexer.current())
	}

	toEmit := tokenKindIdentifier
	if word == "true" || word == "false" {
		toEmit = tokenKindBool
	}
	lexer.emit(toEmit)
	return lexStatementTokens
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
		return lexer.errorf("bad character %#U", lexer.current())
	}

	lexer.next()
	lexer.emit(tokenKindEnum)
	return lexStatementTokens
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
	return lexStatementTokens
}

// lexSpace is a state function for the lexer. It scans the input for spaces
// and emits a tokenSpace when it encounters one or more spaces. After emitting,
// it transitions the lexer to the lexStatementTokens state.
func lexSpace(lexer *lexerState) stateFunction {
	for isSpace(lexer.peek()) {
		lexer.next()
	}
	lexer.emit(tokenKindSpace)
	return lexStatementTokens
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

// isAlphaNumeric reports whether currentChar may appear in a COBOL identifier:
// a letter, a digit, an underscore, or a hyphen (data names are hyphenated).
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
