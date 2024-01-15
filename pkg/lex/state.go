package lex

import (
	"fmt"
	"log"
	"unicode"
)

const (
	substituteHex = '\U0000001A'
	singleQuote   = rune(39) // nolint:gomnd // rune of '

	alphaNumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
)

// lexStatementTokens lexes tokens within a statement.
func lexStatementTokens(lexer *lexerState) stateFunction { // nolint:gocyclo // good luck simplifying this
	switch currentRune := lexer.next(); {
	case isEOL(currentRune):
		lexer.emit(tokenEOL)
	case currentRune == eof:
		lexer.emit(tokenEOF)
		return nil
	case isSpace(currentRune):
		lexer.backup()
		return lexSpace
	case currentRune == picLeft:
		// special look-ahead for "PIC" so we don't break lexer.backup().
		if lexer.pos < Pos(len(lexer.input)) {
			// Look for PIC
			if (currentRune < '0' || '9' < currentRune) && lexer.peek() == 'I' && lexer.lookAhead(2) == 'C' { // nolint:gomnd // obvious meaning
				return lexPICToken
			}
			return lexIdentifier
		}
	case currentRune == 'O':
		return lexOCCURSToken(lexer)
	case currentRune == 'R':
		return lexREDEFINESToken(lexer)
	case currentRune == '+' || currentRune == '-' || ('0' <= currentRune && currentRune <= '9'):
		lexer.backup()
		return lexNumber
	case isAlphaNumeric(currentRune):
		lexer.backup()
		return lexIdentifier
	case currentRune == '.':
		lexer.emit(tokenDot)
	case currentRune == singleQuote:
		return lexEnum
	case currentRune <= unicode.MaxASCII && unicode.IsPrint(currentRune):
		lexer.emit(tokenChar)
	case currentRune == substituteHex:
		log.Printf("found SUBSTITUTE rune")
		lexer.emit(tokenEOF)
	default:
		e := fmt.Errorf("unrecognized character in action: %#U", currentRune)
		log.Println(e)
		return lexer.errorf(e.Error())
	}
	return lexStatementTokens(lexer)
}

// lexPICToken lexes a PIC token.
func lexPICToken(lexer *lexerState) stateFunction {
	var currentRune rune
	for {
		currentRune = lexer.next()
		if !isPICChar(currentRune) {
			// if after a pic character, we get a space it is likely
			// there may be an OCCURS definition to follow, e.g.
			// PIC X(10) OCCURS 12.
			if isSpace(currentRune) {
				switch nx := lexer.peek(); {
				case isPICChar(nx), isPICType(nx), nx == '.':
					continue

				default:
					lexer.backup()
					lexer.emit(tokenPIC)
					return lexSpace(lexer)
				}
			}
			// if we've just reached the terminator '.'
			// let's peek and see if it's actually an explicit decimal point
			// e.g. PIC 9(9).9(2) -> 123456789.50
			if isPICChar(lexer.peek()) {
				continue
			}
			if !lexer.atPICTerminator() {
				lexer.backup()
				break
			}
		}
	}

	if !lexer.atPICTerminator() {
		e := fmt.Errorf("bad character %#U", currentRune)
		log.Println(e)
		return lexer.errorf(e.Error())
	}
	lexer.emit(tokenPIC)
	return lexStatementTokens(lexer)
}
func lexREDEFINESToken(lexer *lexerState) stateFunction {
	if lexer.scanRedefinesToken() {
		lexer.emit(tokenREDEFINES)
	}
	return lexStatementTokens(lexer)
}

func lexOCCURSToken(lexer *lexerState) stateFunction {
	if lexer.scanOccursToken() {
		lexer.emit(tokenOCCURS)
	}
	return lexStatementTokens(lexer)
}

func (lexer *lexerState) scanRedefinesToken() bool {
	lexer.acceptRun("REDEFINES")
	if !isSpace(lexer.peek()) {
		lexer.next()
		return false
	}
	return true
}

func (lexer *lexerState) scanOccursToken() bool {
	lexer.acceptRun("OCCURS")
	if !isSpace(lexer.peek()) {
		lexer.next()
		return false
	}
	for {
		currentRune := lexer.next()
		if !isPICChar(currentRune) && !isSpace(currentRune) {
			if lexer.atTerminator() {
				lexer.backup()
				break
			}

			panic(fmt.Sprintf("bad character %#U", currentRune))
		}
	}
	return true
}

func (lexer *lexerState) atPICTerminator() bool {
	currentRune := lexer.peek()
	return currentRune == picRight
}

// lexIdentifier scans an alphanumeric.
func lexIdentifier(lexer *lexerState) stateFunction {
Loop:
	for {
		switch currentRune := lexer.next(); {
		case isAlphaNumeric(currentRune):
			// absorb.
		default:
			lexer.backup()
			word := lexer.input[lexer.start:lexer.pos]
			if !lexer.atTerminator() {
				e := fmt.Errorf("bad character %#U", currentRune)
				log.Println(e)
				return lexer.errorf(e.Error())
			}
			switch {
			case word == "true", word == "false":
				lexer.emit(tokenBool)
			default:
				lexer.emit(tokenIdentifier)
			}
			break Loop
		}
	}
	return lexStatementTokens(lexer)
}

// lexEnum scans an apostrophe-wrapped alphanumeric value.
func lexEnum(lexer *lexerState) stateFunction {
	for {
		switch currentRune := lexer.peek(); {
		case isAlphaNumeric(currentRune):
			// absorb
			lexer.acceptRun(alphaNumeric)
		default:
			if !lexer.atEnumTerminator() {
				e := fmt.Errorf("bad character %#U", currentRune)
				log.Println(e)
				return lexer.errorf(e.Error())
			}
			lexer.next()
			lexer.emit(tokenEnum)
			return lexStatementTokens(lexer)
		}
	}
}

// lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
// isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(lexer *lexerState) stateFunction {
	if !lexer.scanNumber() {
		return lexer.errorf("bad number syntax: %q", lexer.input[lexer.start:lexer.pos])
	}
	if sign := lexer.peek(); sign == '+' || sign == '-' {
		// Complex: 1+2i. No spaces, must end in 'i'.
		if !lexer.scanNumber() || lexer.input[lexer.pos-1] != 'i' {
			return lexer.errorf("bad number syntax: %q", lexer.input[lexer.start:lexer.pos])
		}
		lexer.emit(tokenComplex)
	} else {
		lexer.emit(tokenNumber)
	}

	return lexStatementTokens(lexer)
}

func (l *lexerState) scanNumber() bool { // nolint:gocyclo // good luck simplifying this
	// Optional leading sign.
	l.accept("+-")
	// Is it hex?
	digits := "0123456789_"
	if l.accept("0") {
		// Note: Leading 0 does not mean octal in floats.
		switch {
		case l.accept("xX"):
			digits = "0123456789abcdefABCDEF_"
		case l.accept("oO"):
			digits = "01234567_"
		case l.accept("bB"):
			digits = "01_"
		}
	}

	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	if len(digits) == 10+1 && l.accept("eE") {
		l.accept("+-")
		l.acceptRun("0123456789_")
	}
	if len(digits) == 16+6+1 && l.accept("pP") {
		l.accept("+-")
		l.acceptRun("0123456789_")
	}
	// Is it imaginary?
	l.accept("i")
	// Next thing mustn't be alphanumeric.
	if isAlphaNumeric(l.peek()) {
		l.next()
		return false
	}
	return true
}

// lexSpace scans a run of space characters.
// We have not consumed the first space, which is known to be present.
// Take care if there is a trim-marked right delimiter, which starts with a space.
func lexSpace(l *lexerState) stateFunction {
	for {
		r := l.peek()
		if !isSpace(r) {
			break
		}

		l.next()
	}
	l.emit(tokenSpace)
	return lexStatementTokens(l)
}

// atTerminator reports whether the input is at valid termination character to
// appear after an identifier.
func (l *lexerState) atTerminator() bool {
	r := l.peek()
	if isSpace(r) || isEOL(r) {
		return true
	}
	switch r {
	case eof, '.', ',', '|', ':', ')', '(':
		return true
	}

	return false
}

func (l *lexerState) atEnumTerminator() bool {
	r := l.peek()
	return r == singleQuote
}

// isSpace reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t'
}

// isEOL reports whether r is an end-of-line character.
func isEOL(r rune) bool {
	return r == '\r' || r == '\n'
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || r == '-' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

func isPICChar(r rune) bool {
	_, ok := picChars[r]
	return ok || unicode.IsNumber(r)
}

func isPICType(r rune) bool {
	_, ok := picTypes[r]
	return ok
}
