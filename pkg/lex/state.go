package lex

import (
	"fmt"
	"log"
	"unicode"
)

const (
	substituteHex = '\U0000001A'
)

func lexInsideStatement(l *lexer) stateFn { // nolint:gocyclo // good luck simplifying this
	switch r := l.next(); {
	case isEOL(r):
		l.emit(itemEOL)

	case r == eof:
		l.emit(itemEOF)
		return nil

	case isSpace(r):
		l.backup()
		return lexSpace

	case r == picLeft:
		// special look-ahead for "PIC" so we don't break l.backup().
		if l.pos < Pos(len(l.input)) {
			// Look for PIC
			if (r < '0' || '9' < r) && l.peek() == 'I' && l.lookAhead(2) == 'C' { // nolint:gomnd // obvious meaning
				return lexPIC
			}

			return lexIdentifier
		}

	case r == 'O':
		return lexOCCURS(l)

	case r == 'R':
		return lexREDEFINES(l)

	case r == '+' || r == '-' || ('0' <= r && r <= '9'):
		l.backup()
		return lexNumber

	case isAlphaNumeric(r):
		l.backup()
		return lexIdentifier

	case r == '.':
		l.emit(itemDot)

	case r <= unicode.MaxASCII && unicode.IsPrint(r):
		l.emit(itemChar)

	case r == substituteHex:
		log.Printf("found SUBSTITUTE rune")
		l.emit(itemEOF)

	default:
		e := fmt.Errorf("unrecognized character in action: %#U", r)
		log.Println(e)
		return l.errorf(e.Error())
	}

	return lexInsideStatement(l)
}

func lexPIC(l *lexer) stateFn {
	var r rune
	for {
		r = l.next()

		if !isPICChar(r) {
			// if after a pic character, we get a space it is likely
			// there may be an OCCURS definition to follow, e.g.
			// PIC X(10) OCCURS 12.
			if isSpace(r) {
				switch nx := l.peek(); {
				case isPICChar(nx), isPICType(nx), nx == '.':
					continue

				default:
					l.backup()
					l.emit(itemPIC)
					return lexSpace(l)
				}
			}

			// if we've just reached the terminator '.'
			// let's peek and see if it's actually an explicit decimal point
			// e.g. PIC 9(9).9(2) -> 123456789.50
			if isPICChar(l.peek()) {
				continue
			}

			if !l.atPICTerminator() {
				l.backup()
				break
			}
		}
	}

	if !l.atPICTerminator() {
		e := fmt.Errorf("bad character %#U", r)
		log.Println(e)
		return l.errorf(e.Error())
	}

	l.emit(itemPIC)
	return lexInsideStatement(l)
}

func lexREDEFINES(l *lexer) stateFn {
	if l.scanRedefines() {
		l.emit(itemREDEFINES)
	}

	return lexInsideStatement(l)
}

func lexOCCURS(l *lexer) stateFn {
	if l.scanOccurs() {
		l.emit(itemOCCURS)
	}

	return lexInsideStatement(l)
}

func (l *lexer) scanRedefines() bool {
	l.acceptRun("REDEFINES")
	if !isSpace(l.peek()) {
		l.next()
		return false
	}

	return true
}

func (l *lexer) scanOccurs() bool {
	l.acceptRun("OCCURS")
	if !isSpace(l.peek()) {
		l.next()
		return false
	}

	for {
		r := l.next()
		if !isPICChar(r) && !isSpace(r) {
			if l.atTerminator() {
				l.backup()
				break
			}

			panic(fmt.Sprintf("bad character %#U", r))
		}
	}

	return true
}

func (l *lexer) atPICTerminator() bool {
	r := l.peek()
	return r == picRight
}

// lexIdentifier scans an alphanumeric.
func lexIdentifier(l *lexer) stateFn {
Loop:
	for {
		switch r := l.next(); {
		case isAlphaNumeric(r):
			// absorb.
		default:
			l.backup()
			word := l.input[l.start:l.pos]
			if !l.atTerminator() {
				e := fmt.Errorf("bad character %#U", r)
				log.Println(e)
				return l.errorf(e.Error())
			}

			switch {
			case word == "true", word == "false":
				l.emit(itemBool)

			default:
				l.emit(itemIdentifier)
			}

			break Loop
		}
	}

	return lexInsideStatement(l)
}

// lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
// isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(l *lexer) stateFn {
	if !l.scanNumber() {
		return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
	}

	if sign := l.peek(); sign == '+' || sign == '-' {
		// Complex: 1+2i. No spaces, must end in 'i'.
		if !l.scanNumber() || l.input[l.pos-1] != 'i' {
			return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
		}

		l.emit(itemComplex)
	} else {
		l.emit(itemNumber)
	}

	return lexInsideStatement(l)
}

func (l *lexer) scanNumber() bool { // nolint:gocyclo // good luck simplifying this
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
func lexSpace(l *lexer) stateFn {
	var r rune
	var numSpaces int
	for {
		r = l.peek()
		if !isSpace(r) {
			break
		}

		l.next()
		numSpaces++
	}

	l.emit(itemSpace)
	return lexInsideStatement(l)
}

// atTerminator reports whether the input is at valid termination character to
// appear after an identifier.
func (l *lexer) atTerminator() bool {
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
