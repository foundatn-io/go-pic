package lex

import (
	"fmt"
)

var (
	picChars = map[rune]struct{}{
		'P': {}, 'I': {}, 'C': {}, leftParen: {}, rightParen: {}, 'X': {}, '9': {}, 'S': {}, 'V': {},
	}

	picTypes = map[rune]struct{}{
		'X': {}, '9': {}, 'S': {}, 'V': {},
	}
)

const (
	tokenError      tokenType = iota // error occurred; value is text of error
	tokenBool                        // boolean constant
	tokenChar                        // printable ASCII character; grab bag for comma etc.
	tokenComplex                     // complex constant (1+2i); imaginary is just a number
	tokenEOF                         // end of file
	tokenEOL                         // end of line
	tokenIdentifier                  // Name of PIC or group
	tokenNumber                      // simple number, including imaginary
	tokenSpace                       // run of spaces separating arguments
	tokenDot                         // the cursor, spelled '.'
	tokenOCCURS                      // OCCURS keyword
	tokenPIC                         // PIC keyword
	tokenREDEFINES                   // REDEFINES keyword
	tokenEnum                        // enum example: 'Y' 'N' 'T' 'F'
)

const (
	eof        = -1
	picLeft    = 'P'
	picRight   = '.'
	leftParen  = '('
	rightParen = ')'
)

// tokenType identifies the type of lex tokens.
type tokenType int

// token represents a token or text string returned from the scanner.
type token struct {
	typ  tokenType // The type of this token.
	pos  Pos       // The starting position, in bytes, of this token in the input string.
	val  string    // The value of this token.
	line int       // The line number at the start of this token.
}

// String returns a string representation of the token.
func (t token) String() string {
	switch t.typ {
	case tokenEOF:
		return "EOF"
	case tokenError:
		return t.val
	default:
		return fmt.Sprintf("%q", t.val)
	}
}
