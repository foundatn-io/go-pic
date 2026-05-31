package lex

import (
	"fmt"
)

var (
	// picChars is the set of characters that may appear inside a PIC token,
	// including the keyword letters (P, I, C), type symbols, and parentheses.
	picChars = map[rune]struct{}{
		'P': {}, 'I': {}, 'C': {},
		leftParenthesis: {}, rightParenthesis: {},
		'X': {}, '9': {}, 'S': {}, 'V': {}, 'A': {},
	}

	// picTypes is the set of type-indicator characters that may follow a space
	// inside a PIC token (used to distinguish a mid-token space from a token
	// boundary before OCCURS or REDEFINES).
	picTypes = map[rune]struct{}{
		'X': {}, '9': {}, 'S': {}, 'V': {}, 'A': {},
	}
)

const (
	tokenKindError      tokenKind = iota // error occurred; value is text of error
	tokenKindBool                        // boolean constant
	tokenKindChar                        // printable ASCII character; grab bag for comma etc.
	tokenKindComplex                     // complex constant (1+2i); imaginary is just a number
	tokenKindEOF                         // end of file
	tokenKindEOL                         // end of line
	tokenKindIdentifier                  // Name of PIC or group
	tokenKindNumber                      // simple number, including imaginary
	tokenKindSpace                       // run of spaces separating arguments
	tokenKindDot                         // the cursor, spelled '.'
	tokenKindOCCURS                      // OCCURS keyword
	tokenKindPIC                         // PIC keyword
	tokenKindREDEFINES                   // REDEFINES keyword
	tokenKindEnum                        // enum example: 'Y' 'N' 'T' 'F'

	tokenKindCount // sentinel: total number of token kinds; must stay last
)

const (
	endOfFile        = -1
	picLeft          = 'P'
	picRight         = '.'
	leftParenthesis  = '('
	rightParenthesis = ')'
)

const (
	endOfFileStr = "EOF"
)

// tokenKind identifies the type of lex tokens.
type tokenKind int

// token represents a token or text string returned from the scanner.
type token struct {
	kind       tokenKind // The type of this token.
	position   Pos       // The starting position, in bytes, of this token in the input string.
	value      string    // The value of this token.
	lineNumber int       // The line number at the start of this token.
}

// String returns a string representation of the token.
func (t token) String() string {
	switch t.kind {
	case tokenKindEOF:
		return endOfFileStr
	case tokenKindError:
		return t.value
	default:
		return fmt.Sprintf("%q", t.value)
	}
}
