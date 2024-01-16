package lex

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

// Constants representing valid digits for different number systems.
const (
	// decimalDigits represents the valid digits in a decimal number.
	// It includes the digits 0-9 and the underscore for digit grouping.
	decimalDigits = "0123456789_"

	// hexDigits represents the valid digits in a hexadecimal number.
	// It includes the digits 0-9, letters a-f (both lowercase and uppercase) for digits 10-15,
	// and the underscore for digit grouping.
	hexDigits = "0123456789abcdefABCDEF_"

	// octalDigits represents the valid digits in an octal number.
	// It includes the digits 0-7 and the underscore for digit grouping.
	octalDigits = "01234567_"

	// binaryDigits represents the valid digits in a binary number.
	// It includes the digits 0-1 and the underscore for digit grouping.
	binaryDigits = "01_"
)

// stateFunction represents the state of the scanner as a function that returns the next state.
type stateFunction func(*lexerState) stateFunction

// lexerState holds the state of the scanner.
type lexerState struct {
	name      string  // the name of the input; used only for error reports
	input     string  // the string being scanned
	pos       Pos     // current position in the input
	start     Pos     // start position of this item
	width     Pos     // width of last rune read from input
	items     []token // channel of scanned items
	line      int     // 1+number of newlines seen
	startLine int     // start line of this item
}

// Lexer is an interface that represents a lexer. It has methods for getting the next token and the name of the lexer.
type Lexer interface {
	getNext() token
	getName() string
}

// New creates a new scanner for the input string.
// It takes a name and an input string, and returns a Lexer.
func New(name, input string) Lexer {
	lexer := &lexerState{
		name:      name,
		input:     input,
		items:     make([]token, 0),
		line:      1,
		startLine: 1,
	}
	lexer.run()
	return lexer
}

func (lexer *lexerState) getName() string {
	return lexer.name
}

// run runs the state machine for the lexer.
func (lexer *lexerState) run() {
	for state := lexStatementTokens(lexer); state != nil; {
		state = state(lexer)
	}
}

// next returns the next rune in the input.
func (lexer *lexerState) next() rune {
	if int(lexer.pos) >= len(lexer.input) {
		lexer.width = 0
		return endOfFile
	}
	r, width := utf8.DecodeRuneInString(lexer.input[lexer.pos:])
	lexer.width = Pos(width)
	lexer.pos += lexer.width
	if r == '\n' {
		lexer.line++
	}
	return r
}

func (lexer *lexerState) current() rune {
	if int(lexer.pos) >= len(lexer.input) {
		lexer.width = 0
		return endOfFile
	}
	r, _ := utf8.DecodeRuneInString(lexer.input[lexer.pos:])
	return r
}

// peek returns but does not consume the next rune in the input.
func (lexer *lexerState) peek() rune {
	r := lexer.next()
	lexer.backup()
	return r
}

// lookAhead returns the rune at the specified position ahead in the input, without consuming any runes.
func (lexer *lexerState) lookAhead(i int) rune {
	if int(lexer.pos) >= len(lexer.input) {
		lexer.width = 0
		return endOfFile
	}
	r, width := utf8.DecodeRuneInString(lexer.input[lexer.pos+Pos(i-1):])
	lexer.width = Pos(width)
	lexer.pos += lexer.width
	if r == '\n' {
		lexer.line++
	}
	lexer.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (lexer *lexerState) backup() {
	lexer.pos -= lexer.width
	// Correct newline count.
	if lexer.width == 1 && lexer.input[lexer.pos] == '\n' {
		lexer.line--
	}
}

// emit passes an item back to the client. It takes a token type as a parameter.
func (lexer *lexerState) emit(kind tokenKind) {
	lexer.items = append(lexer.items, token{kind, lexer.start, lexer.input[lexer.start:lexer.pos], lexer.startLine})
	lexer.start = lexer.pos
	lexer.startLine = lexer.line
}

// accept consumes the next rune if it's from the valid set of runes.
// It takes a string of valid runes as a parameter and returns a boolean indicating whether the next rune was accepted.
func (lexer *lexerState) accept(validRunes string) bool {
	if strings.ContainsRune(validRunes, lexer.next()) {
		return true
	}
	lexer.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set of runes.
// It takes a string of valid runes as a parameter.
func (lexer *lexerState) acceptRun(validRunes string) {
	for strings.ContainsRune(validRunes, lexer.next()) {
	}
	lexer.backup()
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating lexer.getNext.
// It takes a string format for the error message and a variadic number of arguments to format the error message.
func (lexer *lexerState) errorf(errorMessageFormat string, formatArgs ...interface{}) stateFunction {
	lexer.items = append(lexer.items, token{
		kind:       tokenKindError,
		position:   lexer.start,
		value:      fmt.Sprintf(errorMessageFormat, formatArgs...),
		lineNumber: lexer.startLine,
	})
	return nil
}

// getNext returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (lexer *lexerState) getNext() token {
	if len(lexer.items) == 0 {
		return token{}
	}
	var next token
	next, lexer.items = lexer.items[0], lexer.items[1:]
	return next
}

// scanRedefinesToken scans the input for the "REDEFINES" token.
// It returns true if the token is found and false otherwise.
func (lexer *lexerState) scanRedefinesToken() bool {
	lexer.acceptRun("REDEFINES")
	if !isSpace(lexer.peek()) {
		lexer.next()
		return false
	}
	return true
}

// scanOccursToken scans the input for the "OCCURS" token.
// It returns true if the token is found and false otherwise.
func (lexer *lexerState) scanOccursToken() (bool, error) {
	lexer.acceptRun("OCCURS")
	if !isSpace(lexer.peek()) {
		lexer.next()
		return false, nil
	}
	current := lexer.next()
	for isPICChar(current) || isSpace(current) {
		current = lexer.next()
	}
	if !lexer.atTerminator() {
		return false, fmt.Errorf("bad character %#U", current)
	}
	lexer.backup()
	return true, nil
}

// atPICTerminator checks if the current character is a PIC terminator.
// It returns true if the current character is a PIC terminator and false otherwise.
func (lexer *lexerState) atPICTerminator() bool {
	return lexer.peek() == picRight
}

// atTerminator checks if the current character is a valid termination character
// that can appear after an identifier.
// It returns true if the current character is a valid termination character and false otherwise.
func (lexer *lexerState) atTerminator() bool {
	currentChar := lexer.peek()
	return isSpace(currentChar) || isEOL(currentChar) || isTerminatorSymbol(currentChar)
}

// isTerminatorSymbol checks if the given character is a terminator symbol.
func isTerminatorSymbol(char rune) bool {
	switch char {
	case endOfFile, '.', ',', '|', ':', ')', '(':
		return true
	default:
		return false
	}
}

// atEnumTerminator checks if the current character is an enum terminator.
// It returns true if the current character is an enum terminator and false otherwise.
func (lexer *lexerState) atEnumTerminator() bool {
	return lexer.peek() == singleQuoteElement
}

// scanNumber scans the input for a number.
// It returns true if a number is found and false otherwise.
func (lexer *lexerState) scanNumber() bool {
	// optional leading signs
	lexer.accept("+-")
	validDigits := decimalDigits
	if lexer.accept("0") {
		// Note: Leading 0 does not mean octal in floats.
		switch {
		case lexer.accept("xX"):
			validDigits = hexDigits
		case lexer.accept("oO"):
			validDigits = octalDigits
		case lexer.accept("bB"):
			validDigits = binaryDigits
		}
	}

	lexer.acceptRun(validDigits)
	if lexer.accept(".") {
		lexer.acceptRun(validDigits)
	}
	if validDigits == decimalDigits && lexer.accept("eE") {
		lexer.accept("+-")
		lexer.acceptRun(decimalDigits)
	}
	if validDigits == hexDigits && lexer.accept("pP") {
		lexer.accept("+-")
		lexer.acceptRun(decimalDigits)
	}
	// Is it imaginary?
	lexer.accept("i")
	// Next thing mustn't be alphanumeric.
	if isAlphaNumeric(lexer.peek()) {
		lexer.next()
		return false
	}
	return true
}
