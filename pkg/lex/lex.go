package lex

import (
	"fmt"
	"strings"
	"unicode/utf8"
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
		return eof
	}
	r, width := utf8.DecodeRuneInString(lexer.input[lexer.pos:])
	lexer.width = Pos(width)
	lexer.pos += lexer.width
	if r == '\n' {
		lexer.line++
	}
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
		return eof
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
func (lexer *lexerState) emit(tokenType tokenType) {
	lexer.items = append(lexer.items, token{tokenType, lexer.start, lexer.input[lexer.start:lexer.pos], lexer.startLine})
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
		typ:  tokenError,
		pos:  lexer.start,
		val:  fmt.Sprintf(errorMessageFormat, formatArgs...),
		line: lexer.startLine})
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
