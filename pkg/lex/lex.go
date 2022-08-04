package lex

import (
	"fmt"
	"log"
	"strings"
	"unicode/utf8"
)

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	name        string  // the name of the input; used only for error reports
	input       string  // the string being scanned
	pos         Pos     // current position in the input
	start       Pos     // start position of this token
	width       Pos     // width of last rune read from input
	tokens      []token // list of scanned tokens
	currentLine int     // 1+number of newlines seen
	startLine   int     // starting line of this token
}

type Lexer interface {
	getNext() token
	getName() string
}

// New creates a new scanner for the input string.
func New(name, input string) Lexer {
	log.Println("building new lexer")
	l := &lexer{
		name:        name,
		input:       input,
		tokens:      make([]token, 0),
		currentLine: 1,
		startLine:   1,
	}
	l.run()
	return l
}

func (l *lexer) getName() string {
	return l.name
}

// run begins lexing
func (l *lexer) run() {
	for state := lexInsideStatement(l); state != nil; {
		state = state(l)
	}
}

// next returns the next rune in the input.
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = Pos(w)
	l.pos += l.width

	if r == '\n' {
		l.currentLine++
	}

	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// lookAhead ...
//
// FIXME: a bit hacky, revisit to improve
func (l *lexer) lookAhead(i int) rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}

	r, w := utf8.DecodeRuneInString(l.input[l.pos+Pos(i-1):])
	l.width = Pos(w)
	l.pos += l.width

	if r == '\n' {
		l.currentLine++
	}

	l.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width

	// Correct newline count.
	if l.width == 1 && l.input[l.pos] == '\n' {
		l.currentLine--
	}
}

// emit passes a token adds a new token of the provided type t, with the current
// position information to the recorded tokens
func (l *lexer) emit(t itemType) {
	l.tokens = append(l.tokens, token{t, l.start, l.input[l.start:l.pos], l.startLine})
	l.start = l.pos
	l.startLine = l.currentLine
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}

	l.backup()
	return false
}

// acceptSequence consumes a sequence of runes from the valid set.
func (l *lexer) acceptSequence(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}

	l.backup()
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.getNext.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens = append(l.tokens, token{
		typ:  itemError,
		pos:  l.start,
		val:  fmt.Sprintf(format, args...),
		line: l.startLine})

	return nil
}

// getNext returns the next token from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) getNext() token {
	var next token
	if len(l.tokens) == 0 {
		log.Println("no lexed tokens left in stack")
		return token{}
	}

	next, l.tokens = l.tokens[0], l.tokens[1:]
	return next
}
