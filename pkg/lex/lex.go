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
	name      string // the name of the input; used only for error reports
	input     string // the string being scanned
	pos       Pos    // current position in the input
	start     Pos    // start position of this item
	width     Pos    // width of last rune read from input
	items     []item // channel of scanned items
	line      int    // 1+number of newlines seen
	startLine int    // start line of this item
}

type Lexer interface {
	nextItem() item
	getName() string
}

// New creates a new scanner for the input string.
func New(name, input string) Lexer {
	log.Println("building new lexer")
	l := &lexer{
		name:      name,
		input:     input,
		items:     make([]item, 0),
		line:      1,
		startLine: 1,
	}
	l.run()
	return l
}

func (l *lexer) getName() string {
	return l.name
}

// run runs the state machine for the lexer.
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
		l.line++
	}

	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// bit of a hack tbh
func (l *lexer) lookAhead(i int) rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}

	r, w := utf8.DecodeRuneInString(l.input[l.pos+Pos(i-1):])
	l.width = Pos(w)
	l.pos += l.width

	if r == '\n' {
		l.line++
	}

	l.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width

	// Correct newline count.
	if l.width == 1 && l.input[l.pos] == '\n' {
		l.line--
	}
}

// emit passes an item back to the client.
func (l *lexer) emit(t itemType) {
	l.items = append(l.items, item{t, l.start, l.input[l.start:l.pos], l.startLine})
	l.start = l.pos
	l.startLine = l.line
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}

	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}

	l.backup()
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items = append(l.items, item{
		typ:  itemError,
		pos:  l.start,
		val:  fmt.Sprintf(format, args...),
		line: l.startLine})

	return nil
}

// nextItem returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) nextItem() item {
	var next item
	if len(l.items) == 0 {
		log.Println("no lexed items left in stack")
		return item{}
	}

	next, l.items = l.items[0], l.items[1:]
	return next
}
