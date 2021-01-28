package lex

import (
	"errors"
	"io"
	"log"
	"reflect"
)

type Tree struct {
	lex   Lexer
	token item
	lines []line
	state *Record
	line  line
	lIdx  int
}

func NewTree(lxr Lexer) *Tree {
	log.Println("building new tree")
	root := &Record{Typ: reflect.Struct, Name: lxr.getName(), depthMap: make(map[string]*Record)}
	return &Tree{
		lex:   lxr,
		state: root,
		lIdx:  -1,
	}
}

func (t *Tree) Parse() *Record {
	log.Println("parsing lexer tokens")
	for {
		li := t.scanLine()
		if t.token.typ == itemEOF {
			log.Println("reached EOF token, input lexed.")
			break
		}

		l := buildLine(t.scanLine, li)
		if l == nil {
			continue
		}

		t.lines = append(t.lines, *l)
	}

	t.parseLines(t.state)
	return t.state
}

func (t *Tree) scanLine() []item {
	var lineItems []item
	for {
		t.token = t.next()
		if t.token == (item{}) || t.token.typ == itemError {
			break
		}

		if t.token.typ == itemEOL || itemEOF == t.token.typ {
			break
		}

		lineItems = append(lineItems, t.token)
	}

	return lineItems
}

// next returns the next token.
func (t *Tree) next() item {
	return t.lex.nextItem()
}

// parseLines generates the text for the line
// and adds it to the tree data
func (t *Tree) parseLines(root *Record) {
	for {
		if errors.Is(t.nextLine(), io.EOF) {
			break
		}

		switch t.line.typ {
		case lineJunk:
			continue

		case lineStruct, lineRedefines, lineGroupRedefines, lineMultilineRedefines:
			t.line.fn(t, t.line, root)

		default:
			rec := t.line.fn(t, t.line, root)
			if rec == nil {
				log.Fatalf("parser returned nil record for line: %+v", t.line)
			}

			parent, ok := root.depthMap[rec.depth]
			if ok {
				root = parent
			}

			idx := len(root.Children)
			l := rec.Length
			if rec.Occurs > 0 {
				l *= rec.Occurs
			}

			root.Length += l
			root.Children = append(root.Children, root.toCache(rec, idx))
		}
	}
}

func (t *Tree) nextLine() error {
	if t.lIdx == len(t.lines)-1 {
		return io.EOF
	}

	t.lIdx++
	t.line = t.lines[t.lIdx]
	return nil
}
