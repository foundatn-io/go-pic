package lex

import (
	"errors"
	"io"
	"log"
	"reflect"
)

type Tree struct {
	lexr      Lexer
	token     token
	lines     []line
	state     *Record
	line      line
	lineIndex int
}

func NewTree(lexr Lexer) *Tree {
	log.Println("building new tree")
	root := &Record{Typ: reflect.Struct, Name: lexr.getName(), depthMap: make(map[string]*Record)}
	return &Tree{
		lexr:      lexr,
		state:     root,
		lineIndex: -1,
	}
}

func (t *Tree) Parse() *Record {
	log.Println("parsing lexer token")
	for {
		li := t.scanLine()
		if t.token.typ == itemEOF {
			log.Println("reached EOF token, input lexed.")
			break
		}

		l := buildLine(li)

		if l == nil {
			continue
		}

		t.lines = append(t.lines, *l)
	}

	t.parseLines(t.state)
	return t.state
}

func (t *Tree) scanLine() []token {
	var lineItems []token
	for {
		t.token = t.next()
		if t.token == (token{}) || t.token.typ == itemError {
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
func (t *Tree) next() token {
	return t.lexr.getNext()
}

// parseLines generates the text for the line
// and adds it to the tree data
func (t *Tree) parseLines(root *Record) { //nolint:gocyclo // TODO: refine
	for {
		if errors.Is(t.nextLine(), io.EOF) {
			break
		}

		switch t.line.typ {
		case lineJunk, lineEnum:
			log.Printf("%s on copybook line %d resulted in no-op", t.line.typ, t.lineIndex)
			continue

		case lineRedefines, lineMultilineRedefines, lineGroupRedefines:
			t.line.fn(t, t.line, root)

		case lineStruct:
			rec := t.line.fn(t, t.line, root)
			if rec == nil {
				continue
			}

			parent, ok := root.depthMap[rec.depth]
			if ok {
				root = parent
			}

			delve(t, root, rec)

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
	if t.lineIndex == len(t.lines)-1 {
		return io.EOF
	}

	t.lineIndex++
	t.line = t.lines[t.lineIndex]
	return nil
}
