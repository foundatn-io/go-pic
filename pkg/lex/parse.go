package lex

import (
	"io"
	"log"
	"reflect"
	"sync"
)

type Tree struct {
	lex   *lexer
	token item
	lines []line
	state *record
	line  line
	lIdx  int
}

type record struct {
	Children []*record
	Name     string
	Length   int
	Occurs   int
	Typ      reflect.Kind
	depthMap map[string]*record
	cache    sync.Map
}

func NewTree(lxr *lexer) *Tree {
	root := &record{Typ: reflect.Struct, Name: "root", depthMap: make(map[string]*record)}
	return &Tree{
		lex:   lxr,
		state: root,
		lIdx:  -1,
	}
}

func (t *Tree) Parse() *record {
	for {
		li := t.scanLine()
		log.Printf("building line for %d", t.token.line)
		if t.token.typ == itemEOF {
			break
		}

		l := buildLine(t.scanLine, li)
		if l == nil {
			break
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

		log.Println(t.token.line, t.token.typ)
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
func (t *Tree) parseLines(root *record) {
	for {
		if t.nextLine() == io.EOF {
			break
		}
		switch t.line.typ {
		case lineStruct:
			t.line.fn(t, t.line, root)
		default:
			idx := len(root.Children)
			root.Children = append(root.Children,
				root.toCache(t.line.fn(t, t.line, root), idx))
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

// toCache returns a record, just stored into or previously loaded from the cache
func (r *record) toCache(child *record, idx int) *record {
	r.cache.Store(r.Name, idx)
	return child
}

// fromCache loads a record, by name, from the cache if present
func (r *record) fromCache(name string) (*record, int) {
	idx, ok := r.cache.Load(name)
	if !ok {
		return nil, 0
	}

	i, ok := idx.(int)
	if !ok {
		log.Fatalln("failed to cast cache return value to integer")
	}

	return r.Children[i], i
}

func (r *record) redefine(target string, src *record) *record {
	dst, i := r.fromCache(target)
	if dst == nil {
		log.Fatalln("redefinition target does not exist")
	}

	r.cache.Delete(dst.Name)
	dst.Name = src.Name
	dst.Length = src.Length
	dst.Typ = src.Typ
	r.toCache(dst, i)
	return dst
}
