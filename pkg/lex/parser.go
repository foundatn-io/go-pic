package lex

import (
	"io"
	"reflect"
)

var (
	readers = map[lineType]func(items []item) (parser, bool){
		lineJunk:               isJunk,
		lineStruct:             isStruct,
		linePIC:                isPic,
		lineRedefines:          isRedefinition,
		lineMultilineRedefines: isMultilineRedefinition,
		lineOccurs:             isOccurrence,
		lineMultilineOccurs:    isMultilineOccurrence,
	}
)

type Tree struct {
	lex          *lexer
	token        item
	lines        []line
	currentDepth int
	state        *record
	line         line
	lIdx         int
}

type line struct {
	items []item
	typ   lineType
	fn    parser
}

type record struct {
	Children []*record
	Name     string
	Length   int
	Occurs   int
	Typ      reflect.Kind
	depthMap map[string]*record
}

func New() *Tree {
	root := &record{Typ: reflect.Struct, Name: "root", depthMap: make(map[string]*record)}
	return &Tree{
		lex:   nil,
		lines: nil,
		state: root,
		lIdx:  -1,
	}
}

func (t *Tree) parse() {
	for {
		li := t.scanLine()
		t.lines = append(t.lines, buildLine(li))
		if t.token.typ == itemEOF {
			break
		}
	}
	t.parseLines(t.state)
}

func (t *Tree) scanLine() []item {
	var lineItems []item
	for {
		t.token = t.next()
		if t.token.typ != itemEOL && itemEOF != t.token.typ {
			lineItems = append(lineItems, t.token)
		} else {
			return lineItems
		}
	}
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
			root.Children = append(root.Children, t.line.fn(t, t.line, root))
		}
	}
	return
}

func (t *Tree) nextLine() error {
	if t.lIdx == len(t.lines)-1 {
		return io.EOF
	}
	t.lIdx++
	t.line = t.lines[t.lIdx]
	return nil
}

func buildLine(items []item) line {
	for typ, fingerPrinter := range readers {
		parser, ok := fingerPrinter(items)
		if ok {
			return line{
				items: items,
				typ:   typ,
				fn:    parser,
			}
		}
	}
	return line{}
}

// itemType identifies the type of lex items.
type lineType int

const (
	lineStruct             lineType = iota // is a new struct line
	linePIC                                // is a new PIC line
	lineJunk                               // is a line full of rubbish text
	lineRedefines                          // is a line containing a PIC redefinition
	lineMultilineRedefines                 // is a line containing a redefinition without a target
	lineOccurs                             // is a line containing a PIC occurrence
	lineMultilineOccurs                    // is a line containing an incomplete PIC occurrence
)

func isStruct(items []item) (parser, bool) {
	lineFingerprint := make([]itemType, len(items))
	for i, l := range items {
		lineFingerprint[i] = l.typ
	}

	if equalFingerprints(lineFingerprint, nonNumDelimitedStruct) {
		return parseNonNumDelimitedStruct, true
	}

	if equalFingerprints(lineFingerprint, numDelimitedStruct) {
		return parseNumDelimitedStruct, true
	}

	return nil, false
}

func isPic(items []item) (parser, bool) {
	lineFingerprint := make([]itemType, len(items))
	for i, l := range items {
		lineFingerprint[i] = l.typ
	}

	if !equalFingerprints(lineFingerprint, pic) {
		return nil, false
	}

	return parsePIC, false

}

func isJunk(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isRedefinition(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isMultilineRedefinition(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isOccurrence(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isMultilineOccurrence(items []item) (parser, bool) {

	return unimplementedParser, false
}

// equalFingerprints tells whether a and b contain the same elements.
// A nil argument is equivalent to an empty slice.
func equalFingerprints(a, b []itemType) bool {
	if len(a) != len(b) {
		return false
	}

	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}
