package lex

import (
	"log"
	"reflect"

	"github.com/pgmitche/go-pic/cmd/pkg/decoder"
)

var (
	// num space num space text dot space num eol
	// 000160  05  DUMMY-GROUP-1.  00000115
	numDelimitedStruct = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// space num space text dot space eol
	//  05  DUMMY-GROUP-1.
	nonNumDelimitedStruct = []itemType{itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// num space num space text space pic space num eol
	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	pic = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefines = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}
)

type parser func(t *Tree, l line, root *record) *record

func unimplementedParser(t *Tree, l line, root *record) *record {
	panic("unimplemented")
}

func parsePIC(_ *Tree, l line, _ *record) *record {
	len, err := decoder.ParsePICCount(l.items[6].val)
	if err != nil {
		log.Fatalln(err)
	}

	return &record{
		Name:   l.items[4].val,
		Length: len,
		Typ:    decoder.ParsePICType(l.items[6].val),
	}
}

func parseRedefines(_ *Tree, l line, root *record) *record {
	len, err := decoder.ParsePICCount(l.items[10].val)
	if err != nil {
		log.Fatalln(err)
	}

	r := &record{
		Name:   l.items[4].val,
		Length: len,
		Typ:    decoder.ParsePICType(l.items[10].val),
	}

	return root.redefine(l.items[8].val, r)
}

func parseNumDelimitedStruct(t *Tree, l line, root *record) *record {
	return parseStruct(t, l, root, 4, 2)
}

func parseNonNumDelimitedStruct(t *Tree, l line, root *record) *record {
	return parseStruct(t, l, root, 3, 1)
}

func parseStruct(t *Tree, l line, root *record, nameIdx, groupIdx int) *record {
	r := &record{
		Name: l.items[nameIdx].val,
		Typ:  reflect.Struct,
	}

	// if the parent object already contains this depth
	// root
	//  | - 05 Group1
	//  | - 05 Group2
	parent, seenGroup := root.depthMap[l.items[groupIdx].val]
	if seenGroup {
		// then the root is the target for continued addition of children
		parent.Children = append(parent.Children, r)
		t.parseLines(r)
		return parent
	}

	// else the newStruct is the target for continued addition of children
	root.depthMap[l.items[groupIdx].val] = root
	r.depthMap = root.depthMap
	root.Children = append(root.Children, r)
	t.parseLines(r)
	return r
}
