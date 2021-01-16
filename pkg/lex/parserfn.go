package lex

import (
	"log"
	"reflect"
	"strings"

	"github.com/pgmitche/go-pic/cmd/pkg/decoder"
)

var (
	picPrefix = "PIC "
)

type parser func(t *Tree, l line, root *record) *record

func unimplementedParser(t *Tree, l line, root *record) *record {
	panic("unimplemented")
}

// parsePIC is a parserfn that is used to build records
// for lines that are determined to be PIC definitions
func parsePIC(_ *Tree, l line, _ *record) *record {
	picNumDef := strings.TrimPrefix(l.items[6].val, picPrefix)
	len, err := decoder.ParsePICCount(picNumDef)
	if err != nil {
		log.Fatalln(err)
	}

	return &record{
		Name:   l.items[4].val,
		Length: len,
		Typ:    decoder.ParsePICType(l.items[6].val),
	}
}

// parseRedefines is a parserfn that is used to build records
// for lines that are determined to be REDEFINES definitions
//
// It will build the new record and replace the the redefinition
// target
func parseRedefines(_ *Tree, l line, root *record) *record {
	picNumDef := strings.TrimPrefix(l.items[10].val, picPrefix)
	len, err := decoder.ParsePICCount(picNumDef)
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

// TODO: (pgmitche) parse occurs lines
func parseOccurs(_ *Tree, l line, root *record) *record {
	return unimplementedParser(nil, l, root)
}

// parseNumDelimitedStruct is a parserfn wrapper used to build records
// for lines that are determined to be new group definitions that are built with
// number delimiter tokens at the start and end of the source line
//
// It will call parseStruct to handle logic for new groups
func parseNumDelimitedStruct(t *Tree, l line, root *record) *record {
	return parseStruct(t, l, root, 4, 2)
}

// parseNonNumDelimitedStruct is a parserfn wrapper used to build records
// for lines that are determined to be new group definitions that are built
// without number delimiter tokens at the start and end of the source line
//
// It will call parseStruct to handle logic for new groups
func parseNonNumDelimitedStruct(t *Tree, l line, root *record) *record {
	return parseStruct(t, l, root, 3, 1)
}

// parseStruct will build a new record of type struct, store itself under the
// parent record as a child. It will also add an entry to the parent struct
// indicating a new group.
//
// new groups are determined by a number token preceding an identifier token
// if this number has been seen before, then this struct is not a new group, but
// a sibling of other structs/pics in the root record
// if this number is a new unseen value, parsing will continue for each line
// but each new line will treat this struct as it's root.
//
// In this way we may build a tree like
// root
//  |-group1
//  |   |-picA
//  |-group2
//  |	|-picA
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
