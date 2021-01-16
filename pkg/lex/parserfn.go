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
