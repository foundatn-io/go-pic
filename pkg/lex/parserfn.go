package lex

import (
	"fmt"
	"log"
	"reflect"
	"strconv"
	"strings"
)

var (
	picPrefix = "PIC "
)

type parser func(t *Tree, l line, root *Record) *Record

func unimplementedParser(_ *Tree, _ line, _ *Record) *Record {
	panic("unimplemented")
}

// parsePIC is a parserfn that is used to build records
// for lines that are determined to be PIC definitions
func parsePIC(_ *Tree, l line, _ *Record) *Record {
	picNumDef := strings.TrimPrefix(l.items[6].val, picPrefix)
	len, err := parsePICCount(picNumDef)
	if err != nil {
		log.Fatalln(err)
	}

	return &Record{
		Name:   l.items[4].val,
		Length: len,
		depth:  l.items[2].val,
		Typ:    parsePICType(picNumDef),
	}
}

// parseRedefines is a parserfn that is used to build records
// for lines that are determined to be REDEFINES definitions
//
// It will build the new Record and replace the the redefinition
// target
func parseRedefines(_ *Tree, l line, root *Record) *Record {
	picNumDef := strings.TrimPrefix(l.items[10].val, picPrefix)
	len, err := parsePICCount(picNumDef)
	if err != nil {
		log.Fatalln(err)
	}

	r := &Record{
		Name:   l.items[4].val,
		Length: len,
		Typ:    parsePICType(picNumDef),
	}

	return root.redefine(l.items[8].val, r)
}

func parseOccurs(_ *Tree, l line, root *Record) *Record {
	picNumDef := strings.TrimPrefix(strings.TrimSpace(l.items[6].val), picPrefix)
	len, err := parsePICCount(picNumDef)
	if err != nil {
		log.Fatalln(err)
	}

	n, err := parseOccursCount(l.items[8])
	if err != nil {
		log.Fatalln(err)
	}

	return &Record{
		Name:   l.items[4].val,
		Length: len,
		Occurs: n,
		depth:  l.items[2].val,
		Typ:    parsePICType(picNumDef),
	}
}

// parseNumDelimitedStruct is a parserfn wrapper used to build records
// for lines that are determined to be new group definitions that are built with
// number delimiter tokens at the start and end of the source line
//
// It will call parseStruct to handle logic for new groups
func parseNumDelimitedStruct(t *Tree, l line, root *Record) *Record {
	return parseStruct(t, l, root, 4, 2)
}

// parseNonNumDelimitedStruct is a parserfn wrapper used to build records
// for lines that are determined to be new group definitions that are built
// without number delimiter tokens at the start and end of the source line
//
// It will call parseStruct to handle logic for new groups
func parseNonNumDelimitedStruct(t *Tree, l line, root *Record) *Record {
	return parseStruct(t, l, root, 3, 1)
}

// parseStruct will build a new Record of type struct, store itself under the
// parent Record as a child. It will also add an entry to the parent struct
// indicating a new group.
//
// new groups are determined by a number token preceding an identifier token
// if this number has been seen before, then this struct is not a new group, but
// a sibling of other structs/pics in the root Record
// if this number is a new unseen value, parsing will continue for each line
// but each new line will treat this struct as it's root.
//
// In this way we may build a tree like
// root
//  |-group1
//  |   |-picA
//  |-group2
//  |	|-picA
func parseStruct(t *Tree, l line, root *Record, nameIdx, groupIdx int) *Record {
	r := &Record{
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

func parseOccursCount(i item) (int, error) {
	s := strings.TrimPrefix(i.val, "OCCURS ")
	n := strings.TrimSuffix(s, ".")
	return strconv.Atoi(n)
}

type picType int

const (
	unknown picType = iota
	unsigned
	signed
	decimal
	alpha

	alphaIndicators     = "XA"
	decimalIndicators   = ".VP"
	signedIntIndicators = "S"
	intIndicators       = "9"
)

var (
	types = map[picType]reflect.Kind{
		unknown:  reflect.Invalid,
		unsigned: reflect.Uint,
		signed:   reflect.Int,
		decimal:  reflect.Float64,
		alpha:    reflect.String,
	}
)

// ParsePICType identifies an equivalent Go type from the given substring
// that contains a PIC definition
func parsePICType(s string) reflect.Kind {
	picType := unknown
	s = strings.TrimRight(s, ".")
	if strings.ContainsAny(s, alphaIndicators) {
		if alpha > picType {
			picType = alpha
			return types[picType]
		}
	}

	if strings.ContainsAny(s, decimalIndicators) {
		if decimal > picType {
			picType = decimal
			return types[picType]
		}
	}

	if strings.ContainsAny(s, signedIntIndicators) {
		if signed > picType {
			picType = signed
			return types[picType]
		}
	}

	if strings.ContainsAny(s, intIndicators) {
		picType = unsigned
		return types[picType]
	}

	return types[picType]
}

// ParsePICCount identifies the fixed width, or length, of the given
// PIC definition such as: X(2)., XX., 9(9)., etc.
func parsePICCount(s string) (int, error) {
	// prepare a slice of runes, representing the string
	s = strings.TrimRight(s, ".")
	c := []rune(s)

	size := 0

	// S9(9)V9(9)
	// SV = 2 + 18 = 20
	for strings.Contains(s, "(") {
		left := strings.Index(s, "(")
		right := strings.Index(s, ")")
		// capture type when using parentheses "9(99)" should be stripped to
		// "" so that it results in 99+0, not 99+len("9")
		start := left - 1
		end := right + 1
		amount, err := strconv.Atoi(s[left+1 : right])
		if err != nil {
			return 0, err
		}

		size += amount
		c = append(c[:start], c[end:]...)
		s = string(c)
	}

	return size + len(c), nil
}
