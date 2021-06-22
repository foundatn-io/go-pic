package lex

import (
	"log"
)

type word []itemType

type entry struct {
	w   word
	fn  parser
	typ lineType
}

const (
	recordDescriptionIndicator = "01"
)

var (
	parsers = NewTrie()

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12. 00000241
	occursWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemOCCURS, itemDot, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccursWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001300      OCCURS 12.                            00000242
	multiOccursPartWord = word{itemNumber, itemSpace, itemOCCURS, itemDot, itemSpace, itemNumber}

	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPartWord = word{itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefinesWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemNumber}

	// 000830  05  DUMMY-GROUP-3  REDEFINES   DUMMY-GROUP-2.  00000195
	groupRedefinesWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefinesWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	picWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	//  05  DUMMY-GROUP-1.
	nonNumDelimitedStructWord = word{itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace}

	// 000160  05  DUMMY-GROUP-1.  00000115
	numDelimitedStructWord = word{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	dictionary = map[string]entry{
		"numDelimitedStruct": {
			typ: lineStruct,
			fn:  parseNumDelimitedStruct,
			w:   numDelimitedStructWord},

		"nonNumDelimitedStruct": {
			typ: lineStruct,
			fn:  parseNonNumDelimitedStruct,
			w:   nonNumDelimitedStructWord},

		"pic": {
			typ: linePIC,
			fn:  parsePIC,
			w:   picWord},

		"redefines": {
			typ: lineRedefines,
			fn:  parseRedefines,
			w:   redefinesWord},

		"groupRedefines": {
			typ: lineGroupRedefines,
			fn:  parseGroupRedefines,
			w:   groupRedefinesWord},

		"multiRedefines": {
			typ: lineMultilineRedefines,
			fn:  parseRedefinesMulti,
			w:   multiRedefinesWord},

		"multiRedefinesPart": {
			typ: lineMultilineRedefines,
			fn:  parseRedefinesMulti,
			w:   multiRedefinesPartWord},

		"occurs": {
			typ: lineOccurs,
			fn:  parseOccurs,
			w:   occursWord},

		"multiOccurs": {
			typ: lineMultilineOccurs,
			fn:  parseOccursMulti,
			w:   multiOccursWord},

		"multiOccursPart": {
			typ: lineMultilineOccurs,
			fn:  parseOccursMulti,
			w:   multiOccursPartWord},
	}
)

func init() { // nolint:gochecknoinits
	for _, v := range dictionary {
		parsers.Insert(v.w, v.fn, v.typ)
	}

	log.Println("trie preloaded")
}

// getWord constructs a word for a slice of items
func getWord(items []item) word {
	w := make(word, len(items))
	for i, l := range items {
		w[i] = l.typ
	}

	return w
}

func basicParserGet(items []item) (parser, []item, bool) { // nolint:unparam // param is used...
	d := parsers.Search(getWord(items))
	if d == nil {
		return nil, nil, false
	}

	return d.fn, items, true
}

// equalWord tells whether a and b contain the same elements.
// A nil argument is equivalent to an empty slice.
func equalWord(a, b word) bool {
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
