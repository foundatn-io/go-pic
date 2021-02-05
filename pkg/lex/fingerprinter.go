package lex

import (
	"log"
)

type fingerprint []itemType

type entry struct {
	fp  fingerprint
	fn  parser
	typ lineType
}

const (
	recordDescriptionIndicator = "01"
)

var (
	parsers = NewTrie()
	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12. 00000241
	occursFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemOCCURS, itemDot, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccursFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001300      OCCURS 12.                            00000242
	multiOccursPartFp = fingerprint{itemNumber, itemSpace, itemOCCURS, itemDot, itemSpace, itemNumber}

	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPartFp = fingerprint{itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefinesFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemNumber}

	// 000830  05  DUMMY-GROUP-3  REDEFINES   DUMMY-GROUP-2.  00000195
	groupRedefinesFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefinesFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	picFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber}

	//  05  DUMMY-GROUP-1.
	nonNumDelimitedStructFp = fingerprint{itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace}

	// 000160  05  DUMMY-GROUP-1.  00000115
	numDelimitedStructFp = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	fingerprints = map[string]entry{
		"numDelimitedStruct": {
			typ: lineStruct,
			fn:  parseNumDelimitedStruct,
			fp:  numDelimitedStructFp},

		"nonNumDelimitedStruct": {
			typ: lineStruct,
			fn:  parseNonNumDelimitedStruct,
			fp:  nonNumDelimitedStructFp},

		"pic": {
			typ: linePIC,
			fn:  parsePIC,
			fp:  picFp},

		"redefines": {
			typ: lineRedefines,
			fn:  parseRedefines,
			fp:  redefinesFp},

		"groupRedefines": {
			typ: lineGroupRedefines,
			fn:  parseGroupRedefines,
			fp:  groupRedefinesFp},

		"multiRedefines": {
			typ: lineMultilineRedefines,
			fn:  parseRedefinesMulti,
			fp:  multiRedefinesFp},

		"multiRedefinesPart": {
			typ: lineMultilineRedefines,
			fn:  parseRedefinesMulti,
			fp:  multiRedefinesPartFp},

		"occurs": {
			typ: lineOccurs,
			fn:  parseOccurs,
			fp:  occursFp},

		"multiOccurs": {
			typ: lineMultilineOccurs,
			fn:  parseOccursMulti,
			fp:  multiOccursFp},

		"multiOccursPart": {
			typ: lineMultilineOccurs,
			fn:  parseOccursMulti,
			fp:  multiOccursPartFp},
	}
)

func init() { // nolint:gochecknoinits
	for _, v := range fingerprints {
		parsers.Insert(v.fp, v.fn, v.typ)
	}

	log.Println("trie preloaded")
}

// getFingerprint constructs a fingerprint for a slice of items
func getFingerprint(items []item) fingerprint {
	fp := make(fingerprint, len(items))
	for i, l := range items {
		fp[i] = l.typ
	}

	return fp
}

func basicParserGet(items []item) (parser, []item, bool) { // nolint:unparam // param is used...
	d := parsers.Search(getFingerprint(items))
	if d == nil {
		return nil, nil, false
	}

	return d.fn, items, true
}

// equalFingerprints tells whether a and b contain the same elements.
// A nil argument is equivalent to an empty slice.
func equalFingerprints(a, b fingerprint) bool {
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
