package lex

import (
	"log"
)

type word []tokenKind

type entry struct {
	wordPattern word
	parseFunc   lineParser
	lineType    lineType
}

const (
	recordDescriptionIndicator = "01"
)

var (
	parsers = NewTrie()

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12. 00000241
	occursPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindPIC, tokenKindSpace, tokenKindOCCURS, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccursPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindPIC, tokenKindSpace, tokenKindNumber}
	// 001300      OCCURS 12.                            00000242
	multiOccursPartPattern = word{tokenKindNumber, tokenKindSpace, tokenKindOCCURS, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPartPattern = word{tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindPIC, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefinesPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindREDEFINES, tokenKindSpace, tokenKindNumber}
	// 000830  05  DUMMY-GROUP-3  REDEFINES   DUMMY-GROUP-2.  00000195
	groupRedefinesPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindREDEFINES, tokenKindSpace, tokenKindIdentifier, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefinesPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindREDEFINES, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindPIC, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	picPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindPIC, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 05  DUMMY-GROUP-1.
	nonNumDelimitedStructPattern = word{tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindDot, tokenKindSpace}
	// 000160  05  DUMMY-GROUP-1.  00000115
	numDelimitedStructPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 000600   88   EXAMPLE-ENUM VALUE   'N'. 00000600
	numDelimitedEnumPattern = word{tokenKindNumber, tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindEnum, tokenKindDot, tokenKindSpace, tokenKindNumber}
	// 88   EXAMPLE-ENUM-VALUE   'N'.
	nonNumDelimitedEnumPattern = word{tokenKindSpace, tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindEnum, tokenKindDot, tokenKindSpace}

	dictionary = map[string]entry{
		"numDelimitedStruct": {
			lineType:    lineStruct,
			parseFunc:   parseNumDelimitedStruct,
			wordPattern: numDelimitedStructPattern},

		"nonNumDelimitedStruct": {
			lineType:    lineStruct,
			parseFunc:   parseNonNumDelimitedStruct,
			wordPattern: nonNumDelimitedStructPattern},

		"numDelimitedEnum": {
			lineType:    lineEnum,
			parseFunc:   noop,
			wordPattern: numDelimitedEnumPattern},

		"nonNumDelimitedEnum": {
			lineType:    lineEnum,
			parseFunc:   noop,
			wordPattern: nonNumDelimitedEnumPattern},

		"pic": {
			lineType:    linePIC,
			parseFunc:   parsePIC,
			wordPattern: picPattern},

		"redefines": {
			lineType:    lineRedefines,
			parseFunc:   parseRedefinitions,
			wordPattern: redefinesPattern},

		"groupRedefines": {
			lineType:    lineGroupRedefines,
			parseFunc:   parseGroupRedefinitions,
			wordPattern: groupRedefinesPattern},

		"multiRedefines": {
			lineType:    lineMultilineRedefines,
			parseFunc:   parseRedefinesMulti,
			wordPattern: multiRedefinesPattern},

		"multiRedefinesPart": {
			lineType:    lineMultilineRedefines,
			parseFunc:   parseRedefinesMulti,
			wordPattern: multiRedefinesPartPattern},

		"occurs": {
			lineType:    lineOccurs,
			parseFunc:   parseOccurs,
			wordPattern: occursPattern},

		"multiOccurs": {
			lineType:    lineMultilineOccurs,
			parseFunc:   parseOccursMulti,
			wordPattern: multiOccursPattern},

		"multiOccursPart": {
			lineType:    lineMultilineOccurs,
			parseFunc:   parseOccursMulti,
			wordPattern: multiOccursPartPattern},
	}
)

// init preloads the trie with entries from the dictionary
func init() { //nolint:gochecknoinits
	for _, entry := range dictionary {
		parsers.Insert(entry.wordPattern, entry.parseFunc, entry.lineType)
	}
	log.Println("trie preloaded")
}

// getWord constructs a word from a list of items
func getWord(tokens []token) word {
	wordPattern := make(word, len(tokens))
	for i, token := range tokens {
		wordPattern[i] = token.kind
	}
	return wordPattern
}

// basicParserGet searches for a parser in the parsers trie using a word constructed from the given items
func basicParserGet(tokens []token) (lineParser, []token, bool) { //nolint:unparam // param is used...
	foundEntry := parsers.Search(getWord(tokens))
	if foundEntry == nil {
		return nil, nil, false
	}
	return foundEntry.parseFunc, tokens, true
}

// equalWord checks if two words are deeply equal
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
