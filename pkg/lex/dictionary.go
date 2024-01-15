package lex

import (
	"log"
)

type word []tokenType

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
	occursPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenPIC, tokenSpace, tokenOCCURS, tokenDot, tokenSpace, tokenNumber}
	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccursPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenPIC, tokenSpace, tokenNumber}
	// 001300      OCCURS 12.                            00000242
	multiOccursPartPattern = word{tokenNumber, tokenSpace, tokenOCCURS, tokenDot, tokenSpace, tokenNumber}
	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPartPattern = word{tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenPIC, tokenDot, tokenSpace, tokenNumber}
	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefinesPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenREDEFINES, tokenSpace, tokenNumber}
	// 000830  05  DUMMY-GROUP-3  REDEFINES   DUMMY-GROUP-2.  00000195
	groupRedefinesPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenREDEFINES, tokenSpace, tokenIdentifier, tokenDot, tokenSpace, tokenNumber}
	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefinesPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenREDEFINES, tokenSpace, tokenIdentifier, tokenSpace, tokenPIC, tokenDot, tokenSpace, tokenNumber}
	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	picPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenPIC, tokenDot, tokenSpace, tokenNumber}
	// 05  DUMMY-GROUP-1.
	nonNumDelimitedStructPattern = word{tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenDot, tokenSpace}
	// 000160  05  DUMMY-GROUP-1.  00000115
	numDelimitedStructPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenDot, tokenSpace, tokenNumber}
	// 000600   88   EXAMPLE-ENUM VALUE   'N'. 00000600
	numDelimitedEnumPattern = word{tokenNumber, tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenIdentifier, tokenSpace, tokenEnum, tokenDot, tokenSpace, tokenNumber}
	// 88   EXAMPLE-ENUM-VALUE   'N'.
	nonNumDelimitedEnumPattern = word{tokenSpace, tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenIdentifier, tokenSpace, tokenEnum, tokenDot, tokenSpace}

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
func init() { // nolint:gochecknoinits
	for _, entry := range dictionary {
		parsers.Insert(entry.wordPattern, entry.parseFunc, entry.lineType)
	}
	log.Println("trie preloaded")
}

// getWord constructs a word from a list of items
func getWord(itemList []token) word {
	wordPattern := make(word, len(itemList))
	for i, item := range itemList {
		wordPattern[i] = item.typ
	}
	return wordPattern
}

// basicParserGet searches for a parser in the parsers trie using a word constructed from the given items
func basicParserGet(items []token) (lineParser, []token, bool) { // nolint:unparam // param is used...
	foundEntry := parsers.Search(getWord(items))
	if foundEntry == nil {
		return nil, nil, false
	}
	return foundEntry.parseFunc, items, true
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
