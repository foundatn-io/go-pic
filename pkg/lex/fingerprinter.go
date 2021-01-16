package lex

var (
	readers = map[lineType]func(nx func() []item, items []item) (parser, []item, bool){
		lineJunk:               isJunk,
		lineStruct:             isStruct,
		linePIC:                isPic,
		lineRedefines:          isRedefinition,
		lineMultilineRedefines: isMultilineRedefinition,
		lineOccurs:             isOccurrence,
		lineMultilineOccurs:    isMultilineOccurrence,
	}

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

	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefines = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemNumber}

	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPart = []itemType{itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12 00000241
	occurs = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemOCCURS, itemSpace, itemNumber, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccurs = []itemType{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001300      OCCURS 12.                            00000242
	multiOccursPart = []itemType{itemNumber, itemSpace, itemOCCURS, itemSpace, itemNumber, itemDot, itemSpace, itemNumber}
)

func isStruct(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if equalFingerprints(fp, nonNumDelimitedStruct) {
		return parseNonNumDelimitedStruct, items, true
	}

	if equalFingerprints(fp, numDelimitedStruct) {
		return parseNumDelimitedStruct, items, true
	}

	return nil, nil, false
}

func isPic(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if !equalFingerprints(fp, pic) {
		return nil, nil, false
	}

	return parsePIC, items, true
}

func isJunk(_ func() []item, items []item) (parser, []item, bool) {

	return unimplementedParser, nil, false
}

func isRedefinition(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if !equalFingerprints(fp, redefines) {
		return nil, nil, false
	}

	return parseRedefines, items, true
}

func isMultilineRedefinition(nx func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if !equalFingerprints(fp, multiRedefines) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerPrint(part)
	if !equalFingerprints(fp2, multiRedefinesPart) {
		return nil, nil, false
	}

	return parseRedefines, lineFromMultiRedefines(items, part), true
}

func isOccurrence(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if !equalFingerprints(fp, occurs) {
		return nil, nil, false
	}

	return parseOccurs, items, true
}

func isMultilineOccurrence(nx func() []item, items []item) (parser, []item, bool) {
	fp := getFingerPrint(items)
	if !equalFingerprints(fp, multiOccurs) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerPrint(part)
	if !equalFingerprints(fp2, multiOccursPart) {
		return nil, nil, false
	}

	return parseOccurs, lineFromMultiOccurs(items, part), true
}

func getFingerPrint(items []item) []itemType {
	fingerprint := make([]itemType, len(items))
	for i, l := range items {
		fingerprint[i] = l.typ
	}

	return fingerprint
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
