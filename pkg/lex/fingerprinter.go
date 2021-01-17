package lex

type fingerprint []itemType

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
	numDelimitedStruct = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// space num space text dot space eol
	//  05  DUMMY-GROUP-1.
	nonNumDelimitedStruct = fingerprint{itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber}

	// num space num space text space pic space num eol
	// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
	pic = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
	redefines = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
	multiRedefines = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemNumber}

	// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
	multiRedefinesPart = fingerprint{itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12. 00000241
	occurs = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemOCCURS, itemSpace, itemNumber}

	// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
	multiOccurs = fingerprint{itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber}

	// 001300      OCCURS 12.                            00000242
	multiOccursPart = fingerprint{itemNumber, itemSpace, itemOCCURS, itemSpace, itemNumber, itemDot, itemSpace, itemNumber}
)

func isStruct(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerprint(items)
	if equalFingerprints(fp, nonNumDelimitedStruct) {
		return parseNonNumDelimitedStruct, items, true
	}

	if equalFingerprints(fp, numDelimitedStruct) {
		return parseNumDelimitedStruct, items, true
	}

	return nil, nil, false
}

func isPic(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), pic) {
		return nil, nil, false
	}

	return parsePIC, items, true
}

func isJunk(_ func() []item, items []item) (parser, []item, bool) {
	return unimplementedParser, nil, false
}

func isRedefinition(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), redefines) {
		return nil, nil, false
	}

	return parseRedefines, items, true
}

// isMultilineRedefinition is a fingerprinting function that validates whether a
// line is an indicator for, and sibling of a multi-line redefinition
//
// it first checks the fingerprint of the line against the first fingerprint of
// a multi-line REDEFINES definition
// then uses nx() to get the next line, and validate that against the second
// fingerprint of a multi-line REDEFINES definition
//
// when this is successful, the parseRedefines parserfn is returned, along with
// a new, single, line object built from the two fingerprinted line objects.
func isMultilineRedefinition(nx func() []item, items []item) (parser, []item, bool) {
	fp := getFingerprint(items)
	if !equalFingerprints(fp, multiRedefines) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerprint(part)
	if !equalFingerprints(fp2, multiRedefinesPart) {
		return nil, nil, false
	}

	return parseRedefines, lineFromMultiRedefines(items, part), true
}

func isOccurrence(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), occurs) {
		return nil, nil, false
	}

	return parseOccurs, items, true
}

// isMultilineOccurrence is a fingerprinting function that validates whether a
// line is an indicator for, and sibling of a multi-line occurrence defintion
//
// it first checks the fingerprint of the line against the first fingerprint of
// a multi-line OCCURS definition
// then uses nx() to get the next line, and validate that against the second
// fingerprint of a multi-line OCCURS definition
//
// when this is successful, the parseOccurs parserfn is returned, along with a
// new, single, line object built from the two fingerprinted line objects.
func isMultilineOccurrence(nx func() []item, items []item) (parser, []item, bool) {
	fp := getFingerprint(items)
	if !equalFingerprints(fp, multiOccurs) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerprint(part)
	if !equalFingerprints(fp2, multiOccursPart) {
		return nil, nil, false
	}

	return parseOccurs, lineFromMultiOccurs(items, part), true
}

// getFingerprint constructs a fingerprint for a slice of items
func getFingerprint(items []item) fingerprint {
	fp := make(fingerprint, len(items))
	for i, l := range items {
		fp[i] = l.typ
	}

	return fp
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