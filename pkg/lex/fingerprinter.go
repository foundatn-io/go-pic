package lex

type fingerprint []itemType
type fingerprinter func(nx func() []item, items []item) (parser, []item, bool)

const (
	recordDescriptionIndicator = "01"
)

var (
	readers = map[lineType]fingerprinter{
		lineJunk:               isJunk,
		lineStruct:             isStruct,
		linePIC:                isPic,
		lineRedefines:          isRedefinition,
		lineGroupRedefines:     isGroupRedefinition,
		lineMultilineRedefines: isMultilineRedefinition,
		lineOccurs:             isOccurrence,
		lineMultilineOccurs:    isMultilineOccurrence,
	}

	fingerprints = map[string]fingerprint{
		// num space num space text dot space num
		// 000160  05  DUMMY-GROUP-1.  00000115
		"numDelimitedStruct": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber},

		// space num space text dot space
		//  05  DUMMY-GROUP-1.
		"nonNumDelimitedStruct": {itemSpace, itemNumber, itemSpace, itemIdentifier, itemDot, itemSpace},

		// num space num space text space pic space num
		// 000190  15  DUMMY-GROUP-1-OBJECT-B  PIC X.  00000118
		"pic": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber},

		// 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2 PIC X.  00000195
		"redefines": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber},

		// 000830  05  DUMMY-GROUP-3  REDEFINES   DUMMY-GROUP-2.  00000195
		"groupRedefines": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemIdentifier, itemDot, itemSpace, itemNumber},

		// 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
		"multiRedefines": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemREDEFINES, itemSpace, itemNumber},

		// 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
		"multiRedefinesPart": {itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemDot, itemSpace, itemNumber},

		// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12. 00000241
		"occurs": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemOCCURS, itemSpace, itemNumber},

		// 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
		"multiOccurs": {itemNumber, itemSpace, itemNumber, itemSpace, itemIdentifier, itemSpace, itemPIC, itemSpace, itemNumber},

		// 001300      OCCURS 12.                            00000242
		"multiOccursPart": {itemNumber, itemSpace, itemOCCURS, itemSpace, itemNumber},
	}
)

func init() { // nolint:gochecknoinits
	fps := make([]fingerprint, 0)
	for _, v := range fingerprints {
		fps = append(fps, v)
	}

	for i, a := range fps {
		for _, b := range fps[i+1:] {
			if equalFingerprints(a, b) {
				panic("duplicate fingerprints")
			}
		}
	}
}

func isStruct(_ func() []item, items []item) (parser, []item, bool) {
	fp := getFingerprint(items)
	if equalFingerprints(fp, fingerprints["nonNumDelimitedStruct"]) {
		// if the level number is 01, ignore this object.
		// refer to README.md Level Number section
		if items[1].val == recordDescriptionIndicator {
			return noOp, items, true
		}

		return parseNonNumDelimitedStruct, items, true
	}

	if equalFingerprints(fp, fingerprints["numDelimitedStruct"]) {
		// if the level number is 01, ignore this object.
		// refer to README.md Level Number section
		if items[2].val == recordDescriptionIndicator {
			return noOp, items, true
		}

		return parseNumDelimitedStruct, items, true
	}

	return nil, nil, false
}

func isPic(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), fingerprints["pic"]) {
		return nil, nil, false
	}

	return parsePIC, items, true
}

func isJunk(_ func() []item, _ []item) (parser, []item, bool) {
	return noOp, nil, false
}

func isRedefinition(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), fingerprints["redefines"]) {
		return nil, nil, false
	}

	return parseRedefines, items, true
}

func isGroupRedefinition(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), fingerprints["groupRedefines"]) {
		return nil, nil, false
	}

	return parseGroupRedefines, items, true
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
	if !equalFingerprints(fp, fingerprints["multiRedefines"]) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerprint(part)
	if !equalFingerprints(fp2, fingerprints["multiRedefinesPart"]) {
		return nil, nil, false
	}

	return parseRedefines, lineFromMultiRedefines(items, part), true
}

func isOccurrence(_ func() []item, items []item) (parser, []item, bool) {
	if !equalFingerprints(getFingerprint(items), fingerprints["occurs"]) {
		return nil, nil, false
	}

	return parseOccurs, items, true
}

// isMultilineOccurrence is a fingerprinting function that validates whether a
// line is an indicator for, and sibling of a multi-line occurrence definition
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
	if !equalFingerprints(fp, fingerprints["multiOccurs"]) {
		return nil, nil, false
	}

	// glob the next line and build it into the response item line
	part := nx()
	fp2 := getFingerprint(part)
	if !equalFingerprints(fp2, fingerprints["multiOccursPart"]) {
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
