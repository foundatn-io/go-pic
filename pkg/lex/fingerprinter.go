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

type line struct {
	items []item
	typ   lineType
	fn    parser
}

// lineType identifies the type of a full line
type lineType int

const (
	lineStruct             lineType = iota // is a new struct line
	linePIC                                // is a new PIC line
	lineJunk                               // is a line full of rubbish text
	lineRedefines                          // is a line containing a PIC redefinition
	lineMultilineRedefines                 // is a line containing a redefinition without a target
	lineOccurs                             // is a line containing a PIC occurrence
	lineMultilineOccurs                    // is a line containing an incomplete PIC occurrence
)

func buildLine(nx func() []item, items []item) *line {
	for typ, fingerPrinter := range readers {
		p, i, ok := fingerPrinter(nx, items)
		if ok {
			return &line{
				items: i,
				typ:   typ,
				fn:    p,
			}
		}
	}

	return nil
}

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

func isOccurrence(nx func() []item, items []item) (parser, []item, bool) {
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

	return parseRedefines, lineFromMultiOccurs(items, part), true
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

// a 	= 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
// b 	= 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
//
// res 	= 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2   PIC X(7).  00000195
func lineFromMultiRedefines(a, b []item) []item {
	res := make([]item, len(redefines))
	// copy all but the num delimiter at the end of a
	i := 0
	for i < len(a)-1 {
		res[i] = a[i]
		i++
	}

	// should be 6, so that next is 8, as inserted up to res[7]
	// j is 2 so that num delimiter and space are ignored from b
	i -= 2
	for j := 2; j < len(b); j++ {
		res[i+j] = b[j]
	}

	if !equalFingerprints(getFingerPrint(res), redefines) {
		panic("multiline redefinition builder failed to build a redefinition with the correct fingerprint")
	}

	return res
}

// a 	= 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
// b 	= 001300      OCCURS 12.                            00000242
//
// res  = 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12 00000241
func lineFromMultiOccurs(a, b []item) []item {
	res := make([]item, len(occurs))

	// TODO: (pgmitche) rebuild multi line occurrence from item indices
	//
	// copy all but the num delimiter at the end of a
	// i := 0
	// for i < len(a)-1 {
	// 	res[i] = a[i]
	// 	i++
	// }
	//
	// // should be 6, so that next is 8, as inserted up to res[7]
	// // j is 2 so that num delimiter and space are ignored from b
	// i -= 2
	// for j := 2; j < len(b); j++ {
	// 	res[i+j] = b[j]
	// }

	if !equalFingerprints(getFingerPrint(res), occurs) {
		panic("multiline redefinition builder failed to build an occurrence with the correct fingerprint")
	}

	return res
}
