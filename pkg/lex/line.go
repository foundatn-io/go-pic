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

	return unimplementedParser, nil, false
}

func isMultilineOccurrence(nx func() []item, items []item) (parser, []item, bool) {

	return unimplementedParser, nil, false
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
