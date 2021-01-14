package lex

var (
	readers = map[lineType]func(items []item) (parser, bool){
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

func buildLine(items []item) *line {
	for typ, fingerPrinter := range readers {
		p, ok := fingerPrinter(items)
		if ok {
			return &line{
				items: items,
				typ:   typ,
				fn:    p,
			}
		}
	}

	return nil
}

func isStruct(items []item) (parser, bool) {
	lineFingerprint := make([]itemType, len(items))
	for i, l := range items {
		lineFingerprint[i] = l.typ
	}

	if equalFingerprints(lineFingerprint, nonNumDelimitedStruct) {
		return parseNonNumDelimitedStruct, true
	}

	if equalFingerprints(lineFingerprint, numDelimitedStruct) {
		return parseNumDelimitedStruct, true
	}

	return nil, false
}

func isPic(items []item) (parser, bool) {
	lineFingerprint := make([]itemType, len(items))
	for i, l := range items {
		lineFingerprint[i] = l.typ
	}

	if !equalFingerprints(lineFingerprint, pic) {
		return nil, false
	}

	return parsePIC, true
}

func isJunk(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isRedefinition(items []item) (parser, bool) {
	lineFingerprint := make([]itemType, len(items))
	for i, l := range items {
		lineFingerprint[i] = l.typ
	}

	if !equalFingerprints(redefines, pic) {
		return nil, false
	}

	return parseRedefines, true
}

func isMultilineRedefinition(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isOccurrence(items []item) (parser, bool) {

	return unimplementedParser, false
}

func isMultilineOccurrence(items []item) (parser, bool) {

	return unimplementedParser, false
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
