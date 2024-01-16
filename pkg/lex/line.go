package lex

// lineTypeDescriptions maps line types to their string descriptions.
var lineTypeDescriptions = map[lineType]string{
	lineStruct:             "lineType: struct",
	linePIC:                "lineType: PIC",
	lineJunk:               "lineType: Junk",
	lineRedefines:          "lineType: Redefines",
	lineGroupRedefines:     "lineType: GroupRedefines",
	lineMultilineRedefines: "lineType: multiline Redefines",
	lineOccurs:             "lineType: Occurrence",
	lineMultilineOccurs:    "lineType: multiline Occurrence",
	lineEnum:               "lineType: enum",
}

// lineType identifies the type of a full line.
type lineType int

// Constants for lineType.
const (
	lineStruct             lineType = iota // is a new struct line
	linePIC                                // is a new PIC line
	lineJunk                               // is a line full of rubbish text
	lineRedefines                          // is a line containing a PIC redefinition
	lineGroupRedefines                     // is a line containing a group redefinition
	lineMultilineRedefines                 // is a line containing a redefinition without a target
	lineOccurs                             // is a line containing a PIC occurrence
	lineMultilineOccurs                    // is a line containing an incomplete PIC occurrence
	lineEnum                               // is a line containing an enum example value
)

// line represents a line of tokens with a specific type and parser function.
type line struct {
	tokens []token
	typ    lineType
	fn     lineParser
}

// String returns the string description of the line type.
func (l lineType) String() string {
	return lineTypeDescriptions[l]
}

// buildLine constructs a line from a slice of tokens.
func buildLine(tokens []token) *line {
	parserData := parsers.Search(getWord(tokens))
	if parserData != nil {
		return &line{
			tokens: tokens,
			typ:    parserData.lineType,
			fn:     parserData.parseFunc,
		}
	}
	return &line{
		tokens: tokens,
		typ:    lineJunk,
		fn:     noop,
	}
}

// lineFromMulti constructs a line from two slices of tokens and a pattern.
func lineFromMulti(a, b []token, pattern word) []token {
	res := joinLines(a, b)
	if !equalWord(getWord(res), pattern) {
		panic("multiline redefinition builder failed to build with the correct word")
	}
	return res
}

// lineFromMultiRedefines constructs a line from two slices of tokens with a redefines pattern.
//
// a 	= 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
// b 	= 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
//
// res 	= 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2   PIC X(7).  00000195
func lineFromMultiRedefines(a, b []token) []token {
	return lineFromMulti(a, b, redefinesPattern)
}

// lineFromMultiOccurs constructs a line from two slices of tokens with an occurs pattern.
//
// a 	= 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
// b 	= 001300      OCCURS 12.                            00000242
//
// res  = 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12 00000241
func lineFromMultiOccurs(a, b []token) []token {
	return lineFromMulti(a, b, occursPattern)
}

// TODO: there is a bug in the joining of lines
// joinLines joins two slices of tokens into one.
func joinLines(a, b []token) []token {
	// copy all but the num delimiter at the end of a
	return append(a[:len(a)-1], b[2:]...)
}
