package lex

// lineType identifies the type of a full line
type lineType int

var (
	lineTypeStrings = map[lineType]string{
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
)

func (l lineType) String() string {
	return lineTypeStrings[l]
}

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

type line struct {
	items []item
	typ   lineType
	fn    parser
}

func buildLine(items []item) *line {
	d := parsers.Search(getWord(items))
	if d != nil {
		return &line{
			items: items,
			typ:   d.typ,
			fn:    d.fn,
		}
	}

	return &line{
		items: items,
		typ:   lineJunk,
		fn:    noOp,
	}
}

// a 	= 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
// b 	= 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
//
// res 	= 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2   PIC X(7).  00000195
func lineFromMultiRedefines(a, b []item) []item {
	res := joinLines(len(redefinesWord), a, b)

	if !equalWord(getWord(res), redefinesWord) {
		panic("multiline redefinition builder failed to build a redefinition with the correct word")
	}

	return res
}

// a 	= 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
// b 	= 001300      OCCURS 12.                            00000242
//
// res  = 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12 00000241
func lineFromMultiOccurs(a, b []item) []item {
	res := joinLines(len(occursWord), a, b)

	if !equalWord(getWord(res), occursWord) {
		panic("multiline redefinition builder failed to build an occurrence with the correct word")
	}

	return res
}

func joinLines(size int, a, b []item) []item {
	res := make([]item, size)
	// copy all but the num delimiter at the end of a
	i := 0
	for i < len(a)-1 {
		res[i] = a[i]
		i++
	}

	// j is 2 so that num delimiter and space are ignored from b
	i -= 2
	for j := 2; j < len(b); j++ {
		res[i+j] = b[j]
	}

	return res
}
