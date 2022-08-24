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
	tokens []token
	typ    lineType
	fn     parser
}

// buildLine takes a group of tokens expected to represent a valid word that is
// stored within the trie. If the word is located in the trie, a line object is
// built that holds the tokens, the type of the word, and the appropriate parser
// to use for that word. If the word is not located in the trie, a line object
// is built that treats the tokens as junk and worthy of no op.
func buildLine(tokens []token) *line {
	w := trie.Lookup(getWord(tokens))
	if w != nil {
		return &line{
			tokens: tokens,
			typ:    w.typ,
			fn:     w.fn,
		}
	}

	return &line{
		tokens: tokens,
		typ:    lineJunk,
		fn:     noOp,
	}
}

// lineFromMultiRedefines accepts multiple (two) lines from a copybook
// joins their content in such a way that the PIC would read as if it was
// defined on a single line (view example below) and checks the trie to see if
// the line that is built matches a stored word.
//
// a 	= 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
// b 	= 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
//
// res 	= 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2   PIC X(7).  00000195
//
// FIXME: lineFromMultiRedefines & lineFromMultiOccurs should be compounded into
//
//	a single function that accepts an expected word parameter.
func lineFromMultiRedefines(a, b []token) []token {
	res := joinLines(len(redefinesWord), a, b)

	if !equalWord(getWord(res), redefinesWord) {
		panic("multiline redefinition builder failed to build a redefinition with the correct word")
	}

	return res
}

// lineFromMultiOccurs accepts multiple (two) lines from a copybook
// joins their content in such a way that the PIC would read as if it was
// defined on a single line (view example below) and checks the trie to see if
// the line that is built matches a stored word.
//
// a 	= 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)  00000241
// b 	= 001300      OCCURS 12.                            00000242
//
// res  = 001290  15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12) OCCURS 12 00000241
//
// FIXME: lineFromMultiRedefines & lineFromMultiOccurs should be compounded into
//
//	a single function that accepts an expected word parameter.
func lineFromMultiOccurs(a, b []token) []token {
	res := joinLines(len(occursWord), a, b)

	if !equalWord(getWord(res), occursWord) {
		panic("multiline redefinition builder failed to build an occurrence with the correct word")
	}

	return res
}

// joinLines joins two sets of tokens to read as if the two tokens were
// defined on a single line in the copybook. This if for use with PICs that are
// defined over multiple lines.
//
// Refer to lineFromMultiRedefines & lineFromMultiOccurs
func joinLines(size int, a, b []token) []token {
	res := make([]token, size)
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
