package lex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_lexer_run(t *testing.T) {
	tests := []struct {
		name string
		l    *lexer
		want []item
	}{
		{ // nolint:dupl
			name: "Simple",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X.                  00000167\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC X", line: 0},
				{typ: itemDot, pos: 53, val: ".", line: 0},
				{typ: itemSpace, pos: 54, val: "                  ", line: 0},
				{typ: itemNumber, pos: 72, val: "00000167", line: 0},
				{typ: itemEOL, pos: 80, val: "\n", line: 0},
				{typ: itemEOF, pos: 81, val: "", line: 1},
			},
		}, { // nolint:dupl
			name: "SimpleIdentifierWithNameStartingWith_RorP",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  P-OBJECT       PIC X.                  00000167\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "P-OBJECT", line: 0},
				{typ: itemSpace, pos: 27, val: "       ", line: 0},
				{typ: itemPIC, pos: 34, val: "PIC X", line: 0},
				{typ: itemDot, pos: 39, val: ".", line: 0},
				{typ: itemSpace, pos: 40, val: "                  ", line: 0},
				{typ: itemNumber, pos: 58, val: "00000167", line: 0},
				{typ: itemEOL, pos: 66, val: "\n", line: 0},
				{typ: itemEOF, pos: 67, val: "", line: 1},
			},
		}, { // nolint:dupl
			name: "SimplePICWithParentheses",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X(10).                  00000167\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC X(10)", line: 0},
				{typ: itemDot, pos: 57, val: ".", line: 0},
				{typ: itemSpace, pos: 58, val: "                  ", line: 0},
				{typ: itemNumber, pos: 76, val: "00000167", line: 0},
				{typ: itemEOL, pos: 84, val: "\n", line: 0},
				{typ: itemEOF, pos: 85, val: "", line: 1},
			},
		},
		{
			name: "SimpleWithREDEFINES",
			l: &lexer{
				name:  "lexer",
				input: "000420             15  X710203-SORT-CNTRY-CD    REDEFINES               00000142\n000420                 X710203-DOCUMENT-ID-TIE  PIC XX.                 00000143\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, val: "000420"},
				{typ: itemSpace, pos: 6, val: "             "},
				{typ: itemNumber, pos: 19, val: "15"},
				{typ: itemSpace, pos: 21, val: "  "},
				{typ: itemIdentifier, pos: 23, val: "X710203-SORT-CNTRY-CD"},
				{typ: itemSpace, pos: 44, val: "    "},
				{typ: itemREDEFINES, pos: 48, val: "REDEFINES"},
				{typ: itemSpace, pos: 57, val: "               "},
				{typ: itemNumber, pos: 72, val: "00000142"},
				{typ: itemEOL, pos: 80, val: "\n"},
				{typ: itemNumber, pos: 81, val: "000420", line: 1},
				{typ: itemSpace, pos: 87, val: "                 ", line: 1},
				{typ: itemIdentifier, pos: 104, val: "X710203-DOCUMENT-ID-TIE", line: 1},
				{typ: itemSpace, pos: 127, val: "  ", line: 1},
				{typ: itemPIC, pos: 129, val: "PIC XX", line: 1},
				{typ: itemDot, pos: 135, val: ".", line: 1},
				{typ: itemSpace, pos: 136, val: "                 ", line: 1},
				{typ: itemNumber, pos: 153, val: "00000143", line: 1},
				{typ: itemEOL, pos: 161, val: "\n", line: 1},
				{typ: itemEOF, pos: 162, val: "", line: 2},
			},
		},
		{
			name: "SimplePICWithParentheses_OCCURS",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X(10)  OCCURS 2.  00000167\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC X(10)", line: 0},
				{typ: itemSpace, pos: 57, val: "  ", line: 0},
				{typ: itemOCCURS, pos: 59, val: "OCCURS 2", line: 0},
				{typ: itemDot, pos: 67, val: ".", line: 0},
				{typ: itemSpace, pos: 68, val: "  ", line: 0},
				{typ: itemNumber, pos: 70, val: "00000167", line: 0},
				{typ: itemEOL, pos: 78, val: "\n", line: 0},
				{typ: itemEOF, pos: 79, val: "", line: 1},
			},
		}, { // nolint:dupl // test data
			name: "SimplePICWithParentheses_FloatExplicitDecimalPoint",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC 9(10).9(3).                  00000167\n",
				items: make([]item, 0),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC 9(10).9(3)", line: 0},
				{typ: itemDot, pos: 62, val: ".", line: 0},
				{typ: itemSpace, pos: 63, val: "                  ", line: 0},
				{typ: itemNumber, pos: 81, val: "00000167", line: 0},
				{typ: itemEOL, pos: 89, val: "\n", line: 0},
				{typ: itemEOF, pos: 90, val: "", line: 1},
			},
		},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			tt.l.run()
			require.ElementsMatch(t, tt.want, tt.l.items)
		})
	}
}
