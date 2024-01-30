package lex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_lexer_run(t *testing.T) {
	tests := []struct {
		name string
		l    *lexerState
		want []token
	}{
		{ //nolint:dupl
			name: "Simple",
			l: &lexerState{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X.                  00000167\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "         ", lineNumber: 0},
				{kind: tokenKindNumber, position: 15, value: "10", lineNumber: 0},
				{kind: tokenKindSpace, position: 17, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 19, value: "X710203-STATEMENT-TYPE", lineNumber: 0},
				{kind: tokenKindSpace, position: 41, value: "       ", lineNumber: 0},
				{kind: tokenKindPIC, position: 48, value: "PIC X", lineNumber: 0},
				{kind: tokenKindDot, position: 53, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 54, value: "                  ", lineNumber: 0},
				{kind: tokenKindNumber, position: 72, value: "00000167", lineNumber: 0},
				{kind: tokenKindEOL, position: 80, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 81, value: "", lineNumber: 1},
			},
		}, { //nolint:dupl
			name: "SimpleIdentifierWithNameStartingWith_RorP",
			l: &lexerState{
				name:  "lexer",
				input: "000600         10  P-OBJECT       PIC X.                  00000167\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "         ", lineNumber: 0},
				{kind: tokenKindNumber, position: 15, value: "10", lineNumber: 0},
				{kind: tokenKindSpace, position: 17, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 19, value: "P-OBJECT", lineNumber: 0},
				{kind: tokenKindSpace, position: 27, value: "       ", lineNumber: 0},
				{kind: tokenKindPIC, position: 34, value: "PIC X", lineNumber: 0},
				{kind: tokenKindDot, position: 39, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 40, value: "                  ", lineNumber: 0},
				{kind: tokenKindNumber, position: 58, value: "00000167", lineNumber: 0},
				{kind: tokenKindEOL, position: 66, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 67, value: "", lineNumber: 1},
			},
		}, { //nolint:dupl
			name: "SimplePICWithParentheses",
			l: &lexerState{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X(10).                  00000167\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "         ", lineNumber: 0},
				{kind: tokenKindNumber, position: 15, value: "10", lineNumber: 0},
				{kind: tokenKindSpace, position: 17, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 19, value: "X710203-STATEMENT-TYPE", lineNumber: 0},
				{kind: tokenKindSpace, position: 41, value: "       ", lineNumber: 0},
				{kind: tokenKindPIC, position: 48, value: "PIC X(10)", lineNumber: 0},
				{kind: tokenKindDot, position: 57, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 58, value: "                  ", lineNumber: 0},
				{kind: tokenKindNumber, position: 76, value: "00000167", lineNumber: 0},
				{kind: tokenKindEOL, position: 84, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 85, value: "", lineNumber: 1},
			},
		},
		{
			name: "SimpleWithREDEFINES",
			l: &lexerState{
				name:  "lexer",
				input: "000420             15  X710203-SORT-CNTRY-CD    REDEFINES               00000142\n000420                 X710203-DOCUMENT-ID-TIE  PIC XX.                 00000143\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, value: "000420"},
				{kind: tokenKindSpace, position: 6, value: "             "},
				{kind: tokenKindNumber, position: 19, value: "15"},
				{kind: tokenKindSpace, position: 21, value: "  "},
				{kind: tokenKindIdentifier, position: 23, value: "X710203-SORT-CNTRY-CD"},
				{kind: tokenKindSpace, position: 44, value: "    "},
				{kind: tokenKindREDEFINES, position: 48, value: "REDEFINES"},
				{kind: tokenKindSpace, position: 57, value: "               "},
				{kind: tokenKindNumber, position: 72, value: "00000142"},
				{kind: tokenKindEOL, position: 80, value: "\n"},
				{kind: tokenKindNumber, position: 81, value: "000420", lineNumber: 1},
				{kind: tokenKindSpace, position: 87, value: "                 ", lineNumber: 1},
				{kind: tokenKindIdentifier, position: 104, value: "X710203-DOCUMENT-ID-TIE", lineNumber: 1},
				{kind: tokenKindSpace, position: 127, value: "  ", lineNumber: 1},
				{kind: tokenKindPIC, position: 129, value: "PIC XX", lineNumber: 1},
				{kind: tokenKindDot, position: 135, value: ".", lineNumber: 1},
				{kind: tokenKindSpace, position: 136, value: "                 ", lineNumber: 1},
				{kind: tokenKindNumber, position: 153, value: "00000143", lineNumber: 1},
				{kind: tokenKindEOL, position: 161, value: "\n", lineNumber: 1},
				{kind: tokenKindEOF, position: 162, value: "", lineNumber: 2},
			},
		},
		{ //nolint:dupl
			name: "SimplePICWithParentheses_OCCURS",
			l: &lexerState{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X(10)  OCCURS 2.  00000167\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "         ", lineNumber: 0},
				{kind: tokenKindNumber, position: 15, value: "10", lineNumber: 0},
				{kind: tokenKindSpace, position: 17, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 19, value: "X710203-STATEMENT-TYPE", lineNumber: 0},
				{kind: tokenKindSpace, position: 41, value: "       ", lineNumber: 0},
				{kind: tokenKindPIC, position: 48, value: "PIC X(10)", lineNumber: 0},
				{kind: tokenKindSpace, position: 57, value: "  ", lineNumber: 0},
				{kind: tokenKindOCCURS, position: 59, value: "OCCURS 2", lineNumber: 0},
				{kind: tokenKindDot, position: 67, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 68, value: "  ", lineNumber: 0},
				{kind: tokenKindNumber, position: 70, value: "00000167", lineNumber: 0},
				{kind: tokenKindEOL, position: 78, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 79, value: "", lineNumber: 1},
			},
		}, { //nolint:dupl // test data
			name: "SimplePICWithParentheses_FloatExplicitDecimalPoint",
			l: &lexerState{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC 9(10).9(3).                  00000167\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "         ", lineNumber: 0},
				{kind: tokenKindNumber, position: 15, value: "10", lineNumber: 0},
				{kind: tokenKindSpace, position: 17, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 19, value: "X710203-STATEMENT-TYPE", lineNumber: 0},
				{kind: tokenKindSpace, position: 41, value: "       ", lineNumber: 0},
				{kind: tokenKindPIC, position: 48, value: "PIC 9(10).9(3)", lineNumber: 0},
				{kind: tokenKindDot, position: 62, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 63, value: "                  ", lineNumber: 0},
				{kind: tokenKindNumber, position: 81, value: "00000167", lineNumber: 0},
				{kind: tokenKindEOL, position: 89, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 90, value: "", lineNumber: 1},
			},
		}, { //nolint:dupl
			name: "88EnumNonDelimitedSkipped",
			l: &lexerState{
				name:  "lexer",
				input: "   88   EXAMPLE-ENUM  VALUE  'N'. \n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindSpace, position: 0, value: "   ", lineNumber: 0},
				{kind: tokenKindNumber, position: 3, value: "88", lineNumber: 0},
				{kind: tokenKindSpace, position: 5, value: "   ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 8, value: "EXAMPLE-ENUM", lineNumber: 0},
				{kind: tokenKindSpace, position: 20, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 22, value: "VALUE", lineNumber: 0},
				{kind: tokenKindSpace, position: 27, value: "  ", lineNumber: 0},
				{kind: tokenKindEnum, position: 29, value: "'N'", lineNumber: 0},
				{kind: tokenKindDot, position: 32, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 33, value: " ", lineNumber: 0},
				{kind: tokenKindEOL, position: 34, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 35, value: "", lineNumber: 1},
			},
		}, { //nolint:dupl
			name: "88EnumDelimitedSkipped",
			l: &lexerState{
				name:  "lexer",
				input: "000600   88   EXAMPLE-ENUM  VALUE  'N'. 00000600\n",
				items: make([]token, 0),
			},
			want: []token{
				{kind: tokenKindNumber, position: 0, value: "000600", lineNumber: 0},
				{kind: tokenKindSpace, position: 6, value: "   ", lineNumber: 0},
				{kind: tokenKindNumber, position: 9, value: "88", lineNumber: 0},
				{kind: tokenKindSpace, position: 11, value: "   ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 14, value: "EXAMPLE-ENUM", lineNumber: 0},
				{kind: tokenKindSpace, position: 26, value: "  ", lineNumber: 0},
				{kind: tokenKindIdentifier, position: 28, value: "VALUE", lineNumber: 0},
				{kind: tokenKindSpace, position: 33, value: "  ", lineNumber: 0},
				{kind: tokenKindEnum, position: 35, value: "'N'", lineNumber: 0},
				{kind: tokenKindDot, position: 38, value: ".", lineNumber: 0},
				{kind: tokenKindSpace, position: 39, value: " ", lineNumber: 0},
				{kind: tokenKindNumber, position: 40, value: "00000600", lineNumber: 0},
				{kind: tokenKindEOL, position: 48, value: "\n", lineNumber: 0},
				{kind: tokenKindEOF, position: 49, value: "", lineNumber: 1},
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
