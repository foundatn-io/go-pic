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
		{
			name: "Simple",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X.                  00000167\n",
				items: make(chan item),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC X.", line: 0},
				{typ: itemSpace, pos: 54, val: "                  ", line: 0},
				{typ: itemNumber, pos: 72, val: "00000167", line: 0},
				{typ: itemEOL, pos: 80, val: "\n", line: 0},
				{typ: itemEOF, pos: 81, val: "", line: 1},
			},
		}, {
			name: "SimplePICWithParentheses",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC(10).                  00000167\n",
				items: make(chan item),
			},
			want: []item{
				{typ: itemNumber, pos: 0, val: "000600", line: 0},
				{typ: itemSpace, pos: 6, val: "         ", line: 0},
				{typ: itemNumber, pos: 15, val: "10", line: 0},
				{typ: itemSpace, pos: 17, val: "  ", line: 0},
				{typ: itemIdentifier, pos: 19, val: "X710203-STATEMENT-TYPE", line: 0},
				{typ: itemSpace, pos: 41, val: "       ", line: 0},
				{typ: itemPIC, pos: 48, val: "PIC(10).", line: 0},
				{typ: itemSpace, pos: 56, val: "                  ", line: 0},
				{typ: itemNumber, pos: 74, val: "00000167", line: 0},
				{typ: itemEOL, pos: 82, val: "\n", line: 0},
				{typ: itemEOF, pos: 83, val: "", line: 1},
			},
		},
		{
			name: "SimpleWithREDEFINES",
			l: &lexer{
				name:  "lexer",
				input: "000420             15  X710203-SORT-CNTRY-CD    REDEFINES               00000142\n000420                 X710203-DOCUMENT-ID-TIE  PIC XX.                 00000143\n",
				items: make(chan item),
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
				{typ: itemPIC, pos: 129, val: "PIC XX.", line: 1},
				{typ: itemSpace, pos: 136, val: "                 ", line: 1},
				{typ: itemNumber, pos: 153, val: "00000143", line: 1},
				{typ: itemEOL, pos: 161, val: "\n", line: 1},
				{typ: itemEOF, pos: 162, val: "", line: 2},
			},
		},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			go tt.l.run()

			items := make([]item, 0)
			for { // nolint: gosimple // test code
				select {
				case i, open := <-tt.l.items:
					if !open {
						goto finalize
					}
					items = append(items, i)
					if i.typ == itemEOF {
						goto finalize
					}
				}
			}
		finalize:
			require.ElementsMatch(t, tt.want, items)
		})
	}
}
