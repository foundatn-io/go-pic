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
			name: "test",
			l: &lexer{
				name:  "lexer",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X.                  00000167\n",
				items: make(chan item),
			},
			want: []item{
				{
					typ:  itemNumber,
					pos:  0,
					val:  "000600",
					line: 0,
				}, {
					typ:  itemSpace,
					pos:  6,
					val:  "         ",
					line: 0,
				}, {
					typ:  itemNumber,
					pos:  15,
					val:  "10",
					line: 0,
				}, {
					typ:  itemSpace,
					pos:  17,
					val:  "  ",
					line: 0,
				}, {
					typ:  itemIdentifier,
					pos:  19,
					val:  "X710203-STATEMENT-TYPE",
					line: 0,
				}, {
					typ:  itemSpace,
					pos:  41,
					val:  "       ",
					line: 0,
				}, {
					typ:  itemPIC,
					pos:  48,
					val:  "PIC X.",
					line: 0,
				}, {
					typ:  itemSpace,
					pos:  54,
					val:  "                  ",
					line: 0,
				}, {
					typ:  itemNumber,
					pos:  72,
					val:  "00000167",
					line: 0,
				}, {
					typ:  itemEOL,
					pos:  80,
					val:  "\n",
					line: 0,
				}, {
					typ:  itemEOF,
					pos:  81,
					val:  "",
					line: 1,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			go tt.l.run()

			items := make([]item, 0)
			for {
				select {
				case i, open := <-tt.l.items:
					if !open {
						goto finalise
					}
					items = append(items, i)
					if i.typ == itemEOF {
						goto finalise
					}
				}
			}
		finalise:
			require.ElementsMatch(t, tt.want, items)
		})
	}
}
