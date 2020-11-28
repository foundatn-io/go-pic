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
				name:  "",
				input: "000600         10  X710203-STATEMENT-TYPE       PIC X.                  00000167\n",
				items: make(chan item),
			},
			want: []item{},
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
						break
					}
					items = append(items, i)
					if i.typ == itemEOF {
						break
					}
				}
			}
			require.NotEmpty(t, items)
		})
	}
}
