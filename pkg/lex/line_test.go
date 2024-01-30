package lex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_joinLinesFuncs(t *testing.T) {
	tests := []struct {
		name string
		fn   func(a, b []token) []token
		a    []token
		b    []token
		want []token
	}{
		{
			name: "Basic",
			fn:   joinLines,
			a: []token{
				{kind: tokenKindNumber, value: "000830"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindNumber, value: "05"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindIdentifier, value: "DUMMY-OBJECT-3"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindREDEFINES, value: "REDEFINES"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindNumber, value: "00000195"},
			}, // 000830  05  DUMMY-OBJECT-3  REDEFINES   00000195
			b: []token{
				{kind: tokenKindNumber, value: "001150"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindIdentifier, value: "DUMMY-OBJECT-2"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindPIC, value: "PIC"},
				{kind: tokenKindNumber, value: "7"},
				{kind: tokenKindDot, value: "."},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindNumber, value: "00000227"},
			}, // 001150  DUMMY-OBJECT-2   PIC X(7).  00000227
			want: []token{
				{kind: tokenKindNumber, value: "000830"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindNumber, value: "05"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindIdentifier, value: "DUMMY-OBJECT-3"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindREDEFINES, value: "REDEFINES"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindIdentifier, value: "DUMMY-OBJECT-2"},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindPIC, value: "PIC"},
				{kind: tokenKindNumber, value: "7"},
				{kind: tokenKindDot, value: "."},
				{kind: tokenKindSpace, value: " "},
				{kind: tokenKindNumber, value: "00000227"},
			}, // 000830  05  DUMMY-OBJECT-3  REDEFINES  DUMMY-OBJECT-2   PIC X(7).  00000195
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			result := tt.fn(tt.a, tt.b)
			require.Equal(t, tt.want, result)
		})
	}
}
