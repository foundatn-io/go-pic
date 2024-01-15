package lex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_equalWord(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		a, b word
		want bool
	}{
		{
			name: "Equal",
			a:    numDelimitedStructPattern,
			b:    numDelimitedStructPattern,
			want: true,
		}, {
			name: "NotEqual",
			a:    numDelimitedStructPattern,
			b:    nonNumDelimitedStructPattern,
			want: false,
		}, {
			name: "NotPrebuilt-Equal",
			a:    word{tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenNumber},
			b:    word{tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenNumber},
			want: true,
		}, {
			name: "NotPrebuilt-NotEqual",
			a:    word{tokenNumber, tokenSpace, tokenIdentifier, tokenSpace, tokenNumber},
			b:    word{tokenNumber, tokenSpace, tokenPIC, tokenSpace, tokenNumber},
			want: false,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, equalWord(tt.a, tt.b))
		})
	}
}

func Test_getWord(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		in   []token
		want word
	}{
		{
			name: "Returns-NumDelimited",
			in: []token{
				{
					typ: tokenNumber,
					val: "000123",
				}, {
					typ: tokenSpace,
					pos: 6,
					val: "  ",
				}, {
					typ: tokenNumber,
					pos: 8,
					val: "10",
				}, {
					typ: tokenSpace,
					pos: 10,
					val: "  ",
				}, {
					typ: tokenIdentifier,
					pos: 12,
					val: "OBJ-A",
				}, {
					typ: tokenDot,
					pos: 17,
					val: ".",
				}, {
					typ: tokenSpace,
					pos: 18,
					val: "  ",
				}, {
					typ: tokenNumber,
					pos: 20,
					val: "000123",
				},
			},
			want: numDelimitedStructPattern,
		}, {
			name: "Returns-NumDelimited",
			in: []token{
				{
					typ: tokenSpace,
					val: "  ",
				}, {
					typ: tokenNumber,
					val: "10",
				}, {
					typ: tokenSpace,
					val: "  ",
				}, {
					typ: tokenIdentifier,
					val: "OBJ-A",
				}, {
					typ: tokenDot,
					val: ".",
				}, {
					typ: tokenSpace,
					val: "  ",
				},
			},
			want: nonNumDelimitedStructPattern,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, getWord(tt.in))
		})
	}
}
