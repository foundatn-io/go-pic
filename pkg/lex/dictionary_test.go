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
			a:    word{tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindNumber},
			b:    word{tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindNumber},
			want: true,
		}, {
			name: "NotPrebuilt-NotEqual",
			a:    word{tokenKindNumber, tokenKindSpace, tokenKindIdentifier, tokenKindSpace, tokenKindNumber},
			b:    word{tokenKindNumber, tokenKindSpace, tokenKindPIC, tokenKindSpace, tokenKindNumber},
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
					kind:  tokenKindNumber,
					value: "000123",
				}, {
					kind:     tokenKindSpace,
					position: 6,
					value:    "  ",
				}, {
					kind:     tokenKindNumber,
					position: 8,
					value:    "10",
				}, {
					kind:     tokenKindSpace,
					position: 10,
					value:    "  ",
				}, {
					kind:     tokenKindIdentifier,
					position: 12,
					value:    "OBJ-A",
				}, {
					kind:     tokenKindDot,
					position: 17,
					value:    ".",
				}, {
					kind:     tokenKindSpace,
					position: 18,
					value:    "  ",
				}, {
					kind:     tokenKindNumber,
					position: 20,
					value:    "000123",
				},
			},
			want: numDelimitedStructPattern,
		}, {
			name: "Returns-NumDelimited",
			in: []token{
				{
					kind:  tokenKindSpace,
					value: "  ",
				}, {
					kind:  tokenKindNumber,
					value: "10",
				}, {
					kind:  tokenKindSpace,
					value: "  ",
				}, {
					kind:  tokenKindIdentifier,
					value: "OBJ-A",
				}, {
					kind:  tokenKindDot,
					value: ".",
				}, {
					kind:  tokenKindSpace,
					value: "  ",
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
