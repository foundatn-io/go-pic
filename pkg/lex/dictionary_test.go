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
			a:    numDelimitedStructWord,
			b:    numDelimitedStructWord,
			want: true,
		}, {
			name: "NotEqual",
			a:    numDelimitedStructWord,
			b:    nonNumDelimitedStructWord,
			want: false,
		}, {
			name: "NotPrebuilt-Equal",
			a:    word{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			b:    word{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			want: true,
		}, {
			name: "NotPrebuilt-NotEqual",
			a:    word{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			b:    word{itemNumber, itemSpace, itemPIC, itemSpace, itemNumber},
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
					typ: itemNumber,
					val: "000123",
				}, {
					typ: itemSpace,
					pos: 6,
					val: "  ",
				}, {
					typ: itemNumber,
					pos: 8,
					val: "10",
				}, {
					typ: itemSpace,
					pos: 10,
					val: "  ",
				}, {
					typ: itemIdentifier,
					pos: 12,
					val: "OBJ-A",
				}, {
					typ: itemDot,
					pos: 17,
					val: ".",
				}, {
					typ: itemSpace,
					pos: 18,
					val: "  ",
				}, {
					typ: itemNumber,
					pos: 20,
					val: "000123",
				},
			},
			want: numDelimitedStructWord,
		}, {
			name: "Returns-NumDelimited",
			in: []token{
				{
					typ: itemSpace,
					val: "  ",
				}, {
					typ: itemNumber,
					val: "10",
				}, {
					typ: itemSpace,
					val: "  ",
				}, {
					typ: itemIdentifier,
					val: "OBJ-A",
				}, {
					typ: itemDot,
					val: ".",
				}, {
					typ: itemSpace,
					val: "  ",
				},
			},
			want: nonNumDelimitedStructWord,
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
