package lex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_equalFingerprints(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		a, b fingerprint
		want bool
	}{
		{
			name: "Equal",
			a:    numDelimitedStructFp,
			b:    numDelimitedStructFp,
			want: true,
		}, {
			name: "NotEqual",
			a:    numDelimitedStructFp,
			b:    nonNumDelimitedStructFp,
			want: false,
		}, {
			name: "NotPrebuilt-Equal",
			a:    fingerprint{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			b:    fingerprint{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			want: true,
		}, {
			name: "NotPrebuilt-NotEqual",
			a:    fingerprint{itemNumber, itemSpace, itemIdentifier, itemSpace, itemNumber},
			b:    fingerprint{itemNumber, itemSpace, itemPIC, itemSpace, itemNumber},
			want: false,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, equalFingerprints(tt.a, tt.b))
		})
	}
}

func Test_getFingerprint(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		in   []item
		want fingerprint
	}{
		{
			name: "Returns-NumDelimited",
			in: []item{
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
			want: numDelimitedStructFp,
		}, {
			name: "Returns-NumDelimited",
			in: []item{
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
			want: nonNumDelimitedStructFp,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, getFingerprint(tt.in))
		})
	}
}
