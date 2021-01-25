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
			a:    fingerprints["numDelimitedStruct"],
			b:    fingerprints["numDelimitedStruct"],
			want: true,
		}, {
			name: "NotEqual",
			a:    fingerprints["numDelimitedStruct"],
			b:    fingerprints["nonNumDelimitedStruct"],
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
			want: fingerprints["numDelimitedStruct"],
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
			want: fingerprints["nonNumDelimitedStruct"],
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

func Test_Fingerprinter(t *testing.T) {
	t.Parallel()

	fpFns := []fingerprinter{
		0: isJunk,
		1: isStruct,
		2: isPic,
		3: isRedefinition,
		4: isGroupRedefinition,
		5: isMultilineOccurrence,
		6: isOccurrence,
		7: isMultilineOccurrence,
	}

	tests := []struct {
		name     string
		in       []item
		nextFunc func() []item
		wantIdx  int
	}{
		{
			name: "Success_MatchesPIC",
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
					typ: itemSpace,
					pos: 18,
					val: "  ",
				}, {
					typ:  itemPIC,
					pos:  20,
					val:  "PIC 9(5)",
					line: 0,
				}, {
					typ: itemDot,
					pos: 28,
					val: ".",
				}, {
					typ: itemSpace,
					pos: 29,
					val: "  ",
				}, {
					typ: itemNumber,
					pos: 31,
					val: "000123",
				},
			},
			wantIdx: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			didMatch := false
			for idx, fn := range fpFns {
				_, _, hit := fn(tt.nextFunc, tt.in)
				if hit {
					require.Equal(t, tt.wantIdx, idx, "failed to match fingerprint indices")
					didMatch = true
				} else {
					require.False(t, hit)
				}
			}
			require.True(t, didMatch)
		})
	}
}
