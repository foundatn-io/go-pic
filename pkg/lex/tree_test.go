package lex

import (
	"fmt"
	"log"
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_parseLines(t *testing.T) {
	root := &Record{Typ: reflect.Struct, Name: "root", depthMap: make(map[string]*Record)}
	tree := &Tree{
		lIdx: -1,
		lines: []line{
			{
				typ: lineStruct,
				fn:  parseNumDelimitedStruct,
				items: []item{
					{typ: itemNumber, pos: 0, val: "000160", line: 0},
					{typ: itemSpace, pos: 6, val: "         ", line: 0},
					{typ: itemNumber, pos: 15, val: "05", line: 0},
					{typ: itemSpace, pos: 17, val: "  ", line: 0},
					{typ: itemIdentifier, pos: 19, val: "DUMMY-GROUP-1", line: 0},
					{typ: itemDot, pos: 32, val: ".", line: 0},
					{typ: itemSpace, pos: 33, val: "                  ", line: 0},
					{typ: itemNumber, pos: 51, val: "00000115", line: 0},
					{typ: itemEOL, pos: 59, val: "\n", line: 0},
				},
			}, {
				typ: linePIC,
				fn:  parsePIC,
				items: []item{
					{typ: itemNumber, pos: 0, val: "000600", line: 1},
					{typ: itemSpace, pos: 6, val: "         ", line: 1},
					{typ: itemNumber, pos: 15, val: "10", line: 1},
					{typ: itemSpace, pos: 17, val: "  ", line: 1},
					{typ: itemIdentifier, pos: 19, val: "DUMMY-GROUP-1-OBJECT-A", line: 1},
					{typ: itemSpace, pos: 41, val: "       ", line: 1},
					{typ: itemPIC, pos: 48, val: "PIC X.", line: 1},
					{typ: itemSpace, pos: 54, val: "                  ", line: 1},
					{typ: itemNumber, pos: 72, val: "00000167", line: 1},
					{typ: itemEOL, pos: 80, val: "\n", line: 1},
				},
			}, {
				typ: lineStruct,
				fn:  parseNonNumDelimitedStruct,
				items: []item{
					{typ: itemSpace, pos: 6, val: "         ", line: 2},
					{typ: itemNumber, pos: 15, val: "05", line: 2},
					{typ: itemSpace, pos: 17, val: "  ", line: 2},
					{typ: itemIdentifier, pos: 19, val: "DUMMY-GROUP-2", line: 2},
					{typ: itemDot, pos: 32, val: ".", line: 2},
					{typ: itemSpace, pos: 33, val: "                  ", line: 2},
					{typ: itemEOL, pos: 59, val: "\n", line: 2},
					{typ: itemEOF, pos: 60, val: "", line: 3},
				},
			},
		},
		state: root,
	}

	tree.parseLines(tree.state)
	log.Println(tree.state)
}

func Test_Parse(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		in   *Tree
		want *Record
	}{
		{ // nolint:dupl // test data
			name: "Simple",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 3,
				Children: []*Record{{
					Name:   "DUMMY-GROUP-1",
					Typ:    reflect.Struct,
					Length: 3,
					Children: []*Record{{
						Name:   "DUMMY-SUB-GROUP-1",
						Typ:    reflect.Struct,
						Length: 3,
						Children: []*Record{{
							Name:   "DUMMY-GROUP-1-OBJECT-A",
							Typ:    reflect.Uint,
							Length: 1,
						}, {
							Name:   "DUMMY-GROUP-1-OBJECT-B",
							Typ:    reflect.String,
							Length: 1,
						}, {
							Name:   "DUMMY-GROUP-1-OBJECT-C",
							Typ:    reflect.Uint,
							Length: 1,
						}},
					}},
				}},
			},
			in: NewTree(
				New("test",
					`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9.               00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.               00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9.               00000119
		`)),
		}, {
			name: "RedefinesWithParentheses",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 5,
				Children: []*Record{{
					Name:   "DUMMY-SUB-GROUP-1",
					Typ:    reflect.Struct,
					Length: 0,
				}, {
					Name:   "DUMMY-GROUP-2-OBJECT-D",
					Typ:    reflect.String,
					Length: 1,
				}, {
					Name:   "DUMMY-GROUP-2-OBJECT-F",
					Typ:    reflect.String,
					Length: 4,
				}},
			},
			in: NewTree(
				New("test",
					`000170         10  DUMMY-SUB-GROUP-1.                                   00000116
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC X(4).               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150             DUMMY-GROUP-2-OBJECT-E       PIC X(4).               00000227
		`)),
		}, {
			name: "Redefines",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 5,
				Children: []*Record{{
					Name:   "DUMMY-SUB-GROUP-1",
					Typ:    reflect.Struct,
					Length: 0,
				}, {
					Name:   "DUMMY-GROUP-2-OBJECT-D",
					Typ:    reflect.String,
					Length: 1,
				}, {
					Name:   "DUMMY-GROUP-2-OBJECT-F",
					Typ:    reflect.String,
					Length: 4,
				}},
			},
			in: NewTree(
				New("test",
					`000170         10  DUMMY-SUB-GROUP-1.                                   00000116
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC XXXX.               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150              DUMMY-GROUP-2-OBJECT-E      PIC XXXX.               00000227
		`)),
		},
		{
			name: "SimpleOccurs",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 12,
				Children: []*Record{{
					Name:   "DUMMY-GROUP-1",
					Typ:    reflect.Struct,
					Length: 12,
					Children: []*Record{{
						Name:   "DUMMY-SUB-GROUP-1",
						Typ:    reflect.Struct,
						Length: 12,
						Children: []*Record{
							{
								Name:   "DUMMY-GROUP-1-OBJECT-A",
								Typ:    reflect.Uint,
								Length: 1,
								Occurs: 12,
							},
						},
					}},
				}},
			},
			in: NewTree(
				New("test",
					`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                   00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9  OCCURS 12.       00000117
`)),
		}, {
			name: "MultilineOccurs",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 12,
				Children: []*Record{{
					Name:   "DUMMY-GROUP-1",
					Typ:    reflect.Struct,
					Length: 12,
					Children: []*Record{{
						Name:   "DUMMY-SUB-GROUP-1",
						Typ:    reflect.Struct,
						Length: 12,
						Children: []*Record{
							{
								Name:   "DUMMY-GROUP-1-OBJECT-A",
								Typ:    reflect.Uint,
								Length: 1,
								Occurs: 12,
							},
						},
					}},
				}},
			},
			in: NewTree(
				New("test",
					`000160     05  DUMMY-GROUP-1.                             00000115
000170         10  DUMMY-SUB-GROUP-1.                        00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9        00000117
001300             OCCURS 12.                                00000242
`)),
		}, {
			name: "ExampleData",
			want: &Record{
				Name:   "exampledata",
				Typ:    reflect.Struct,
				Length: 240,
				Children: []*Record{
					{
						Name:   "DUMMY-GROUP-1",
						Typ:    reflect.Struct,
						Length: 63,
						Children: []*Record{{
							Name:   "DUMMY-SUB-GROUP-1",
							Typ:    reflect.Struct,
							Length: 63,
							Children: []*Record{{
								Name:   "DUMMY-GROUP-1-OBJECT-A",
								Typ:    reflect.Uint,
								Length: 4,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-B",
								Typ:    reflect.String,
								Length: 1,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-C",
								Typ:    reflect.Uint,
								Length: 4,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-D",
								Typ:    reflect.String,
								Length: 40,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-E",
								Typ:    reflect.String,
								Length: 8,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-G",
								Typ:    reflect.String,
								Length: 2,
							}, {
								Name:   "DUMMY-GROUP-1-OBJECT-H",
								Typ:    reflect.Uint,
								Length: 4,
							},
							}}},
					}, {
						Name:   "DUMMY-GROUP-2",
						Typ:    reflect.Struct,
						Length: 177,
						Children: []*Record{{
							Name:   "DUMMY-GROUP-2-OBJECT-A",
							Typ:    reflect.String,
							Length: 14,
						}, {
							Name:   "DUMMY-GROUP-2-OBJECT-B",
							Typ:    reflect.Uint,
							Length: 7,
						}, {
							Name:   "DUMMY-GROUP-2-OBJECT-C",
							Typ:    reflect.String,
							Length: 4,
						}, {
							Name:   "DUMMY-GROUP-2-OBJECT-D",
							Typ:    reflect.String,
							Length: 1,
						}, {
							Name:   "DUMMY-GROUP-2-OBJECT-F",
							Typ:    reflect.String,
							Length: 7,
						}, {
							Name:   "DUMMY-SUBGROUP-2",
							Typ:    reflect.Struct,
							Length: 144,
							Children: []*Record{
								{
									Name:   "DUMMY-SUBGROUP-2-OBJECT-A",
									Typ:    reflect.String,
									Length: 12,
									Occurs: 12,
								},
							},
						},
						},
					},
				},
			},
			in: NewTree(New("exampledata",
				`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                   00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9(4).               00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.                  00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9(4).               00000119
000210             15  DUMMY-GROUP-1-OBJECT-D   PIC X(40).              00000120
000410             15  DUMMY-GROUP-1-OBJECT-E   PIC X(8).               00000140
000420             15  DUMMY-GROUP-1-OBJECT-F   PIC XX.                 00000141
000420             15  DUMMY-GROUP-1-OBJECT-G   REDEFINES               00000142
000420                 DUMMY-GROUP-1-OBJECT-F   PIC XX.                 00000143
000430             15  DUMMY-GROUP-1-OBJECT-H   PIC 9(4).               00000144
000550     05  DUMMY-BIGDATA                    PIC X(201).             00000162
000830     05  DUMMY-GROUP-2     REDEFINES      DUMMY-BIGDATA.          00000195
000840         10  DUMMY-GROUP-2-OBJECT-A       PIC X(14).              00000196
000850         10  DUMMY-GROUP-2-OBJECT-B       PIC 9(7).               00000197
001060         10  DUMMY-GROUP-2-OBJECT-C       PIC XXXX.               00000218
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC X(7).               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150              DUMMY-GROUP-2-OBJECT-E      PIC X(7).               00000227
001280         10  DUMMY-SUBGROUP-2.                                    00000240
001290           15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)               00000241
001300             OCCURS 12.                                           00000242
`)),
		}, {
			name: "JunkHeader",
			in: NewTree(New("JunkHeader",
				`000010******************************************************************00000100
000020****                                                             *00000101
000020****                                                             *00000101
000030**** DATA GROUP  : EXAMPLES                                      *00000102
000040**** COPY BOOK   : EXAMPLES                                      *00000103
000050**** DESCRIPTION : OUTPUT FORMAT FOR GO-PIC EXAMPLE ROUTINE      *00000104
000060****                                                             *00000105
000070******************************************************************00000106
000080 01  EXAMPLE.                                                     00000107
000150**** STUFF                                                        00000114
000160     05  DUMMY-GROUP-1.                                           00000115
`)),
			want: &Record{
				Name:   "JunkHeader",
				Typ:    reflect.Struct,
				Length: 0,
				Children: []*Record{
					{
						Name:   "DUMMY-GROUP-1",
						Typ:    reflect.Struct,
						Length: 0,
					},
				},
			},
		}, { // nolint:dupl // test data
			name: "Simple_WithExplicitDecimalPICs",
			want: &Record{
				Name:   "test",
				Typ:    reflect.Struct,
				Length: 22,
				Children: []*Record{{
					Name:   "DUMMY-GROUP-1",
					Typ:    reflect.Struct,
					Length: 22,
					Children: []*Record{{
						Name:   "DUMMY-SUB-GROUP-1",
						Typ:    reflect.Struct,
						Length: 22,
						Children: []*Record{{
							Name:   "DUMMY-GROUP-1-OBJECT-A",
							Typ:    reflect.Float64,
							Length: 12,
						}, {
							Name:   "DUMMY-GROUP-1-OBJECT-B",
							Typ:    reflect.String,
							Length: 1,
						}, {
							Name:   "DUMMY-GROUP-1-OBJECT-C",
							Typ:    reflect.Float64,
							Length: 9,
						}},
					}},
				}},
			},
			in: NewTree(
				New("test",
					`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9(9).9(2).       00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.               00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9(4).9(4).       00000119
		`)),
		},
	}

	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got := tt.in.Parse()
			deepCompare(t, tt.want, got)
		})
	}
}

func deepCompare(t *testing.T, want, got *Record) {
	require.Equal(t, want.Name, got.Name, fmt.Sprintf("name mismatch: %s", want.Name))
	require.Equal(t, want.Length, got.Length, fmt.Sprintf("length mismatch: %s", want.Name))
	require.Equal(t, want.Typ, got.Typ, fmt.Sprintf("type mismatch: %s", want.Name))
	require.Equal(t, want.Occurs, got.Occurs, fmt.Sprintf("occurrence mismatch: %s", want.Name))
	require.Equal(t, len(want.Children), len(got.Children), "nodes' children not equal, comparison not holistic")
	if want.Typ == reflect.Struct {
		for i, nn := range want.Children {
			deepCompare(t, nn, got.Children[i])
		}
	}
}
