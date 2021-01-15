package lex

import (
	"log"
	"reflect"
	"testing"
)

func Test_parseLines(t *testing.T) {
	root := &record{Typ: reflect.Struct, Name: "root", depthMap: make(map[string]*record)}
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
	tests := []struct {
		name string
		in   *Tree
	}{
		{
			name: "Simple",
			in: &Tree{
				lex: New("test", `000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                   00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9.               00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.                  00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9.               00000119`),
			},
		}, {
			name: "RedefinesWithParentheses",
			in: &Tree{
				lex: New("test", `000170         10  DUMMY-SUB-GROUP-1.                                   00000116
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC X(4).               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150              DUMMY-GROUP-2-OBJECT-E      PIC X(4).               00000227`),
			},
		}, {
			name: "Redefines",
			in: &Tree{
				lex: New("test", `000170         10  DUMMY-SUB-GROUP-1.                                   00000116
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC XXXX.               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150              DUMMY-GROUP-2-OBJECT-E      PIC XXXX.               00000227`),
			},
		},
	}

	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			res := tt.in.Parse()
			log.Println(res)
		})
	}
}
