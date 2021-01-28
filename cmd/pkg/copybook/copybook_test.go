package copybook

import (
	"bytes"
	"io"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/pgmitche/go-pic/cmd/pkg/template"
	"github.com/pgmitche/go-pic/pkg/lex"
)

func Test_Build(t *testing.T) {
	tests := []struct {
		name  string
		input io.Reader
	}{
		{
			name: "ExampleData_BasicInput",
			input: strings.NewReader(`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9.               00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.               00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9.               00000119
`),
		}, {
			name: "ExampleData_FullFile",
			input: strings.NewReader(
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
000550     05  DUMMY-GROUP-2                    PIC X(201).             00000162
000830     05  DUMMY-GROUP-3     REDEFINES      DUMMY-GROUP-2.          00000195
000840         10  DUMMY-GROUP-2-OBJECT-A       PIC X(14).              00000196
000850         10  DUMMY-GROUP-2-OBJECT-B       PIC 9(7).               00000197
001060         10  DUMMY-GROUP-2-OBJECT-C       PIC XXXX.               00000218
001070         10  DUMMY-GROUP-2-OBJECT-D       PIC X.                  00000219
001130         10  DUMMY-GROUP-2-OBJECT-E       PIC X(7).               00000225
001140         10  DUMMY-GROUP-2-OBJECT-F       REDEFINES               00000226
001150              DUMMY-GROUP-2-OBJECT-E      PIC X(7).               00000227
001280         10  DUMMY-SUBGROUP-2-GETSDROPPED.                        00000240
001290           15  DUMMY-SUBGROUP-2-OBJECT-A  PIC X(12)               00000241
001300             OCCURS 12.                                           00000242
001310         10  DUMMY-GROUP-2-OBJECT-G       PIC X(12).              00000243
001320         10  DUMMY-GROUP-2-OBJECT-H       PIC X(12).              00000244
`),
		},
	}

	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			c := New(tt.name, "main", template.Copybook())

			b, err := ioutil.ReadAll(tt.input)
			require.NoError(t, err)

			lxr := lex.New(tt.name, string(b))
			tree := lex.NewTree(lxr)
			c.Root = tree.Parse()

			var buf bytes.Buffer
			require.NoError(t, c.WriteToStruct(&buf))
			require.NotEmpty(t, buf)
		})
	}
}
