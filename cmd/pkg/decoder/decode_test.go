package decoder

import (
	"bytes"
	"log"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/pgmitche/go-pic/cmd/pkg/copybook"
	"github.com/pgmitche/go-pic/cmd/pkg/template"
)

func TestDecoder_Unmarshal(t *testing.T) {
	tests := []struct {
		name string
		c    *copybook.Copybook
		in   string
	}{
		{
			name: "SuccessfullyParseBasicDummyCopybook",
			c:    copybook.New("dummy", template.CopyBook),
			in: `000600         10  DUMMY-1                PIC X.                  00000167
000610         10  DUMMY-2                PIC X(3).               00000168
000620         10  DUMMY-3                PIC 9(7).               00000169
000630         10  DUMMY-4                PIC 9(4).               00000170
000640         10  DUMMY-5                PIC XX.                 00000171
000650         10  DUMMY-6                PIC 9(7).               00000172
000660         10  DUMMY-7                PIC X(10).              00000173
000670         10  DUMMY-8                PIC X.                  00000174`,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			err := Unmarshal([]byte(tt.in), tt.c)
			require.NoError(t, err)

			var b bytes.Buffer
			err = tt.c.WriteToStruct(&b)
			require.NoError(t, err)

			log.Println(b.String())
			// YourCopybook contains a representation of your provided Copybook
			// type YourCopybook struct {
			// 	DUMMY1 string `pic:"1"`
			// 	DUMMY2 string `pic:"3"`
			// 	DUMMY3 int `pic:"7"`
			// 	DUMMY4 int `pic:"4"`
			// 	DUMMY5 string `pic:"2"`
			// 	DUMMY6 int `pic:"7"`
			// 	DUMMY7 string `pic:"10"`
			// 	DUMMY8 string `pic:"1"`
			// }
		})
	}
}
