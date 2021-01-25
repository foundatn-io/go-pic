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
			name: "Go",
			input: strings.NewReader(`000160     05  DUMMY-GROUP-1.                                           00000115
000170         10  DUMMY-SUB-GROUP-1.                                00000116
000180             15  DUMMY-GROUP-1-OBJECT-A   PIC 9.               00000117
000190             15  DUMMY-GROUP-1-OBJECT-B   PIC X.               00000118
000200             15  DUMMY-GROUP-1-OBJECT-C   PIC 9.               00000119
`),
		},
	}

	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			c := New(tt.name, template.Copybook())

			b, err := ioutil.ReadAll(tt.input)
			require.NoError(t, err)

			lxr := lex.New("go-pic", string(b))
			tree := lex.NewTree(lxr)
			c.Root = tree.Parse()

			var buf bytes.Buffer
			require.NoError(t, c.WriteToStruct(&buf))

			require.NotEmpty(t, buf)
		})
	}
}
