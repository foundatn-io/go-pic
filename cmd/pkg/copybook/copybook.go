package copybook

import (
	"bytes"
	"go/format"
	"io"
	"reflect"
	"text/template"

	"github.com/pgmitche/go-pic/pkg/lex"
)

type Copybook struct {
	Name string
	Root *lex.Record
	t    *template.Template
}

type Record struct {
	Num     int          // Num is not necessarily required
	Level   int          // Level is not necessarily required
	Name    string       // Name is required
	Picture reflect.Kind // Picture is required
	Length  int          // Length is required
	Occurs  int          // Occurs is required if present
}

func New(name string, t *template.Template) *Copybook {
	return &Copybook{
		Name: name,
		t:    t,
	}
}

func (c *Copybook) WriteToStruct(writer io.Writer) error {
	b := bytes.Buffer{}
	if err := c.t.Execute(&b, c); err != nil {
		return err
	}

	bb, err := format.Source(b.Bytes())
	if err != nil {
		return err
	}

	_, err = writer.Write(bb)
	return err
}
