package copybook

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"reflect"
	"text/template"
)

type Copybook struct {
	Name    string
	Records []*Record
	t       *template.Template
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
		Name:    name,
		Records: make([]*Record, 0),
		t:       t,
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

// RemoveRecord is used to locate a REDEFINES target and remove it from
// the stored records, otherwise failing if the target is not found.
func (c *Copybook) RemoveRecord(r *Record) error {
	cc := *c

	for i, rec := range cc.Records {
		if rec.Name == r.Name {
			copy(cc.Records[i:], cc.Records[i+1:])
			cc.Records[len(cc.Records)-1] = nil
			cc.Records = cc.Records[:len(cc.Records)-1]
			*c = cc
			return nil
		}
	}

	return fmt.Errorf("replacement target %s not found", r.Name)
}

// RedefineRecord is used to locate a REDEFINES target that was defined
// over multiple lines in a copybook file, and replace its name with
// the source of the REDEFINES statement, otherwise failing if
// the target is not found.
func (c *Copybook) RedefineRecord(want *Record) error {
	cc := *c
	for i, rec := range cc.Records {
		if rec.Name == want.Name {
			cc.Records[i] = want
		}
	}

	return fmt.Errorf("replacement target %s not found", want.Name)
}
