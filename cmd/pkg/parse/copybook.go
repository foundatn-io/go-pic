package parse

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"reflect"
)

type Copybook struct {
	Name    string
	Records []*record
}

type record struct {
	Num     int
	Level   int
	Name    string
	Picture reflect.Kind
	Length  int
	Occurs  int
}

func (c Copybook) ToSruct(writer io.Writer) error {
	b := bytes.Buffer{}
	if err := structTemplate.Execute(&b, c); err != nil {
		return err
	}

	bb, err := format.Source(b.Bytes())
	if err != nil {
		return err
	}

	_, err = writer.Write(bb)
	return err
}

func (c *Copybook) findAndRemove(r *record) error {
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

func (c *Copybook) findAndEdit(want, replace *record) error {
	cc := *c
	for i, rec := range cc.Records {
		if rec.Name == replace.Name {
			cc.Records[i] = want
		}
	}

	return fmt.Errorf("replacement target %s not found", replace.Name)
}
