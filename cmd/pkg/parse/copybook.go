package parse

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"reflect"
	"regexp"
	"text/template"
)

var special = regexp.MustCompile("[^a-zA-Z0-9]+")

// keeping track
var (
	startPos = 1
	endPos   = 1
)

var templateFuncs = template.FuncMap{
	"GoType":       GoType,
	"PICTag":       PICTag,
	"SanitiseName": SanitiseName,
}

var structTemplate = template.Must(
	template.New("struct").
		Funcs(templateFuncs).
		Parse(`
package tempcopybook

// YourCopybook contains a representation of your provided Copybook
type YourCopybook struct {
	{{- range $element := .}}
		{{SanitiseName $element.Name}} {{GoType $element.Picture}} {{PICTag $element.Length}}
	{{- end}}
}
`))

type Copybook []*record

type record struct {
	Num     int
	Level   int
	Name    string
	Picture reflect.Kind
	Length  int
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

	for i, rec := range cc {
		if rec.Name == r.Name {
			copy(cc[i:], cc[i+1:])
			cc[len(cc)-1] = nil
			cc = cc[:len(cc)-1]
			*c = cc
			return nil
		}
	}

	return fmt.Errorf("replacement target %s not found", r.Name)
}

func (c *Copybook) findAndEdit(want, replace *record) error {
	cc := *c
	for i, rec := range cc {
		if rec.Name == replace.Name {
			cc[i] = want
		}
	}

	return fmt.Errorf("replacement target %s not found", replace.Name)
}

// GoType translates a type into a go type
func GoType(t reflect.Kind) string {
	switch t {
	case reflect.String:
		return "string"
	case reflect.Int:
		return "int"
	default:
		panic(fmt.Sprintf("unrecognized type %v", t))
	}
}

func PICTag(l int) string {
	s := startPos
	endPos += l
	startPos = endPos
	return "`" + fmt.Sprintf("pic:\"%d,%d,%d\"", l, s, endPos-1) + "`"
}

func SanitiseName(s string) string {
	return special.ReplaceAllString(s, "")
}
