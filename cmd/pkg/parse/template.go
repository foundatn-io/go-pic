package parse

import (
	"fmt"
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
	"IndexComment": IndexComment,
}

var structTemplate = template.Must(
	template.New("struct").
		Funcs(templateFuncs).
		Parse(`
// AUTOGENERATED FILE
// File generated with go-pic
// AUTOGENERATED FILE

// nolint
package tempcopybook

// Copybook{{.Name}} contains a representation of your provided Copybook
type Copybook{{.Name}} struct {
	{{- range $element := .Records}}
		{{SanitiseName $element.Name}} {{GoType $element.Picture $element.Occurs}} {{PICTag $element.Length $element.Occurs}}{{IndexComment $element.Length $element.Occurs}} 
	{{- end}}
}
`))

// GoType translates a type into a go type
func GoType(t reflect.Kind, i int) string {
	switch t {
	case reflect.String:
		if i > 0 {
			return "[]string"
		}
		return "string"
	case reflect.Int:
		if i > 0 {
			return "[]int"
		}
		return "int"
	default:
		panic(fmt.Sprintf("unrecognized type %v", t))
	}
}

func PICTag(l int, i int) string {
	if i > 0 {
		return "`" + fmt.Sprintf("pic:\"%d,%d\"", l, i) + "`"
	}
	return "`" + fmt.Sprintf("pic:\"%d\"", l) + "`"
}

func IndexComment(l int, i int) string {
	size := l
	if i > 0 {
		size *= i
	}

	s := startPos
	endPos += size
	startPos = endPos
	return fmt.Sprintf(" // start:%d end:%d", s, endPos-1)
}

func SanitiseName(s string) string {
	return special.ReplaceAllString(s, "")
}
