package copybook

import (
	"bytes"
	"fmt"
	"go/format"
	"io"
	"log"
	"reflect"
	"strings"
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

func (c *Copybook) Preview() {
	log.Println("------ STRUCT PREVIEW -----")
	treePrint(c.Root, 0)
	log.Println("------- END PREVIEW -------")
}

func treePrint(node *lex.Record, nest int) {
	if node.Typ == reflect.Struct {
		indent := strings.Repeat(">", nest)
		log.Printf("L%d: %s%s", nest, indent, node.Name)
		nest++
		for _, nn := range node.Children {
			treePrint(nn, nest)
		}
	} else {
		indent := strings.Repeat("-", nest)
		log.Printf("L%d: %s%s", nest, indent, node.Name)
	}
}

func (c *Copybook) WriteToStruct(writer io.Writer) error {
	b := bytes.Buffer{}
	if err := c.t.Execute(&b, c); err != nil {
		return fmt.Errorf("failed template copybook data: %w", err)
	}

	bb, err := format.Source(b.Bytes())
	if err != nil {
		return fmt.Errorf("failed to gofmt templated copybook data: %w", err)
	}

	_, err = writer.Write(bb)
	return fmt.Errorf("failed to write templated copybook data: %w", err)
}
