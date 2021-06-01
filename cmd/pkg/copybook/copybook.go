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

	"github.com/foundatn-io/go-pic/pkg/lex"
)

type Copybook struct {
	Name    string
	Root    *lex.Record
	Package string
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

func New(name, pkg string, t *template.Template) *Copybook {
	return &Copybook{
		Name:    name,
		Package: pkg,
		t:       t,
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
	var b bytes.Buffer
	if err := c.t.Execute(&b, c); err != nil {
		return fmt.Errorf("failed template copybook data: %w", err)
	}

	bb, err := format.Source(b.Bytes())
	if err != nil {
		return fmt.Errorf("failed to gofmt templated copybook data: %w", err)
	}

	if _, err = writer.Write(bb); err != nil {
		return fmt.Errorf("failed to write templated copybook data: %w", err)
	}

	b.Reset()

	return nil
}
