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

// Copybook represents a copybook.
type Copybook struct {
	Name     string
	Root     *lex.Record
	Package  string
	template *template.Template
}

// Record represents a record.
type Record struct {
	Number     int          // Number is not necessarily required
	Level      int          // Level is not necessarily required
	Name       string       // Name is required
	Type       reflect.Kind // Type is required
	Length     int          // Length is required
	Occurrence int          // Occurrence is required if present
}

// New creates a new copybook.
func New(name, pkg string, tmpl *template.Template) *Copybook {
	return &Copybook{
		Name:     name,
		Package:  pkg,
		template: tmpl,
	}
}

// Preview prints a preview of the copybook.
func (c *Copybook) Preview() {
	log.Println("------ STRUCT PREVIEW -----")
	printTree(c.Root, 0)
	log.Println("------- END PREVIEW -------")
}

// printTree prints a tree representation of a record.
func printTree(node *lex.Record, nest int) {
	if node.Typ == reflect.Struct {
		indent := strings.Repeat(">", nest)
		log.Printf("L%d: %s%s", nest, indent, node.Name)
		nest++
		for _, nestedNode := range node.Children {
			printTree(nestedNode, nest)
		}
	} else {
		indent := strings.Repeat("-", nest)
		log.Printf("L%d: %s%s", nest, indent, node.Name)
	}
}

// WriteToStruct writes the copybook to a formatted struct.
func (c *Copybook) WriteToStruct(writer io.Writer) error {
	var buffer bytes.Buffer
	if err := c.template.Execute(&buffer, c); err != nil {
		return fmt.Errorf("failed to execute template: %w", err)
	}
	formatted, err := format.Source(buffer.Bytes())
	if err != nil {
		return fmt.Errorf("failed to format source: %w", err)
	}
	if _, err = writer.Write(formatted); err != nil {
		return fmt.Errorf("failed to write to writer: %w", err)
	}
	buffer.Reset()
	return nil
}
