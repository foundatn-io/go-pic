package pic

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"reflect"
	"strings"
)

// ErrNotAPointer is returned when the target object is not a pointer.
var ErrNotAPointer = errors.New("decode: unmarshal target object is not a pointer")

// ErrNilPointer is returned when the target object is nil.
var ErrNilPointer = errors.New("decode: unmarshal target object is nil")

type decoder struct {
	s    *bufio.Scanner
	done bool
}

// Decoder ...
type Decoder interface {
	Decode(interface{}) error
}

// Example
//
// type Company struct {
// 	Name                       string `pic:"1,30"`  // 30 chars, 1-30
// 	Employees                  int    `pic:"31,39"` // 9 chars, 31-39
// 	BusinessRegistrationNumber string `pic:"40,51"` // 12 chars 40-51
// }
// s := `HERARE30CHARS    OFCOMPANYNAME999999999RegistrtnNum`
// c := &Company{}
// pic.Unmarshal([]byte(s)]), c);

// Unmarshal accepts input data, and a destination object it will build a new
// decoder and decode the input into the target object.
func Unmarshal(data []byte, v interface{}) error {
	if err := NewDecoder(bytes.NewReader(data)).Decode(v); err != nil {
		return fmt.Errorf("unmarshal: %w", err)
	}
	return nil
}

// NewDecoder builds a new decoder using a bufio.Scanner for the given input
// io.Reader.
func NewDecoder(r io.Reader) Decoder {
	return &decoder{
		s: bufio.NewScanner(r),
	}
}

// Decode scans through each line of the input data, attempting to unpack its
// values into the provided destination struct. If the target object is a slice,
// it decodes each line into a separate element of the slice. If the target
// object is not a slice, it decodes the first line into the target object.
func (d *decoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr {
		return ErrNotAPointer
	}
	if rv.IsNil() {
		return ErrNilPointer
	}
	if rv.Elem().Kind() == reflect.Slice {
		return d.decodeLines(rv.Elem())
	}
	return d.decodeLine(rv.Elem())
}

// decodeLine scans the next line in the scanner, sets d.done to true if the
// scanner has reached EOF (and returns ErrEOF),
// otherwise, unpacks a byte representation of the bytes associated
// with that line, into the object v using the identified setter function, set.
func (d *decoder) decodeLine(v reflect.Value) error {
	if ok := d.s.Scan(); !ok {
		d.done = true
		return io.EOF
	}
	return newSetValueFunc(v.Type(), 0, 0)(v, string(d.s.Bytes()))
}

// decodeLines iterates through each line of the scanner, builds an instance of
// the type that the line is expected to be unpacked into, sets the type to the
// scanned values, and appends it to the incoming object v
func (d *decoder) decodeLines(v reflect.Value) error {
	currentType := v.Type().Elem()
	for !d.done {
		newValue := reflect.New(currentType).Elem()
		if err := d.decodeLine(newValue); err != nil {
			if errors.Is(err, io.EOF) {
				return nil
			}
			return err
		}
		v.Set(reflect.Append(v, newValue))
	}
	return nil
}

// newValFromLine extracts a substring from line, starting at startIndex and ending at endIndex.
// If startIndex is greater than the length of line, it returns an empty string.
func newValFromLine(line string, startIndex int, endIndex int) string {
	if len(line) == 0 || startIndex > len(line) {
		return ""
	}
	if endIndex > len(line) {
		endIndex = len(line)
	}
	return strings.TrimSpace(line[startIndex-1 : endIndex])
}
