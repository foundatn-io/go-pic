package pic

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"reflect"
	"strings"
)

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
	return NewDecoder(bytes.NewReader(data)).Decode(v)
}

type decoder struct {
	s    *bufio.Scanner
	done bool
}

// Decoder ...
type Decoder interface {
	Decode(interface{}) error
}

// NewDecoder builds a new decoder using a bufio.Scanner for the given input
// io.Reader.
func NewDecoder(r io.Reader) Decoder {
	return &decoder{
		s: bufio.NewScanner(r),
	}
}

// Decode scans through each line of the input data, attempting to unpack its
// values into the provided destination struct.
func (d *decoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr || rv.IsNil() {
		return errors.New("decode: unmarshal target object is not a pointer, or is nil")
	}

	if rv.Elem().Kind() == reflect.Slice {
		return d.decodeLines(rv.Elem())
	}

	ok, err := d.decodeLine(rv)
	if d.done && err == nil && !ok {
		return io.EOF
	}

	return err
}

// decodeLine scans the next line in the scanner, sets d.done to true if the
// scanner has reached EOF (and returns false indicating not to continue),
// otherwise, detecting the appropriate setter function based on the type of the
// given value v, and unpacks a string representation of the bytes associated
// with that line, into the object v using the identified setter function, set.
func (d *decoder) decodeLine(v reflect.Value) (bool, error) {
	if ok := d.s.Scan(); !ok {
		d.done = true
		return false, nil
	}

	set := newSetFunc(v.Type(), 0, 0)
	return true, set(v, string(d.s.Bytes()))
}

// decodeLines iterates through each line of the scanner, builds an instance of
// the type that the line is expected to be unpacked into, sets the type to the
// scanned values, and appends it to the incoming object v
func (d *decoder) decodeLines(v reflect.Value) (err error) {
	currentType := v.Type().Elem()
	for {
		newValue := reflect.New(currentType).Elem()
		ok, err := d.decodeLine(newValue)
		if err != nil {
			return err
		}

		if ok {
			v.Set(reflect.Append(v, newValue))
		}

		if d.done {
			break
		}
	}

	return nil
}

// newValFromLine cuts a value out of a string line from character indices
// start to end
func newValFromLine(line string, start int, end int) string {
	if len(line) == 0 || start > len(line) {
		return ""
	}

	if end > len(line) {
		end = len(line)
	}

	return strings.Trim(line[start-1:end], " ")
}
