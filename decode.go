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
// 	Name                       string `pic:"30"` //30 chars, 0-30
// 	Employees                  int    `pic:"9"`  //9 chars, 31-39
// 	BusinessRegistrationNumber string `pic:"12"` //12 chars 40-51
// }
// s := `HERARE30CHARS    OFCOMPANYNAME999999999RegistrtnNum`
// c := &Company{}
// pic.Unmarshal([]byte(s)]), c);

func Unmarshal(data []byte, v interface{}) error {
	return NewDecoder(bytes.NewReader(data)).Decode(v)
}

type Decoder struct {
	s    *bufio.Scanner
	done bool
}

func NewDecoder(r io.Reader) *Decoder {
	return &Decoder{
		s: bufio.NewScanner(r),
	}
}

func (d *Decoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr || rv.IsNil() {
		return errors.New("decode: unmarshal target object is not a pointer, or is nil")
	}

	if rv.Elem().Kind() == reflect.Slice {
		return d.scanLines(rv.Elem())
	}

	ok, err := d.scanLine(rv)
	if d.done && err == nil && !ok {
		return io.EOF
	}

	return err
}

func (d *Decoder) scanLine(v reflect.Value) (bool, error) {
	if ok := d.s.Scan(); !ok {
		d.done = true
		return false, nil
	}

	l := string(d.s.Bytes())
	t := v.Type()

	set := newSetFunc(t, 0)
	return true, set(v, l)
}

func (d *Decoder) scanLines(v reflect.Value) (err error) {
	ct := v.Type().Elem()
	for {
		nv := reflect.New(ct).Elem()
		ok, err := d.scanLine(nv)
		if err != nil {
			return err
		}

		if ok {
			v.Set(reflect.Append(v, nv))
		}
		if d.done {
			break
		}
	}
	return nil
}

func newValFromLine(s string, start int, end int) string {
	if len(s) == 0 || start > len(s) {
		return ""
	}

	if end > len(s) {
		end = len(s)
	}

	return strings.Trim(s[start-1:end], " ")
}
