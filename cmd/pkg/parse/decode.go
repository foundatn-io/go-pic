package parse

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"strconv"
	"strings"
	"sync"
)

type Decoder struct {
	s     *bufio.Scanner
	done  bool
	cache sync.Map
}

func Unmarshal(data []byte, c *Copybook) error {
	return NewDecoder(bytes.NewReader(data)).Decode(c)
}

func NewDecoder(r io.Reader) *Decoder {
	d := &Decoder{
		s: bufio.NewScanner(r),
	}
	return d
}

func (d *Decoder) Decode(c *Copybook) error {
	return d.scanLines(c)
}

// scanlines advances the scanner line by line until an error is reached
// or the done flag is toggled by the decoder
func (d *Decoder) scanLines(c *Copybook) (err error) {
	for {
		_, err := d.scanLine(c)
		if err != nil {
			return err
		}

		if d.done {
			break
		}
	}
	return nil
}

// scanLine reads each line, advancing the scanner, decoding the buffer as
// a string and attempting to interpret the line as a record definition
// appending the record to the running copybook schema
func (d *Decoder) scanLine(c *Copybook) (bool, error) {
	if ok := d.s.Scan(); !ok {
		d.done = true
		return false, nil
	}

	l := string(d.s.Bytes())
	rec, err := d.findDataRecord(l, c)
	if rec == nil {
		if err != nil {
			return true, nil
		}
		return false, err
	}

	c.Records = append(c.Records, rec)
	return true, nil
}

// findDataRecord accepts a string, line, representing a line in a copybook definition
// and attempts to parse the line, detecting whether it is a picture definition (PIC),
// picture redefinition (REDEFINES), or a repeated picture definition (OCCURS).
func (d *Decoder) findDataRecord(line string, c *Copybook) (*record, error) { // nolint:gocyclo
	line = trimExtraWhitespace(line)
	switch getLineType(line) {
	case pic:
		return d.picRecord(line)

	case picIncomplete:
		return d.incompletePICRecord(line)

	case redefinesSingle:
		r, err := d.redefinedRecord(line)
		if err != nil {
			return nil, err
		}

		if err := c.findAndRemove(r); err != nil {
			return nil, err
		}

		return nil, nil

	case redefinesMulti:
		want, replace, err := d.multiLineRedefinedRecord(line)
		if err != nil {
			return nil, err
		}

		if err := c.findAndEdit(want, replace); err != nil {
			return nil, err
		}

	case occursSingle:
		return d.occursRecord(line)

	case occursMulti:
		return d.multiLineOccursRecord(line)

	case xxx:
		log.Printf("go-pic didn't understand, or chose to ignore line: \"%s\"", line)
		return nil, nil
	}

	return nil, errors.New("matched as record, but record type unimplemented")
}

// Defines picture clause
// 000600         10  DUMMY-1       PIC X.                  00000167
// 000620         10  DUMMY-2        PIC 9(7).               00000169
//
// picRecord decodes the line as a picture definition, returns the record details
// and adds the picture to the PIC definition cache
func (d *Decoder) picRecord(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 6 {
		return nil, errors.New("picRecord: does not match expected length/format")
	}

	num, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	lvl, err := strconv.Atoi(ss[1])
	if err != nil {
		return nil, err
	}

	t, err := parsePICType(ss[4])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[4])
	if err != nil {
		return nil, err
	}

	rec := &record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: t,
		Length:  size,
	}

	return d.toCache(rec), nil
}

func (d *Decoder) incompletePICRecord(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 4 {
		return nil, errors.New("incompletePICRecord: does not match expected length/format")
	}

	lvl, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	t, err := parsePICType(ss[3])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[3])
	if err != nil {
		return nil, err
	}

	rec := &record{
		Level:   lvl,
		Name:    ss[1],
		Picture: t,
		Length:  size,
	}

	return d.toCache(rec), nil
}

func (d *Decoder) findRedefinesTarget(line string) (*record, error) {
	line = trimExtraWhitespace(line)
	ss := strings.Split(line, " ")
	if len(ss) != 5 {
		return nil, errors.New("findRedefinesTarget: does not match expected length/format")
	}

	r := d.fromCache(ss[1])
	if r == nil {
		return nil, fmt.Errorf("REDEFINES target %s not found", ss[1])
	}

	return r, nil
}

func (d *Decoder) toCache(r *record) *record {
	if r, ok := d.cache.Load(r.Name); ok {
		return r.(*record)
	}

	rec, _ := d.cache.LoadOrStore(r.Name, r)
	return rec.(*record)
}

func (d *Decoder) fromCache(name string) *record {
	r, ok := d.cache.Load(name)
	if !ok {
		return nil
	}

	return r.(*record)
}

// REDEFINES is currently broken, as with finding start/end for our records,
// we need to implement a previous line/next line check. Document redefinitions
// seem to be of two kinds, not always on the same line, examples below:
//
// 000420             15  DUMMY-5    REDEFINES               00000142
// 000420                 DUMMY-4  PIC XX.                 00000143
func (d *Decoder) multiLineRedefinedRecord(line string) (*record, *record, error) {
	ss := strings.Split(line, " ")

	if len(ss) != 5 {
		return nil, nil, errors.New("multiLineRedefinedRecord: does not match expected length/format")
	}

	if ok := d.s.Scan(); !ok {
		d.done = true
		return nil, nil, nil
	}

	replace := string(d.s.Bytes())
	r, err := d.findRedefinesTarget(replace)
	if err != nil {
		return nil, nil, err
	}

	want := *r
	want.Name = ss[2]
	return &want, r, nil
}

// redefinedRecord locates a replacement record and removes it
// based on the assumption that intra-line REDEFINES statements
// are followed by the definition of a subgroup, which wholly replaces
// the target picture definition
//
// 000590     05  DUMMY-3  REDEFINES  DUMMY-2. 00000166
func (d *Decoder) redefinedRecord(line string) (*record, error) {
	ss := strings.Split(line, " ")

	if len(ss) != 6 {
		return nil, errors.New("redefinedRecord: does not match expected length/format")
	}

	replace := ss[4]

	if r := d.fromCache(strings.TrimSuffix(replace, ".")); r != nil {
		return r, nil
	}

	return nil, fmt.Errorf("REDEFINES %s target is not present", replace)
}

// Intra-line - 001350           15  DUMMY-1 PIC X  OCCURS 12.       00000247
func (d *Decoder) occursRecord(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 8 {
		return nil, errors.New("occursRecord: does not match expected length/format")
	}

	num, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	lvl, err := strconv.Atoi(ss[1])
	if err != nil {
		return nil, err
	}

	t, err := parsePICType(ss[4])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[4])
	if err != nil {
		return nil, err
	}

	occurs, err := parseOccursCount(ss[6])
	if err != nil {
		return nil, err
	}

	rec := &record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: t,
		Length:  size,
		Occurs:  occurs,
	}

	return d.toCache(rec), nil
}

// Multi-line - 001290           15  DUMMY-1 PIC X(12)               00000241
// 				001300               OCCURS 12.                      00000242
func (d *Decoder) multiLineOccursRecord(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 6 {
		return nil, errors.New("multiLineOccursRecord: does not match expected length/format")
	}

	if ok := d.s.Scan(); !ok {
		d.done = true
		return nil, nil
	}

	check := string(d.s.Bytes())
	if !occursWord.MatchString(check) {
		return nil, errors.New("multiLineOccursRecord: expect next token to contain OCCURS statement, not found")
	}

	occStr := strings.Split(trimExtraWhitespace(check), " ")
	if len(occStr) != 4 {
		return nil, errors.New("multiLineOccursRecord: next token containing OCCURS statement does not match expected length/format")
	}

	num, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	lvl, err := strconv.Atoi(ss[1])
	if err != nil {
		return nil, err
	}

	t, err := parsePICType(ss[4])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[4])
	if err != nil {
		return nil, err
	}

	occurs, err := parseOccursCount(occStr[2])
	if err != nil {
		return nil, err
	}

	rec := &record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: t,
		Length:  size,
		Occurs:  occurs,
	}

	return d.toCache(rec), nil
}
