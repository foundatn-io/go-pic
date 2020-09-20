package decoder

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

	"github.com/pgmitche/go-pic/cmd/pkg/copybook"
)

type decoder struct {
	s     *bufio.Scanner
	done  bool
	cache sync.Map
}

// Unmarshal unpacks the given bytes into the Copybook
// it operates as a convenient point-of-entry that wraps
// the calling of New & Decode
func Unmarshal(data []byte, c *copybook.Copybook) error {
	return New(bytes.NewReader(data)).Decode(c)
}

// New builds a decoder with a scanner based on the provided reader
func New(r io.Reader) *decoder {
	return &decoder{
		s: bufio.NewScanner(r),
	}
}

// Decode scans the given reader line by line, decoding the information within
// and adding them to the given Copybook, until the last line in the readers buffer
// is read
func (d *decoder) Decode(c *copybook.Copybook) error {
	return d.scanLines(c)
}

// toCache returns a record, just stored into or previously loaded from the cache
func (d *decoder) toCache(r *copybook.Record) *copybook.Record {
	d.cache.Store(r.Name, r)
	return r
}

// fromCache loads a record, by name, from the cache if present
func (d *decoder) fromCache(name string) *copybook.Record {
	r, ok := d.cache.Load(name)
	if !ok {
		return nil
	}

	return r.(*copybook.Record)
}

// scanLines advances the scanner line by line until an error is reached
// or the done flag is toggled by the decoder
func (d *decoder) scanLines(c *copybook.Copybook) (err error) {
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
func (d *decoder) scanLine(c *copybook.Copybook) (bool, error) {
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
func (d *decoder) findDataRecord(line string, c *copybook.Copybook) (*copybook.Record, error) { // nolint:gocyclo
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

		if err := c.RemoveRecord(r); err != nil {
			return nil, err
		}

		return nil, nil

	case redefinesMulti:
		want, err := d.multiLineRedefinedRecord(line)
		if err != nil {
			return nil, err
		}

		if err := c.RedefineRecord(want); err != nil {
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

// picRecord decodes the line as a picture definition, returns the record details
// and adds the picture to the PIC definition cache
//
// Examples:
// 	000600         10  DUMMY-1       PIC X.                  00000167
// 	000620         10  DUMMY-2       PIC 9(7).               00000169
func (d *decoder) picRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != picSplitSize {
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

	pic, err := parsePICType(ss[4])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[4])
	if err != nil {
		return nil, err
	}

	rec := &copybook.Record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: pic,
		Length:  size,
	}

	return d.toCache(rec), nil
}

// incompletePICRecord accepts lines that are suspected PIC definitions that do not match
// the perfect PIC pattern, but still contain valid PIC definitions. incompletePICRecord extracts
// the key information that it can, builds a record and adds it to the cache
//
// Missing group:
// 	000420             DUMMY-1  PIC XX.         00000143
// Missing header/trailer:
// 	               10  DUMMY-1  PIC X(23).
func (d *decoder) incompletePICRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != incompletePICSplitSize {
		return nil, errors.New("incompletePICRecord: does not match expected length/format")
	}

	pic, err := parsePICType(ss[3])
	if err != nil {
		return nil, err
	}

	size, err := parsePICCount(ss[3])
	if err != nil {
		return nil, err
	}

	rec := &copybook.Record{
		Name:    ss[1],
		Picture: pic,
		Length:  size,
	}

	return d.toCache(rec), nil
}

// findRedefinesTarget accepts a string, line, expected to contain a REDEFINES target
// validates the line is in the expected format, and attempts to locate the target in
// the cache, under the assumption that only previously recorded PICs can be REDEFINED
func (d *decoder) findRedefinesTarget(line string) (*copybook.Record, error) {
	line = trimExtraWhitespace(line)
	ss := strings.Split(line, " ")
	if len(ss) != multiLineRedefinesSplitSize {
		return nil, errors.New("findRedefinesTarget: does not match expected length/format")
	}

	r := d.fromCache(ss[1])
	if r == nil {
		return nil, fmt.Errorf("findRedefinesTarget: target %s not found", ss[1])
	}

	return r, nil
}

// multiLineRedefinedRecord locates a replacement record and edits it
// to contain the same information as the original target, but updated
// with the name of the redefinition PIC. multiLineRedefinedRecord removes
// the target from the cache, replacing it with the redefined PIC.
// The assumption here is that multiline redefinitions are not of different
// types or lengths
//
// TODO: this assumption may be incorrect. Without further examples
// of all kind of copybook statements, the behavior cannot be certain
//
// Multi-line:
// 	000420             15  DUMMY-5  REDEFINES                 00000142
// 	000420                 DUMMY-4  PIC XX.                   00000143
func (d *decoder) multiLineRedefinedRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != multiLineRedefinesSplitSize {
		return nil, errors.New("multiLineRedefinedRecord: does not match expected length/format")
	}

	if ok := d.s.Scan(); !ok {
		d.done = true
		return nil, nil
	}

	replace := string(d.s.Bytes())
	r, err := d.findRedefinesTarget(replace)
	if err != nil {
		return nil, err
	}

	want := *r
	want.Name = ss[2]
	d.cache.Delete(r.Name)
	return d.toCache(&want), nil
}

// redefinedRecord locates a replacement record and removes it
// based on the assumption that intra-line REDEFINES statements
// are followed by the definition of a subgroup, which wholly replaces
// the target picture definition
//
// TODO: this assumption may be incorrect. Without further examples
// of all kind of copybook statements, the behavior cannot be certain
//
// Intra-line:
// 	000590     05  DUMMY-3  REDEFINES  DUMMY-2. 00000166
func (d *decoder) redefinedRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")

	if len(ss) != redefinesSplitSize {
		return nil, errors.New("redefinedRecord: does not match expected length/format")
	}

	replace := ss[4]

	if r := d.fromCache(strings.TrimSuffix(replace, ".")); r != nil {
		return r, nil
	}

	return nil, fmt.Errorf("REDEFINES %s target is not present", replace)
}

// occursRecord accepts a line that is suspected to be an OCCURS statement that is defined on a single line.
// The length of string elements is validated, and subsequently used to build a record from the line elements.
//
// Intra-line:
// 	001350           15  DUMMY-1 PIC X  OCCURS 12.       00000247
func (d *decoder) occursRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != occursSplitSize {
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

	rec := &copybook.Record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: t,
		Length:  size,
		Occurs:  occurs,
	}

	return d.toCache(rec), nil
}

// multiLineOccursRecord accepts a line that is suspected to be an OCCURS statement
// that has been broken across multiple lines. multiLineOccursRecord validates the length
// of string elements, then scans ahead to the next line and checks for an OCCURS definition,
// validates the new line and builds a record from the PIC defined on the first line, and
// the OCCURS count on the second.
//
// Multi-line:
// 	001290           15  DUMMY-1 PIC X(12)               00000241
// 	001300               OCCURS 12.                      00000242
func (d *decoder) multiLineOccursRecord(line string) (*copybook.Record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != multiLineOccursSplitSize {
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
	if len(occStr) != incompletePICSplitSize {
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

	rec := &copybook.Record{
		Num:     num,
		Level:   lvl,
		Name:    ss[2],
		Picture: t,
		Length:  size,
		Occurs:  occurs,
	}

	return d.toCache(rec), nil
}
