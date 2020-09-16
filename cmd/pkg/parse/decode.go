package parse

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"sync"
)

type lineKind int

const (
	XXX lineKind = iota
	PIC
	PICIncomplete
	RedefinesMulti
	RedefinesSingle
)

type Decoder struct {
	s     *bufio.Scanner
	done  bool
	cache sync.Map
}

var (
	// ^NUM LEVEL NAME ( \. | PIC X~ | PIC X(9) | REDEFINES NAME)\. CLOSERNUM$
	// NUM 											[0-9]+
	// LEVEL 										[0-9]{2}
	// NAME 										[a-zA-Z0-9\-]+
	// Termination of clause, indicates group 		.
	// PIC X~ | PIC X(9) 							(PIC ([X9]+|[X9]\([0-9]+\))
	// REDEFINES NAME								REDEFINES +[a-zA-Z0-9\-]+
	// CLOSERNUM									0+[0-9]+

	// Defines picture clause
	// 000600         10  DUMMY-1       PIC X.                  00000167
	// 000620         10  DUMMY-2       PIC 9(7).               00000169
	picLine = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\))\. +0+[0-9]+$`)

	// Defines picture clause that deviates from typical pattern
	//          10  DUMMY-1       PIC X.                  00000167
	//          10  DUMMY-2       PIC 9(7).               00000169
	incompletePICLine = regexp.MustCompile(`[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\))\.`)

	// Matches same-line REDEFINES definitions
	// 000550     05  DUMMY-1  PIC X(340).             00000162
	// 000590     05  DUMMY-2  REDEFINES  DUMMY-1.     00000166
	redefinesLine = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +REDEFINES +[a-zA-Z0-9\-]+\. +0+[0-9]+$`)

	// Matches multi-line REDEFINES definitions
	// 000420             15  DUMMY-1  PIC XX.                 00000141
	// 000420             15  DUMMY-2  REDEFINES               00000142
	// 000420                 DUMMY-1                          00000143
	redefinesStatement = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +REDEFINES +0+[0-9]+$`)

	// OCCURS statements can be found in 3 different forms
	// Intra-line - 001350           15  DUMMY-1 PIC X  OCCURS 12.       00000247
	// Multi-line - 001290           15  DUMMY-1 PIC X(12)               00000241
	// 				001300               OCCURS 12.                      00000242
	// Verbose    - 001630           15  DUMMY-1 OCCURS 7 TIMES.         00000347
	occursLine      = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\))\. OCCURS ([0-9]+ TIMES\.|[0-9]+\.) +0+[0-9]+$`)
	occursStatement = regexp.MustCompile(`^[0-9]+ +OCCURS ([0-9]+ TIMES\.|[0-9]+\.)$`)
)

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

	*c = append(*c, rec)
	return true, nil
}

func getLineType(line string) lineKind {
	if redefinesStatement.MatchString(line) {
		return RedefinesMulti
	}

	if redefinesLine.MatchString(line) {
		return RedefinesSingle
	}

	if picLine.MatchString(line) {
		return PIC
	}

	if incompletePICLine.MatchString(line) {
		return PICIncomplete
	}

	return XXX
}

// findDataRecord accepts a string, line, representing a line in a copybook definition
// and attempts to parse the line, detecting whether it is a picture definition (PIC),
// picture redefinition (REDEFINES), or a repeated picture definition (OCCURS).
func (d *Decoder) findDataRecord(line string, c *Copybook) (*record, error) {
	line = trimExtraWhitespace(line)
	switch getLineType(line) {
	case PIC:
		return d.getPIC(line)
	case PICIncomplete:
		return d.getIncompletePIC(line)
	case RedefinesSingle:
		r, err := d.getIntraLineRedefines(line)
		if err != nil {
			return nil, err
		}

		if err := c.findAndRemove(r); err != nil {
			return nil, err
		}

		return nil, nil
	case RedefinesMulti:
		want, replace, err := d.getMultiLineRedefines(line)
		if err != nil {
			return nil, err
		}

		if err := c.findAndEdit(want, replace); err != nil {
			return nil, err
		}
	case XXX:
		return nil, nil
	}
	return nil, errors.New("matched as record, but record type unimplemented")
}

func findPICType(pic string) (reflect.Kind, error) {
	switch pic[0:1] {
	case "X":
		return reflect.String, nil
	case "9":
		return reflect.Int, nil
	}

	return reflect.Invalid, errors.New("unexpected PIC type")
}

func findPICCount(pic string) (int, error) {
	// X(2).
	// left = 1+1, right = 3 s[2:3], size = 2
	// XX.
	// XX, size = 2
	if strings.Contains(pic, "(") {
		leftPos := strings.Index(pic, "(") + 1 // FROM index, not including
		rightPos := strings.Index(pic, ")")
		s := pic[leftPos:rightPos]
		size, err := strconv.Atoi(s)
		if err != nil {
			return 0, err
		}

		return size, nil
	}

	s := strings.Trim(pic, ".")
	return len(s), nil
}

// Defines picture clause
// 000600         10  DUMMY-1       PIC X.                  00000167
// 000620         10  DUMMY-2        PIC 9(7).               00000169
//
// getPIC decodes the line as a picture definition, returns the record details
// and adds the picture to the PIC definition cache
func (d *Decoder) getPIC(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 6 {
		return nil, errors.New("getPIC: does not match expected length/format")
	}

	num, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	lvl, err := strconv.Atoi(ss[1])
	if err != nil {
		return nil, err
	}

	t, err := findPICType(ss[4])
	if err != nil {
		return nil, err
	}

	size, err := findPICCount(ss[4])
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

	return d.cacheRecord(rec), nil
}

func (d *Decoder) getIncompletePIC(line string) (*record, error) {
	ss := strings.Split(line, " ")
	if len(ss) != 4 {
		return nil, errors.New("getIncompletePIC: does not match expected length/format")
	}

	lvl, err := strconv.Atoi(ss[0])
	if err != nil {
		return nil, err
	}

	t, err := findPICType(ss[3])
	if err != nil {
		return nil, err
	}

	size, err := findPICCount(ss[3])
	if err != nil {
		return nil, err
	}

	rec := &record{
		Level:   lvl,
		Name:    ss[1],
		Picture: t,
		Length:  size,
	}

	return d.cacheRecord(rec), nil
}

// 000420                 X710203-DOCUMENT-ID-TIE  PIC XX.                 00000143
func (d *Decoder) getRedefinedPIC(line string) (*record, error) {
	line = trimExtraWhitespace(line)
	ss := strings.Split(line, " ")
	if len(ss) != 5 {
		return nil, errors.New("getRedefinedPIC: does not match expected length/format")
	}

	r := d.fetchFromCache(ss[1])
	if r == nil {
		return nil, fmt.Errorf("REDEFINES target %s not found", ss[1])
	}

	return r, nil
}

func (d *Decoder) cacheRecord(r *record) *record {
	if r, ok := d.cache.Load(r.Name); ok {
		return r.(*record)
	}

	rec, _ := d.cache.LoadOrStore(r.Name, r)
	return rec.(*record)
}

func (d *Decoder) fetchFromCache(name string) *record {
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
func (d *Decoder) getMultiLineRedefines(line string) (*record, *record, error) {
	ss := strings.Split(line, " ")

	if len(ss) != 5 {
		return nil, nil, errors.New("getMultiLineRedefines: does not match expected length/format")
	}

	if ok := d.s.Scan(); !ok {
		d.done = true
		return nil, nil, nil
	}

	replace := string(d.s.Bytes())
	r, err := d.getRedefinedPIC(replace)
	if err != nil {
		return nil, nil, err
	}

	want := *r
	want.Name = ss[2]
	return &want, r, nil
}

// getIntraLineRedefines locates a replacement record and removes it
// based on the assumption that intra-line REDEFINES statements
// are followed by the definition of a subgroup, which wholly replaces
// the target picture definition
//
// 000590     05  DUMMY-3  REDEFINES  DUMMY-2. 00000166
func (d *Decoder) getIntraLineRedefines(line string) (*record, error) {
	ss := strings.Split(line, " ")

	if len(ss) != 6 {
		return nil, errors.New("getIntraLineRedefines: does not match expected length/format")
	}

	replace := ss[4]

	if r := d.fetchFromCache(strings.TrimSuffix(replace, ".")); r != nil {
		return r, nil
	}

	return nil, fmt.Errorf("REDEFINES %s target is not present", replace)
}

func trimExtraWhitespace(in string) string {
	return strings.Trim(
		regexp.MustCompile(`\s+`).ReplaceAllString(in, " "),
		" ")
}
