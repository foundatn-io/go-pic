package pic

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"sync"
)

const (
	// Tag values are accepted in the following format:
	// `pic:"-"` // ignore this field
	// `pic:"1,2"` // start:1, end:2 (length 2)
	// `pic:"1,2,2"` // start:1, end:2 (size 1x2)
	tag          = "pic"
	tagSeparator = ","
	ignoreTag    = "-"

	omitIndicator   = 1
	simpleIndicator = 2
	occursIndicator = 3
)

var fieldRepCache sync.Map // map[reflect.Type]structRepresentation

type structRepresentation struct {
	len    int
	fields []fieldRepresentation
}

// fieldRepresentation is a collection of a field/property's tag values,
// relevant determined setFunc and any error collected when building the
// representation
type fieldRepresentation struct {
	setFunc setFunc
	tag     tagRepresentation
	err     error
}

// tagRepresentation is a collection of all tag values associated with a field/
// property
type tagRepresentation struct {
	start  int
	end    int
	occurs int
	length int
	skip   bool
}

// parseTag accepts the values associated with the `pic` tag keyword, splits the
// values based on the accepted comma separator, and unpacks accepted tag formats
// into valid behaviors
func parseTag(tag string) (*tagRepresentation, error) {
	tagVals := &tagRepresentation{}
	ss := strings.Split(tag, tagSeparator)
	switch elemCount := len(ss); elemCount {
	case omitIndicator:
		if ss[0] != ignoreTag {
			return nil, fmt.Errorf("single pic tag value provided, expected %s", ignoreTag)
		}
		tagVals.skip = true
	case simpleIndicator:
		var err error
		tagVals.start, tagVals.end, tagVals.length, err = getSpread(ss)
		if err != nil {
			return nil, err
		}

	case occursIndicator:
		o, err := strconv.Atoi(ss[2])
		if err != nil {
			return nil, fmt.Errorf("failed string->int conversion: %w", err)
		}
		tagVals.occurs = o

		tagVals.start, tagVals.end, tagVals.length, err = getSpread(ss)
		if err != nil {
			return nil, err
		}
	}

	return tagVals, nil
}

// getSpread returns the start, end, and length of an evaluated tag's values
func getSpread(ss []string) (int, int, int, error) {
	start, err := strconv.Atoi(ss[0])
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed string->int conversion: %w", err)
	}

	end, err := strconv.Atoi(ss[1])
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed string->int conversion: %w", err)
	}

	length := end - (start - 1)
	return start, end, length, nil
}

func makeStructRepresentation(t reflect.Type) structRepresentation {
	sr := structRepresentation{
		fields: make([]fieldRepresentation, t.NumField()),
	}

	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
		tagVals, err := parseTag(f.Tag.Get(tag))
		sr.fields[i].tag = *tagVals

		if tagVals.skip {
			sr.fields[i].setFunc = skipSetFunc
		} else {
			sr.fields[i].setFunc = newSetFunc(f.Type, tagVals.length, tagVals.occurs)
		}

		if sr.fields[i].tag.end > sr.len {
			sr.len = sr.fields[i].tag.end
		}
		sr.fields[i].err = err
	}

	return sr
}

// cachedStructRepresentation is like makeStructRepresentation but cached to
// prevent duplicate work.
func cachedStructRepresentation(t reflect.Type) structRepresentation {
	if f, ok := fieldRepCache.Load(t); ok {
		return f.(structRepresentation)
	}

	f, _ := fieldRepCache.LoadOrStore(t, makeStructRepresentation(t))
	return f.(structRepresentation)
}
