package pic

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"sync"
)

// Constants for tag parsing.
const (
	tagKey         = "pic"
	tagSeparator   = ","
	ignoreTagValue = "-"

	omitIndicator   = 1
	occursIndicator = 3
)

// structFieldCache is a cache for struct field representations.
var structFieldCache sync.Map // map[reflect.Type]structRepresentation

// structRepresentation holds the pre-computed layout of a struct's pic tags.
type structRepresentation struct {
	totalLength int
	fields      []fieldRepresentation
}

// fieldRepresentation holds the resolved setter and tag for one struct field.
type fieldRepresentation struct {
	setFunc setValueFunc
	tag     tagRepresentation
	err     error
}

// tagRepresentation holds the parsed values of a single pic struct tag.
type tagRepresentation struct {
	start  int
	end    int
	occurs int
	length int
	skip   bool
}

// parseTag parses a pic struct tag string.
//
// Valid forms:
//
//	"-"         → skip this field
//	"start,end" → 1-based inclusive byte range
//	"start,end,occurs" → as above, with a slice repetition count
func parseTag(tag string) (*tagRepresentation, error) {
	tagValues := &tagRepresentation{}
	splitTag := strings.Split(tag, tagSeparator)
	tagElements := len(splitTag)
	if tagElements > occursIndicator {
		return nil, fmt.Errorf("pic tag has %d elements; expected %q, \"start,end\", or \"start,end,occurs\"", tagElements, ignoreTagValue)
	}
	if tagElements == omitIndicator {
		if splitTag[0] != ignoreTagValue {
			return nil, fmt.Errorf("pic tag with a single element must be %q (got %q)", ignoreTagValue, splitTag[0])
		}
		tagValues.skip = true
		return tagValues, nil
	}
	if tagElements == occursIndicator {
		occurrences, err := strconv.Atoi(strings.TrimSpace(splitTag[2]))
		if err != nil {
			return nil, fmt.Errorf("pic tag occurs count is not an integer: %w", err)
		}
		if occurrences <= 0 {
			return nil, fmt.Errorf("pic tag occurs count must be > 0, got %d", occurrences)
		}
		tagValues.occurs = occurrences
	}
	var err error
	tagValues.start, tagValues.end, tagValues.length, err = getSpread(splitTag)
	if err != nil {
		return nil, err
	}
	return tagValues, nil
}

// getSpread parses the start and end positions from a split tag slice and
// validates that start ≤ end and both are ≥ 1.
func getSpread(splitTag []string) (start, end, length int, err error) {
	start, err = strconv.Atoi(strings.TrimSpace(splitTag[0]))
	if err != nil {
		return 0, 0, 0, fmt.Errorf("pic tag start position is not an integer: %w", err)
	}
	end, err = strconv.Atoi(strings.TrimSpace(splitTag[1]))
	if err != nil {
		return 0, 0, 0, fmt.Errorf("pic tag end position is not an integer: %w", err)
	}
	if start < 1 {
		return 0, 0, 0, fmt.Errorf("pic tag start position must be >= 1, got %d", start)
	}
	if end < start {
		return 0, 0, 0, fmt.Errorf("pic tag end position (%d) must be >= start position (%d)", end, start)
	}
	length = end - (start - 1)
	return start, end, length, nil
}

// makeStructRepresentation creates a structRepresentation for a given reflect.Type.
func makeStructRepresentation(structType reflect.Type) structRepresentation {
	structRep := structRepresentation{
		fields: make([]fieldRepresentation, structType.NumField()),
	}

	for fieldIndex := 0; fieldIndex < structType.NumField(); fieldIndex++ {
		field := structType.Field(fieldIndex)
		tagValues, err := parseTag(field.Tag.Get(tagKey))
		if err != nil {
			structRep.fields[fieldIndex].err = err
			continue
		}
		structRep.fields[fieldIndex].tag = *tagValues

		if tagValues.skip {
			structRep.fields[fieldIndex].setFunc = skipSetValueFunc
		} else {
			structRep.fields[fieldIndex].setFunc = newSetValueFunc(field.Type, tagValues.length, tagValues.occurs)
		}
		if structRep.fields[fieldIndex].tag.end > structRep.totalLength {
			structRep.totalLength = structRep.fields[fieldIndex].tag.end
		}
	}
	return structRep
}

// cachedStructRepresentation returns a structRepresentation for the given type,
// building and caching it on first use to avoid redundant reflection work.
func cachedStructRepresentation(structType reflect.Type) structRepresentation {
	if fieldRep, ok := structFieldCache.Load(structType); ok {
		return fieldRep.(structRepresentation)
	}
	fieldRep := makeStructRepresentation(structType)
	structFieldCache.Store(structType, fieldRep)
	return fieldRep
}
