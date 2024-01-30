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
	simpleIndicator = 2 //nolint:deadcode,varcheck // is unused but kept for clarity
	occursIndicator = 3
)

// structFieldCache is a cache for struct field representations.
var structFieldCache sync.Map // map[reflect.Type]structRepresentation

// structRepresentation represents the structure of a struct, including its length and fields.
type structRepresentation struct {
	totalLength int
	fields      []fieldRepresentation
}

// fieldRepresentation represents a field in a struct, including its set function, tag, and any errors.
type fieldRepresentation struct {
	setFunc setValueFunc
	tag     tagRepresentation
	err     error
}

// tagRepresentation represents a tag in a struct field, including its start, end, occurs, length, and skip values.
type tagRepresentation struct {
	start  int
	end    int
	occurs int
	length int
	skip   bool
}

// parseTag parses a tag string and returns a tagRepresentation and any error.
// The tag string should be in the format `pic:"-"` to ignore the field, or `pic:"1,2"` or `pic:"1,2,2"` to specify the start, end, and optionally occurs values.
func parseTag(tag string) (*tagRepresentation, error) {
	tagValues := &tagRepresentation{}
	splitTag := strings.Split(tag, tagSeparator)
	tagElements := len(splitTag)
	if tagElements == omitIndicator {
		if splitTag[0] != ignoreTagValue {
			return nil, fmt.Errorf("single pic tag value provided, expected %s", ignoreTagValue)
		}
		tagValues.skip = true
		return tagValues, nil
	}
	if tagElements == occursIndicator {
		occurrences, err := strconv.Atoi(splitTag[2])
		if err != nil {
			return nil, fmt.Errorf("failed string->int conversion: %w", err)
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

// getSpread returns the start, end, and length of a tag's values.
func getSpread(splitTag []string) (int, int, int, error) {
	start, err := strconv.Atoi(splitTag[0])
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed string->int conversion: %w", err)
	}
	end, err := strconv.Atoi(splitTag[1])
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed string->int conversion: %w", err)
	}
	length := end - (start - 1)
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

// cachedStructRepresentation creates a structRepresentation for a given reflect.Type and caches it to prevent duplicate work.
func cachedStructRepresentation(structType reflect.Type) structRepresentation {
	if fieldRep, ok := structFieldCache.Load(structType); ok {
		return fieldRep.(structRepresentation)
	}
	fieldRep := makeStructRepresentation(structType)
	structFieldCache.Store(structType, fieldRep)
	return fieldRep
}
