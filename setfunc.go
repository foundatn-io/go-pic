package pic

import (
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"
)

// Constants for float bit sizes.
const (
	float32BitSize = 32
	float64BitSize = 64
)

// setValueFunc is a function type that sets a value on a target reflect.Value
// from a source string. It returns an error if the setting of the value fails.
type setValueFunc func(target reflect.Value, source string) error

// newSetValueFunc returns a setValueFunc appropriate for the given targetType.
// picSize and occursSize are used for slice types to determine the size of the slice.
func newSetValueFunc(targetType reflect.Type, picSize, occursSize int) setValueFunc { //nolint:gocyclo // one arm per reflect.Kind
	switch targetType.Kind() {
	case reflect.String:
		return stringSetValueFunc
	case reflect.Bool:
		return boolSetValueFunc
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return intSetValueFunc
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return uintSetValueFunc
	case reflect.Float32:
		return floatSetValueFunc(float32BitSize)
	case reflect.Float64:
		return floatSetValueFunc(float64BitSize)
	case reflect.Slice:
		return arraySetValueFunc(picSize, occursSize)
	case reflect.Pointer:
		return pointerSetValueFunc(targetType)
	case reflect.Interface:
		return interfaceSetValueFunc
	case reflect.Struct:
		return structSetValueFunc(targetType)
	}
	return failSetValueFunc
}

// skipSetValueFunc is a setValueFunc that does nothing and returns no error.
func skipSetValueFunc(_ reflect.Value, _ string) error {
	return nil
}

// failSetValueFunc is a setValueFunc that always returns an error.
func failSetValueFunc(_ reflect.Value, _ string) error {
	return errors.New("pic: unknown type")
}

// nilSetValueFunc is a setValueFunc that sets the target to its zero value.
func nilSetValueFunc(target reflect.Value, _ string) error {
	target.Set(reflect.Zero(target.Type()))
	return nil
}

// stringSetValueFunc is a setValueFunc that sets the target to the source string.
func stringSetValueFunc(target reflect.Value, source string) error {
	target.SetString(source)
	return nil
}

// boolSetValueFunc converts common COBOL boolean representations to Go bool.
// Truthy:  "Y", "y", "T", "t", "1", "true", "TRUE", "True"
// Falsy:   "N", "n", "F", "f", "0", "false", "FALSE", "False"
// Empty source is treated as false (zero value).
func boolSetValueFunc(target reflect.Value, source string) error {
	if source == "" {
		return nil
	}
	switch strings.ToUpper(strings.TrimSpace(source)) {
	case "Y", "T", "1", "TRUE":
		target.SetBool(true)
	case "N", "F", "0", "FALSE":
		target.SetBool(false)
	default:
		return fmt.Errorf("cannot convert %q to bool: expected Y/N, T/F, 1/0, or true/false", source)
	}
	return nil
}

// intSetValueFunc is a setValueFunc that converts the source string to an int
// and sets the target to the resulting value.
func intSetValueFunc(target reflect.Value, source string) error {
	if source == "" {
		return nil
	}
	intValue, err := strconv.Atoi(source)
	if err != nil {
		return fmt.Errorf("failed string->int conversion: %w", err)
	}
	target.SetInt(int64(intValue))
	return nil
}

// uintSetValueFunc is a setValueFunc that converts the source string to a uint
// and sets the target to the resulting value.
func uintSetValueFunc(target reflect.Value, source string) error {
	if source == "" {
		return nil
	}
	uintValue, err := strconv.ParseUint(source, 10, 64)
	if err != nil {
		return fmt.Errorf("failed string->uint conversion: %w", err)
	}
	target.SetUint(uintValue)
	return nil
}

// floatSetValueFunc returns a setValueFunc that converts the source string to a float
// of the given bitSize and sets the target to the resulting value.
func floatSetValueFunc(bitSize int) setValueFunc {
	return func(target reflect.Value, source string) error {
		if source == "" {
			return nil
		}
		floatValue, err := strconv.ParseFloat(source, bitSize)
		if err != nil {
			return fmt.Errorf("failed string->float64 conversion: %w", err)
		}
		target.SetFloat(floatValue)
		return nil
	}
}

// arraySetValueFunc returns a setValueFunc that sets the target to a slice of values
// derived from the source string. The source string is divided into items of equal size,
// and each item is set on the corresponding index in the target slice.
func arraySetValueFunc(length, itemCount int) setValueFunc {
	return func(target reflect.Value, source string) error {
		if source == "" {
			return nilSetValueFunc(target, source)
		}
		if itemCount <= 0 {
			return fmt.Errorf("slice field requires a positive occurs count (3rd pic tag element), got %d", itemCount)
		}
		itemSize := length / itemCount
		newArray := reflect.MakeSlice(target.Type(), itemCount, itemCount)
		setValueFunction := newSetValueFunc(target.Type().Elem(), 0, 0)
		currentIndex := 1
		for i := 0; i < itemCount; i++ {
			nextIndex := currentIndex + itemSize
			value := newValFromLine(source, currentIndex, nextIndex-1)
			if err := setValueFunction(newArray.Index(i), value); err != nil {
				return fmt.Errorf("failed to set array element %d (value %q): %w", i, value, err)
			}
			currentIndex = nextIndex
		}
		target.Set(newArray)
		return nil
	}
}

// pointerSetValueFunc returns a setValueFunc that sets the target to a new value
// derived from the source string. If the target is nil, a new value of the appropriate
// type is allocated and set on the target.
func pointerSetValueFunc(targetType reflect.Type) setValueFunc {
	innerSetter := newSetValueFunc(targetType.Elem(), 0, 0)
	return func(target reflect.Value, source string) error {
		if source == "" {
			return nilSetValueFunc(target, source)
		}
		if target.IsNil() {
			target.Set(reflect.New(targetType.Elem()))
		}
		return innerSetter(reflect.Indirect(target), source)
	}
}

// interfaceSetValueFunc sets the concrete value stored inside an interface field.
// target.Elem() returns the concrete value but it is not addressable, so we
// allocate a fresh addressable value of the same type, set it, then store it
// back into the interface.
func interfaceSetValueFunc(target reflect.Value, source string) error {
	if target.IsNil() {
		return nil
	}
	inner := target.Elem()
	newVal := reflect.New(inner.Type()).Elem()
	if err := newSetValueFunc(inner.Type(), 0, 0)(newVal, source); err != nil {
		return err
	}
	target.Set(newVal)
	return nil
}

// structSetValueFunc returns a setValueFunc that sets the fields of the target
// to values derived from the source string. The source string is divided into
// items of equal size, and each item is set on the corresponding field in the target struct.
func structSetValueFunc(targetType reflect.Type) setValueFunc {
	spec := cachedStructRepresentation(targetType)
	return func(target reflect.Value, source string) error {
		for i, fieldFunc := range spec.fields {
			// Fields without a usable pic tag (e.g. untagged helper fields)
			// fail tag parsing and are intentionally left at their zero value
			// rather than aborting the whole decode.
			if fieldFunc.err != nil {
				continue
			}
			var value string
			if !fieldFunc.tag.skip {
				value = newValFromLine(source, fieldFunc.tag.start, fieldFunc.tag.end)
			}
			if err := fieldFunc.setFunc(target.Field(i), value); err != nil {
				structField := targetType.Field(i)
				return &UnmarshalTypeError{source, structField.Type, targetType.Name(), structField.Name, err}
			}
		}
		return nil
	}
}
