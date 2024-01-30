package pic

import (
	"errors"
	"fmt"
	"reflect"
	"strconv"
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
func newSetValueFunc(targetType reflect.Type, picSize, occursSize int) setValueFunc {
	switch targetType.Kind() {
	case reflect.String:
		return stringSetValueFunc
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
	case reflect.Ptr:
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

// intSetValueFunc is a setValueFunc that converts the source string to an int
// and sets the target to the resulting value.
func intSetValueFunc(target reflect.Value, source string) error {
	if len(source) < 1 {
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
	if len(source) < 1 {
		return nil
	}
	uintValue, err := strconv.ParseUint(source, 10, 64)
	if err != nil {
		return fmt.Errorf("failed string->int conversion: %w", err)
	}
	target.SetUint(uintValue)
	return nil
}

// floatSetValueFunc returns a setValueFunc that converts the source string to a float
// of the given bitSize and sets the target to the resulting value.
func floatSetValueFunc(bitSize int) setValueFunc {
	return func(target reflect.Value, source string) error {
		if len(source) < 1 {
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
		itemSize := length / itemCount
		if len(source) == 0 {
			return nilSetValueFunc(target, source)
		}

		if target.IsNil() {
			target.Set(reflect.MakeSlice(target.Type(), 0, 0))
		}

		newArray := reflect.MakeSlice(target.Type(), itemCount, itemCount)
		setValueFunction := newSetValueFunc(target.Type().Elem(), 0, 0)
		currentIndex := 1
		for i := 0; i < itemCount; i++ {
			nextIndex := currentIndex + itemSize
			value := newValFromLine(source, currentIndex, nextIndex-1)
			if err := setValueFunction(newArray.Index(i), value); err != nil {
				return fmt.Errorf("failed to set array data: %s %s", value, source)
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
		if len(source) == 0 {
			return nilSetValueFunc(target, source)
		}
		if target.IsNil() {
			target.Set(reflect.New(targetType.Elem()))
		}
		return innerSetter(reflect.Indirect(target), source)
	}
}

// interfaceSetValueFunc is a setValueFunc that sets the target to a new value
// derived from the source string. The target must be settable and its underlying
// value must have a type that is compatible with the source string.
func interfaceSetValueFunc(target reflect.Value, source string) error {
	return newSetValueFunc(target.Elem().Type(), 0, 0)(target.Elem(), source)
}

// structSetValueFunc returns a setValueFunc that sets the fields of the target
// to values derived from the source string. The source string is divided into
// items of equal size, and each item is set on the corresponding field in the target struct.
func structSetValueFunc(targetType reflect.Type) setValueFunc {
	spec := cachedStructRepresentation(targetType)
	return func(target reflect.Value, source string) error {
		for i, fieldFunc := range spec.fields {
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
