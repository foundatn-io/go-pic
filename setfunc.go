package pic

import (
	"errors"
	"fmt"
	"log"
	"reflect"
	"strconv"
)

const (
	float32Size = 32
	float64Size = 64
)

type setFunc func(v reflect.Value, s string) error

// newSetFunc evaluates the Type of the value t and returns a relevant setFunc
// predetermined for that type. Params picSize & occursSize are passed through
// for arraySetFunc alone
func newSetFunc(t reflect.Type, picSize, occursSize int) setFunc {
	switch t.Kind() {
	case reflect.String:
		return strSetFunc
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return intSetFunc
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return uintSetFunc
	case reflect.Float32:
		return floatSetFunc(float32Size)
	case reflect.Float64:
		return floatSetFunc(float64Size)
	case reflect.Slice:
		return arraySetFunc(picSize, occursSize)
	case reflect.Ptr:
		return ptrSetFunc(t)
	case reflect.Interface:
		return ifaceSetFunc
	case reflect.Struct:
		return structSetFunc(t)
	}
	return failSetFunc
}

// skipSetFunc is provided as a setFunc for use when a field is tagged with the
// omit value "-"
func skipSetFunc(_ reflect.Value, _ string) error {
	log.Println("skipping value")
	return nil
}

// failSetFunc is provided as a setFunc for use when a fields type is not
// identified as valid, and will always return an error.
func failSetFunc(_ reflect.Value, _ string) error {
	return errors.New("pic: unknown type")
}

func nilSetFunc(v reflect.Value, _ string) error {
	v.Set(reflect.Zero(v.Type()))
	return nil
}

func strSetFunc(v reflect.Value, s string) error {
	v.SetString(s)
	return nil
}

func intSetFunc(v reflect.Value, s string) error {
	if len(s) < 1 {
		return nil
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		return fmt.Errorf("failed string->int conversion: %w", err)
	}

	v.SetInt(int64(i))
	return nil
}

func uintSetFunc(v reflect.Value, s string) error {
	if len(s) < 1 {
		return nil
	}

	i, err := strconv.ParseUint(s, 10, 64)
	if err != nil {
		return fmt.Errorf("failed string->int conversion: %w", err)
	}

	v.SetUint(i)
	return nil
}

func floatSetFunc(size int) setFunc {
	return func(v reflect.Value, s string) error {
		if len(s) < 1 {
			return nil
		}

		f, err := strconv.ParseFloat(s, size)
		if err != nil {
			return fmt.Errorf("failed string->float64 conversion: %w", err)
		}

		v.SetFloat(f)
		return nil
	}
}

// arraySetFunc is provided as a setFunc for use when reflection identifies the
// field as an array/slice. l represents total length of the array, count is the
// specified element count
func arraySetFunc(l, count int) setFunc {
	return func(v reflect.Value, s string) error {
		size := l / count
		if len(s) == 0 {
			return nilSetFunc(v, s)
		}

		if v.IsNil() {
			v.Set(reflect.MakeSlice(v.Type(), 0, 0))
		}

		newSlice := reflect.MakeSlice(v.Type(), count, count)
		sf := newSetFunc(v.Type().Elem(), 0, 0)
		track := 1

		for i := 0; i < count; i++ {
			next := track + size
			val := newValFromLine(s, track, next-1)

			if err := sf(newSlice.Index(i), val); err != nil {
				return errors.New("failed to set array data" + val + " " + s)
			}

			track = next
		}

		v.Set(newSlice)
		return nil
	}
}

func ptrSetFunc(t reflect.Type) setFunc {
	innerSetter := newSetFunc(t.Elem(), 0, 0)
	return func(v reflect.Value, s string) error {
		if len(s) == 0 {
			return nilSetFunc(v, s)
		}

		if v.IsNil() {
			v.Set(reflect.New(t.Elem()))
		}

		return innerSetter(reflect.Indirect(v), s)
	}
}

func ifaceSetFunc(v reflect.Value, s string) error {
	return newSetFunc(v.Elem().Type(), 0, 0)(v.Elem(), s)
}

func structSetFunc(t reflect.Type) setFunc {
	spec := cachedStructRepresentation(t)
	return func(v reflect.Value, s string) error {
		for i, ff := range spec.fields {
			if ff.err != nil {
				continue
			}

			var val string
			if !ff.tag.skip {
				val = newValFromLine(s, ff.tag.start, ff.tag.end)
			}

			err := ff.setFunc(v.Field(i), val)
			if err != nil {
				sf := t.Field(i)
				return &UnmarshalTypeError{s, sf.Type, t.Name(), sf.Name, err}
			}
		}
		return nil
	}
}
