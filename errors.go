package pic

import (
	"errors"
	"fmt"
	"reflect"
)

// ErrNotAPointer is returned when the target object is not a pointer.
var ErrNotAPointer = errors.New("decode: unmarshal target object is not a pointer")

// ErrNilPointer is returned when the target object is nil.
var ErrNilPointer = errors.New("decode: unmarshal target object is nil")

// UnmarshalTypeError describes a failure to unmarshal a value into a specific Go type.
type UnmarshalTypeError struct {
	Value  string       // raw value that could not be assigned
	Type   reflect.Type // target Go type
	Struct string       // containing struct name, if any
	Field  string       // containing field name, if any
	Cause  error        // underlying error
}

// Error returns a human-readable description of the unmarshal failure.
func (e *UnmarshalTypeError) Error() string {
	var msg string
	if e.Struct != "" || e.Field != "" {
		msg = fmt.Sprintf("pic: cannot unmarshal %s into Go struct field %s.%s of type %s",
			e.Value, e.Struct, e.Field, e.Type)
	} else {
		msg = fmt.Sprintf("pic: cannot unmarshal %s into Go value of type %s",
			e.Value, e.Type)
	}
	if e.Cause != nil {
		return msg + ": " + e.Cause.Error()
	}
	return msg
}

// Unwrap returns the underlying cause so errors.Is and errors.As can traverse the chain.
func (e *UnmarshalTypeError) Unwrap() error {
	return e.Cause
}
