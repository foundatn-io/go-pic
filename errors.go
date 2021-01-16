package pic

import (
	"fmt"
	"reflect"
)

// UnmarshalTypeError represents an unmarshal malfunction
type UnmarshalTypeError struct {
	Value  string       // raw value
	Type   reflect.Type // type of Go value it could not be assigned to
	Struct string       // name of the struct type containing the field
	Field  string       // name of the field holding the Go value
	Cause  error        // original error
}

// Error converts details of an UnmarshalTypeError into a meaningful string
func (e *UnmarshalTypeError) Error() string {
	var err error
	if e.Struct != "" || e.Field != "" {
		err = fmt.Errorf("pic: cannot unmarshal %s into Go struct field %s.%s of type %s", e.Value, e.Struct, e.Field, e.Type.String())
	} else {
		err = fmt.Errorf("pic: cannot unmarshal %s into Go value of type %s", e.Value, e.Type.String())
	}

	if e.Cause != nil {
		return fmt.Errorf("%s: %w", err.Error(), e.Cause).Error() // nolint:errorlint // can't use two %w directives
	}

	return err.Error()
}
