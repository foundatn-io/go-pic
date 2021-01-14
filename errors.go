package pic

import (
	"fmt"
	"reflect"
)

type InvalidUnmarshalError struct {
	Type reflect.Type
}

func (e *InvalidUnmarshalError) Error() string {
	if e.Type == nil {
		return "pic: Unmarshal(nil)"
	}

	if e.Type.Kind() != reflect.Ptr {
		return "pic: Unmarshal(non-pointer " + e.Type.String() + ")"
	}
	return "pic: Unmarshal(nil " + e.Type.String() + ")"
}

type UnmarshalTypeError struct {
	Value  string       // raw value
	Type   reflect.Type // type of Go value it could not be assigned to
	Struct string       // name of the struct type containing the field
	Field  string       // name of the field holding the Go value
	Cause  error        // original error
}

func (e *UnmarshalTypeError) Error() string {
	var err error
	if e.Struct != "" || e.Field != "" {
		err = fmt.Errorf("pic: cannot unmarshal %s into Go struct field %s.%s of type %s", e.Value, e.Struct, e.Field, e.Type.String())
	} else {
		err = fmt.Errorf("pic: cannot unmarshal %s into Go value of type %s", e.Value, e.Type.String())
	}

	if e.Cause != nil {
		return fmt.Errorf("%s: %w", err, e.Cause).Error()
	}

	return err.Error()
}
