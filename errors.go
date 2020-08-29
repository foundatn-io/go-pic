package pic

import "reflect"

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
	var s string
	if e.Struct != "" || e.Field != "" {
		s = "pic: cannot unmarshal " + e.Value + " into Go struct field " + e.Struct + "." + e.Field + " of type " + e.Type.String()
	} else {
		s = "pic: cannot unmarshal " + e.Value + " into Go value of type " + e.Type.String()
	}
	if e.Cause != nil {
		return s + ":" + e.Cause.Error()
	}
	return s
}
