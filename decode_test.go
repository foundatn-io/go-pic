package pic

import (
	"errors"
	"fmt"
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"
)

// ---------- Unmarshal: omit tag -----------------------------------------------

func TestUnmarshal_Omit(t *testing.T) {
	t.Parallel()
	type stuff struct {
		String string  `pic:"1,5"`
		Int    int     `pic:"-"`
		Float  float64 `pic:"11,15"`
	}
	in := &stuff{}
	require.NoError(t, Unmarshal([]byte("foo  123  1.2  "), in))
	require.Equal(t, &stuff{String: "foo", Int: 0, Float: 1.2}, in)
}

func TestUnmarshal_OmitArray(t *testing.T) {
	t.Parallel()
	type stuff struct {
		String string  `pic:"1,5"`
		Int    []int   `pic:"-"`
		Float  float64 `pic:"11,15"`
	}
	in := &stuff{}
	require.NoError(t, Unmarshal([]byte("foo  123  1.2  "), in))
	require.Equal(t, &stuff{String: "foo", Int: nil, Float: 1.2}, in)
}

func TestUnmarshal_OmitStruct(t *testing.T) {
	t.Parallel()
	type abcde struct {
		A int `pic:"1,1"`
		B int `pic:"2,2"`
		C int `pic:"3,3"`
		D int `pic:"4,4"`
		E int `pic:"5,5"`
	}
	type stuff struct {
		String string  `pic:"1,5"`
		Int    abcde   `pic:"-"`
		Float  float64 `pic:"11,15"`
	}
	in := &stuff{}
	require.NoError(t, Unmarshal([]byte("foo  123  1.2  "), in))
	require.Equal(t, &stuff{String: "foo", Int: abcde{}, Float: 1.2}, in)
}

// ---------- Unmarshal: scalar and pointer coverage ----------------------------

func TestUnmarshal_AllScalarTypes(t *testing.T) {
	t.Parallel()

	t.Run("uint field", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N uint `pic:"1,5"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("00042"), got))
		require.Equal(t, uint(42), got.N)
	})

	t.Run("uint empty source leaves field unchanged", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N uint `pic:"1,5"`
		}
		// An all-whitespace field trims to "" — the setter returns early without
		// modifying the target, so the initial zero value is preserved.
		got := &s{}
		require.NoError(t, Unmarshal([]byte("     "), got))
		require.Equal(t, uint(0), got.N)
	})

	t.Run("int negative", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N int `pic:"1,4"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("-042"), got))
		require.Equal(t, -42, got.N)
	})

	t.Run("float32 field", func(t *testing.T) {
		t.Parallel()
		type s struct {
			F float32 `pic:"1,4"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("3.14"), got))
		require.InDelta(t, float32(3.14), got.F, 0.01)
	})

	t.Run("bool truthy values", func(t *testing.T) {
		t.Parallel()
		type s struct {
			A bool `pic:"1,1"`
			B bool `pic:"2,2"`
			C bool `pic:"3,3"`
			D bool `pic:"4,7"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("YT1TRUE"), got))
		require.Equal(t, &s{A: true, B: true, C: true, D: true}, got)
	})

	t.Run("bool falsy values", func(t *testing.T) {
		t.Parallel()
		type s struct {
			A bool `pic:"1,1"`
			B bool `pic:"2,2"`
			C bool `pic:"3,3"`
			D bool `pic:"4,8"`
		}
		got := &s{A: true, B: true, C: true, D: true}
		require.NoError(t, Unmarshal([]byte("NF0FALSE"), got))
		require.Equal(t, &s{}, got)
	})

	t.Run("bool empty source stays false", func(t *testing.T) {
		t.Parallel()
		type s struct {
			F bool `pic:"1,1"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte(" "), got))
		require.False(t, got.F)
	})

	t.Run("bool invalid value returns error", func(t *testing.T) {
		t.Parallel()
		type s struct {
			F bool `pic:"1,1"`
		}
		err := Unmarshal([]byte("Z"), &s{})
		require.Error(t, err)
		require.Contains(t, err.Error(), "cannot convert")
	})

	t.Run("pointer to string populated", func(t *testing.T) {
		t.Parallel()
		type s struct {
			P *string `pic:"1,5"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("hello"), got))
		require.NotNil(t, got.P)
		require.Equal(t, "hello", *got.P)
	})

	t.Run("pointer empty source stays nil", func(t *testing.T) {
		t.Parallel()
		type s struct {
			P *string `pic:"1,3"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("   "), got))
		require.Nil(t, got.P)
	})

	t.Run("interface{} field", func(t *testing.T) {
		t.Parallel()
		val := "placeholder"
		type s struct {
			V interface{} `pic:"1,5"`
		}
		got := &s{V: val}
		require.NoError(t, Unmarshal([]byte("world"), got))
		require.Equal(t, "world", got.V)
	})
}

// ---------- Unmarshal: error paths --------------------------------------------

func TestUnmarshal_Errors(t *testing.T) {
	t.Parallel()

	t.Run("NilTarget", func(t *testing.T) {
		t.Parallel()
		require.ErrorIs(t, Unmarshal([]byte("x"), nil), ErrNotAPointer)
	})

	t.Run("NotPointer_struct", func(t *testing.T) {
		t.Parallel()
		require.ErrorIs(t, Unmarshal([]byte("x"), struct{}{}), ErrNotAPointer)
	})

	t.Run("NilPointer", func(t *testing.T) {
		t.Parallel()
		var p *struct{}
		require.ErrorIs(t, Unmarshal([]byte("x"), p), ErrNilPointer)
	})

	t.Run("EOF_on_empty_input", func(t *testing.T) {
		t.Parallel()
		err := Unmarshal([]byte(""), &struct{}{})
		require.Error(t, err)
	})

	t.Run("bad_int_field", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N int `pic:"1,3"`
		}
		require.Error(t, Unmarshal([]byte("nan"), &s{}))
	})

	t.Run("bad_uint_field", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N uint `pic:"1,3"`
		}
		require.Error(t, Unmarshal([]byte("nan"), &s{}))
	})

	t.Run("bad_float_field", func(t *testing.T) {
		t.Parallel()
		type s struct {
			F float64 `pic:"1,3"`
		}
		// "nan" is accepted by strconv.ParseFloat; use letters-only to trigger error.
		require.Error(t, Unmarshal([]byte("abc"), &s{}))
	})

	t.Run("bad_tag_single_non_dash_field_silently_skipped", func(t *testing.T) {
		t.Parallel()
		// A single-element tag that is not "-" causes a parse error stored on
		// the field representation, but Unmarshal does NOT surface it — the
		// field is silently skipped and left at its zero value.
		type s struct {
			F string `pic:"5"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("hello"), got))
		require.Empty(t, got.F)
	})

	t.Run("bad_tag_end_before_start_field_silently_skipped", func(t *testing.T) {
		t.Parallel()
		// Same rationale: tag validation errors on a field are stored but not
		// propagated — the field is skipped.
		type s struct {
			F string `pic:"5,1"`
		}
		got := &s{}
		require.NoError(t, Unmarshal([]byte("hello"), got))
		require.Empty(t, got.F)
	})

	t.Run("decodeLines_error_mid_slice", func(t *testing.T) {
		t.Parallel()
		type s struct {
			N int `pic:"1,3"`
		}
		require.Error(t, Unmarshal([]byte("001\nnan"), &[]s{}))
	})

	t.Run("unsupported_type_chan", func(t *testing.T) {
		t.Parallel()
		type s struct {
			C chan int `pic:"1,1"`
		}
		require.Error(t, Unmarshal([]byte("1"), &s{}))
	})
}

// ---------- Unmarshal: table-driven main tests --------------------------------

func TestUnmarshal(t *testing.T) {
	t.Parallel()

	type basicTypes struct {
		String string  `pic:"1,5"`
		Int    int     `pic:"6,10"`
		Float  float64 `pic:"11,15"`
	}

	type occursOffset struct {
		A string   `pic:"1,13"`
		B string   `pic:"14,26"`
		C string   `pic:"27,39"`
		D string   `pic:"40,52"`
		E string   `pic:"53,65"`
		F string   `pic:"66,78"`
		G string   `pic:"79,80"`
		H []string `pic:"81,236,12"`
	}

	type C struct {
		CA []string `pic:"1,5,5"`
	}
	type D struct {
		DA string `pic:"1,2"`
	}
	type C2 struct {
		CA []string `pic:"1,5,5"`
		CB D        `pic:"6,7"`
	}
	type nestedStruct struct {
		A string `pic:"1,13"`
		B string `pic:"14,26"`
		C C      `pic:"27,32"`
	}
	type multiNestedStruct struct {
		A string `pic:"1,13"`
		B string `pic:"14,26"`
		C C2     `pic:"27,33"`
	}

	for _, test := range []struct {
		name     string
		val      []byte
		target   interface{}
		expected interface{}
		err      error
	}{
		{
			name:   "SliceCase_noTrailingNewline",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  "),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
		}, {
			name:   "SliceCase_trailingNewline",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
		}, {
			name:   "SliceCase_blankLineMidFile",
			val:    []byte("foo  123  1.2  " + "\n" + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"", 0, 0},
				{"bar", 321, 2.1},
			},
		}, {
			name:     "BasicStruct",
			val:      []byte("foo  123  1.2  "),
			target:   &basicTypes{},
			expected: &basicTypes{"foo", 123, 1.2},
		}, {
			name:     "UnmarshalError_badTypes",
			val:      []byte("foo  nan  ddd  "),
			target:   &basicTypes{},
			expected: &basicTypes{},
			err:      fmt.Errorf("unmarshal: pic: cannot unmarshal foo  nan  ddd   into Go struct field basicTypes.Int of type int: failed string->int conversion: strconv.Atoi: parsing \"nan\": invalid syntax"),
		}, {
			name:     "EmptyLine_EOF",
			val:      []byte(""),
			target:   &basicTypes{},
			expected: &basicTypes{},
			err:      fmt.Errorf("unmarshal: EOF"),
		}, {
			name:     "InvalidTarget_notPointer",
			val:      []byte("foo  123  1.2  "),
			target:   basicTypes{},
			expected: basicTypes{},
			err:      fmt.Errorf("unmarshal: decode: unmarshal target object is not a pointer"),
		}, {
			name:   "OccursOffset",
			val:    []byte("000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 00000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 000000000.00 "),
			target: &occursOffset{},
			expected: &occursOffset{
				A: "000000000.00", B: "000000000.00", C: "000000000.00",
				D: "000000000.00", E: "000000000.00", F: "000000000.00",
				G: "00",
				H: []string{
					"000000000.00", "000000000.00", "000000000.00",
					"000000000.00", "000000000.00", "000000000.00",
					"000000000.00", "000000000.00", "000000000.00",
					"000000000.00", "000000000.00", "000000000.00",
				},
			},
		}, {
			name:   "NestedStruct",
			val:    []byte("thirteen13131thirteen13131ABCDE"),
			target: &nestedStruct{},
			expected: &nestedStruct{
				A: "thirteen13131",
				B: "thirteen13131",
				C: C{CA: []string{"A", "B", "C", "D", "E"}},
			},
		}, {
			name:   "MultiNestedStruct",
			val:    []byte("thirteen13131thirteen13131ABCDEAA"),
			target: &multiNestedStruct{},
			expected: &multiNestedStruct{
				A: "thirteen13131",
				B: "thirteen13131",
				C: C2{CA: []string{"A", "B", "C", "D", "E"}, CB: D{DA: "AA"}},
			},
		},
	} {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			err := Unmarshal(tt.val, tt.target)
			if tt.err != nil || err != nil {
				require.EqualError(t, err, tt.err.Error())
			} else {
				require.Equal(t, tt.expected, tt.target)
			}
		})
	}
}

// ---------- newValFromLine ----------------------------------------------------

func TestNewValFromLine(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name       string
		line       string
		start, end int
		want       string
	}{
		{"empty line", "", 1, 5, ""},
		{"start beyond length", "abc", 10, 15, ""},
		{"end clipped to length", "hello", 1, 100, "hello"},
		{"normal extract", "hello world", 7, 11, "world"},
		{"single char", "abc", 2, 2, "b"},
		{"trims spaces", "  hi  ", 1, 6, "hi"},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, newValFromLine(tt.line, tt.start, tt.end))
		})
	}
}

// ---------- UnmarshalTypeError ------------------------------------------------

func TestUnmarshalTypeError_Error(t *testing.T) {
	t.Parallel()
	intT := reflect.TypeOf(0)

	t.Run("with struct and field", func(t *testing.T) {
		t.Parallel()
		e := &UnmarshalTypeError{
			Value:  "bad",
			Type:   intT,
			Struct: "MyStruct",
			Field:  "MyField",
		}
		require.Contains(t, e.Error(), "Go struct field MyStruct.MyField")
	})

	t.Run("without struct or field", func(t *testing.T) {
		t.Parallel()
		e := &UnmarshalTypeError{
			Value: "bad",
			Type:  intT,
		}
		require.Contains(t, e.Error(), "Go value of type")
		require.NotContains(t, e.Error(), "struct field")
	})

	t.Run("cause is propagated via Unwrap", func(t *testing.T) {
		t.Parallel()
		cause := errors.New("root cause")
		e := &UnmarshalTypeError{Cause: cause}
		require.ErrorIs(t, e, cause)
		require.Contains(t, e.Error(), "root cause")
	})

	t.Run("no cause omits nested error text", func(t *testing.T) {
		t.Parallel()
		e := &UnmarshalTypeError{Value: "v", Type: intT}
		msg := e.Error()
		require.Contains(t, msg, "v")
		require.Contains(t, msg, "int")
		// Without a Cause the message ends after the type — no extra ": <cause>" suffix.
		require.NotContains(t, msg, "root cause")
	})
}

// ---------- slice tags --------------------------------------------------------

func TestUnmarshal_SliceWithoutOccursErrors(t *testing.T) {
	t.Parallel()
	// A slice field must carry an occurs count (the 3rd tag element); without it
	// there is no element size, which must surface as an error rather than a
	// divide-by-zero panic.
	type s struct {
		Nums []int `pic:"1,6"`
	}
	err := Unmarshal([]byte("010203"), &s{})
	require.Error(t, err)
	require.Contains(t, err.Error(), "occurs count")
}

// ---------- parseTag ----------------------------------------------------------

func Test_parseTag(t *testing.T) {
	t.Parallel()

	t.Run("skip tag", func(t *testing.T) {
		t.Parallel()
		got, err := parseTag("-")
		require.NoError(t, err)
		require.True(t, got.skip)
	})

	t.Run("start,end", func(t *testing.T) {
		t.Parallel()
		got, err := parseTag("3,7")
		require.NoError(t, err)
		require.Equal(t, 3, got.start)
		require.Equal(t, 7, got.end)
		require.Equal(t, 5, got.length)
	})

	t.Run("start,end,occurs", func(t *testing.T) {
		t.Parallel()
		got, err := parseTag("1,6,3")
		require.NoError(t, err)
		require.Equal(t, 3, got.occurs)
	})

	t.Run("single non-dash element errors", func(t *testing.T) {
		t.Parallel()
		_, err := parseTag("5")
		require.Error(t, err)
	})

	t.Run("too many elements errors", func(t *testing.T) {
		t.Parallel()
		_, err := parseTag("1,2,3,4")
		require.Error(t, err)
		require.Contains(t, err.Error(), "elements")
	})

	t.Run("non-positive occurs errors", func(t *testing.T) {
		t.Parallel()
		_, err := parseTag("1,6,0")
		require.Error(t, err)
	})

	t.Run("end before start errors", func(t *testing.T) {
		t.Parallel()
		_, err := parseTag("7,3")
		require.Error(t, err)
	})
}
