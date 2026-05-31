package pic

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestUnmarshal_Omit(t *testing.T) {
	t.Parallel()
	type stuff struct {
		String string  `pic:"1,5"`
		Int    int     `pic:"-"`
		Float  float64 `pic:"11,15"`
	}

	in := &stuff{}
	err := Unmarshal([]byte("foo  123  1.2  "), in)
	require.NoError(t, err)
	require.Equal(t, &stuff{
		String: "foo",
		Int:    0,
		Float:  1.2,
	}, in)
}

func TestUnmarshal_OmitArray(t *testing.T) {
	t.Parallel()
	type stuff struct {
		String string  `pic:"1,5"`
		Int    []int   `pic:"-"`
		Float  float64 `pic:"11,15"`
	}

	in := &stuff{}
	err := Unmarshal([]byte("foo  123  1.2  "), in)
	require.NoError(t, err)
	require.Equal(t, &stuff{
		String: "foo",
		Int:    nil,
		Float:  1.2,
	}, in)
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
	err := Unmarshal([]byte("foo  123  1.2  "), in)
	require.NoError(t, err)
	require.Equal(t, &stuff{
		String: "foo",
		Int:    abcde{},
		Float:  1.2,
	}, in)
}

func TestUnmarshal_Other(t *testing.T) {
	t.Parallel()

	t.Run("FieldLength1", func(t *testing.T) {
		t.Parallel()
		var st = struct {
			F1 string `pic:"1,1"`
		}{}
		require.NoError(t, Unmarshal([]byte("v"), &st))
		require.Equal(t, "v", st.F1)
	})

	t.Run("BasicOCCURS_primitives", func(t *testing.T) {
		t.Parallel()
		type basicWithOccurs struct {
			String    string `pic:"1,5"`
			Int       int    `pic:"6,10"`
			IntOccurs []int  `pic:"11,16,3"`
		}
		expect := &basicWithOccurs{"foo", 123, []int{12, 34, 56}}
		got := &basicWithOccurs{}
		require.NoError(t, Unmarshal([]byte("foo  123  123456"), got))
		require.Equal(t, expect, got)
	})

	t.Run("BasicOCCURS_structs", func(t *testing.T) {
		t.Parallel()
		type dummy struct {
			A int `pic:"1,1"`
			B int `pic:"2,2"`
		}
		type basicWithOccursStruct struct {
			String string  `pic:"1,5"`
			Int    int     `pic:"6,10"`
			Dummy  []dummy `pic:"11,16,3"`
		}
		expect := &basicWithOccursStruct{"foo", 123, []dummy{{A: 1, B: 2}, {A: 3, B: 4}, {A: 5, B: 6}}}
		got := &basicWithOccursStruct{}
		require.NoError(t, Unmarshal([]byte("foo  123  123456"), got))
		require.Equal(t, expect, got)
	})

	t.Run("InvalidUnmarshalErrors", func(t *testing.T) {
		t.Parallel()
		for _, test := range []struct {
			name      string
			v         interface{}
			shouldErr bool
		}{
			{"Nil", nil, true},
			{"NotPointer_struct", struct{}{}, true},
			{"NotPointer_slice", []struct{}{}, true},
			{"ValidSlice", &[]struct{}{}, false},
			{"ValidStruct_emptyInput", &struct{}{}, true}, // EOF on empty input
		} {
			tt := test
			t.Run(tt.name, func(t *testing.T) {
				t.Parallel()
				err := Unmarshal([]byte{}, tt.v)
				if tt.shouldErr {
					require.Error(t, err)
				} else {
					require.NoError(t, err)
				}
			})
		}
	})
}

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
				A: "000000000.00",
				B: "000000000.00",
				C: "000000000.00",
				D: "000000000.00",
				E: "000000000.00",
				F: "000000000.00",
				G: "00",
				H: []string{
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00",
					"000000000.00"},
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

func TestUnmarshalTypeError_Unwrap(t *testing.T) {
	t.Parallel()
	cause := fmt.Errorf("underlying cause")
	e := &UnmarshalTypeError{Cause: cause}
	require.ErrorIs(t, e, cause)
}
