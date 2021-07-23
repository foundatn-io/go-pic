package pic

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestUnmarshal_Omit(t *testing.T) {
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
	t.Run("Field Length 1", func(t *testing.T) {
		var st = struct {
			F1 string `pic:"1,1"`
		}{}

		err := Unmarshal([]byte("v"), &st)
		if err != nil {
			t.Errorf("Unmarshal() err %v", err)
		}

		if st.F1 != "v" {
			t.Errorf("Unmarshal() want %v, have %v", "v", st.F1)
		}
	})

	t.Run("Replicate basic OCCURS clauses", func(t *testing.T) {
		type basicWithOccurs struct {
			String    string `pic:"1,5"`
			Int       int    `pic:"6,10"`
			IntOccurs []int  `pic:"11,16,3"`
		}
		expect := &basicWithOccurs{"foo", 123, []int{12, 34, 56}}
		got := &basicWithOccurs{}
		err := Unmarshal([]byte("foo  123  123456"), got)
		require.NoError(t, err)
		require.Equal(t, expect, got)
	})

	t.Run("Replicate basic OCCURS clauses", func(t *testing.T) {
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
		err := Unmarshal([]byte("foo  123  123456"), got)
		require.NoError(t, err)
		require.Equal(t, expect, got)
	})

	t.Run("Invalid Unmarshal Errors", func(t *testing.T) {
		for _, test := range []struct {
			name      string
			v         interface{}
			shouldErr bool
		}{
			{"Invalid Unmarshal Nil", nil, true},
			{"Invalid Unmarshal Not Pointer 1", struct{}{}, true},
			{"Invalid Unmarshal Not Pointer 2", []struct{}{}, true},
			{"Valid Unmarshal slice", &[]struct{}{}, false},
			{"Valid Unmarshal struct", &struct{}{}, true},
		} {
			tt := test
			t.Run(tt.name, func(t *testing.T) {
				err := Unmarshal([]byte{}, tt.v)
				if tt.shouldErr != (err != nil) {
					t.Errorf("Unmarshal() err want %v, have %v (%v)", tt.shouldErr, err != nil, err)
				}
			})
		}
	})
}

func TestUnmarshal(t *testing.T) {
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
			name:   "Slice Case (no trailing new line)",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  "),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
		}, {
			name:   "Slice Case (trailing new line)",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
		}, {
			name:   "Slice Case (blank line mid file)",
			val:    []byte("foo  123  1.2  " + "\n" + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"", 0, 0},
				{"bar", 321, 2.1},
			},
		}, {
			name:     "Basic Struct Case",
			val:      []byte("foo  123  1.2  "),
			target:   &basicTypes{},
			expected: &basicTypes{"foo", 123, 1.2},
		}, {
			name:     "Unmarshal Error",
			val:      []byte("foo  nan  ddd  "),
			target:   &basicTypes{},
			expected: &basicTypes{},
			err:      fmt.Errorf("pic: cannot unmarshal foo  nan  ddd   into Go struct field basicTypes.Int of type int: failed string->int conversion: strconv.Atoi: parsing \"nan\": invalid syntax"),
		}, {
			name:     "Empty Line",
			val:      []byte(""),
			target:   &basicTypes{},
			expected: &basicTypes{},
			err:      fmt.Errorf("EOF"),
		}, {
			name:     "Invalid Target",
			val:      []byte("foo  123  1.2  "),
			target:   basicTypes{},
			expected: basicTypes{},
			err:      fmt.Errorf("decode: unmarshal target object is not a pointer, or is nil"),
		}, {
			name:   "offsetcheck",
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
			name:   "BasicStructUnpack",
			val:    []byte("thirteen13131thirteen13131ABCDE"),
			target: &nestedStruct{},
			expected: &nestedStruct{
				A: "thirteen13131",
				B: "thirteen13131",
				C: C{CA: []string{"A", "B", "C", "D", "E"}},
			},
		}, {
			name:   "MultiNestedStructUnpack",
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
			err := Unmarshal(tt.val, tt.target)
			if tt.err != nil || err != nil {
				require.EqualError(t, err, tt.err.Error())
			} else {
				require.Equal(t, tt.expected, tt.target)
			}
		})
	}
}
