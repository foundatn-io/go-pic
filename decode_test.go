package pic

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestUnmarshal(t *testing.T) {
	type basicTypes struct {
		String string  `pic:"5"`
		Int    int     `pic:"5"`
		Float  float64 `pic:"5"`
	}

	type occursOffset struct {
		A string   `pic:"13"`    // start:227 end:239
		B string   `pic:"13"`    // start:240 end:252
		C string   `pic:"13"`    // start:253 end:265
		D string   `pic:"13"`    // start:266 end:278
		E string   `pic:"13"`    // start:279 end:291
		F string   `pic:"13"`    // start:292 end:304
		G string   `pic:"2"`     // start:305 end:306
		H []string `pic:"13,12"` // start:307 end:462
	}

	for _, test := range []struct {
		name      string
		val       []byte
		target    interface{}
		expected  interface{}
		shouldErr bool
	}{
		{
			name:   "Slice Case (no trailing new line)",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  "),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
			shouldErr: false,
		},
		{
			name:   "Slice Case (trailing new line)",
			val:    []byte("foo  123  1.2  " + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"bar", 321, 2.1},
			},
			shouldErr: false,
		},
		{
			name:   "Slice Case (blank line mid file)",
			val:    []byte("foo  123  1.2  " + "\n" + "\n" + "bar  321  2.1  " + "\n"),
			target: &[]basicTypes{},
			expected: &[]basicTypes{
				{"foo", 123, 1.2},
				{"", 0, 0},
				{"bar", 321, 2.1},
			},
			shouldErr: false,
		},
		{
			name:      "Basic Struct Case",
			val:       []byte("foo  123  1.2  "),
			target:    &basicTypes{},
			expected:  &basicTypes{"foo", 123, 1.2},
			shouldErr: false,
		},
		{
			name:      "Unmarshal Error",
			val:       []byte("foo  nan  ddd  "),
			target:    &basicTypes{},
			expected:  &basicTypes{},
			shouldErr: true,
		},
		{
			name:      "Empty Line",
			val:       []byte(""),
			target:    &basicTypes{},
			expected:  &basicTypes{},
			shouldErr: true,
		},
		{
			name:      "Invalid Target",
			val:       []byte("foo  123  1.2  "),
			target:    basicTypes{},
			expected:  basicTypes{},
			shouldErr: true,
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
		},
	} {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			err := Unmarshal(tt.val, tt.target)
			if tt.shouldErr {
				require.Error(t, err)
			} else {
				require.Equal(t, tt.target, tt.expected)
			}
		})
	}

	t.Run("Field Length 1", func(t *testing.T) {
		var st = struct {
			F1 string `pic:"1"`
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
			String    string `pic:"5"`
			Int       int    `pic:"5"`
			IntOccurs []int  `pic:"2,3"`
		}
		expect := &basicWithOccurs{"foo", 123, []int{12, 34, 56}}
		got := &basicWithOccurs{}
		err := Unmarshal([]byte("foo  123  123456"), got)
		require.NoError(t, err)
		require.Equal(t, expect, got)
	})

	t.Run("Replicate basic OCCURS clauses", func(t *testing.T) {
		type dummy struct {
			A int `pic:"1"`
			B int `pic:"1"`
		}
		type basicWithOccursStruct struct {
			String string  `pic:"5"`
			Int    int     `pic:"5"`
			Dummy  []dummy `pic:"2,3"`
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
