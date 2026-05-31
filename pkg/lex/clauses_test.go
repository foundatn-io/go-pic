package lex

import (
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_parsePICCount(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name    string
		in      string
		want    int
		wantErr bool
	}{
		{"bare single char", "X", 1, false},
		{"bare multi char", "XX", 2, false},
		{"bare four chars", "XXXX", 4, false},
		{"parenthesised single", "X(10)", 10, false},
		{"parenthesised multi digit", "9(14)", 14, false},
		{"explicit decimal", "9(9).9(2)", 12, false},
		{"explicit decimal larger", "9(10).9(3)", 14, false},
		{"signed integer", "S9(9)", 10, false},
		{"trailing dot stripped", "X(5).", 5, false},
		{"simple 9", "9", 1, false},
		{"V decimal indicator", "9(4)V9(4)", 9, false},
		{"bad parens no close", "X(10", 0, true},
		{"bad count non-numeric", "X(abc)", 0, true},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := parsePICCount(tt.in)
			if tt.wantErr {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)
			require.Equal(t, tt.want, got)
		})
	}
}

func Test_parsePICType(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name string
		in   string
		want reflect.Kind
	}{
		{"alpha X", "X", reflect.String},
		{"alpha A", "A", reflect.String},
		{"alpha with count", "X(10)", reflect.String},
		{"unsigned int", "9", reflect.Uint},
		{"unsigned int with count", "9(4)", reflect.Uint},
		{"signed int", "S9(9)", reflect.Int},
		{"decimal V", "9(4)V9(4)", reflect.Float64},
		{"decimal explicit dot", "9(9).9(2)", reflect.Float64},
		{"decimal P", "P9(3)", reflect.Float64},
		{"unknown empty", "", reflect.Invalid},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, parsePICType(tt.in))
		})
	}
}
