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
		signSep bool
	}{
		// bare chars
		{"bare single X", "X", 1, false, false},
		{"bare multi X", "XX", 2, false, false},
		{"bare four X", "XXXX", 4, false, false},
		{"bare single 9", "9", 1, false, false},
		// parenthesised
		{"paren single", "X(10)", 10, false, false},
		{"paren two digits", "9(14)", 14, false, false},
		// explicit decimal '.' counts as physical byte
		{"explicit decimal 9(9).9(2)", "9(9).9(2)", 12, false, false},
		{"explicit decimal 9(10).9(3)", "9(10).9(3)", 14, false, false},
		// V = implicit decimal, consumes NO physical bytes
		{"V implicit decimal bare", "9(4)V9(2)", 6, false, false},
		{"V bare no parens", "9999V99", 6, false, false},
		{"V with paren repetition", "9(5)V(2)", 5, false, false}, // V(N) — V contributes 0, repetition skipped
		// P = assumed decimal scaling, consumes NO physical bytes
		{"P scaling leading", "PPP999", 3, false, false},
		{"P scaling trailing", "9(4)PPP", 4, false, false},
		// S = sign: 0 bytes by default (overpunch on a digit, USAGE DISPLAY default)
		{"S overpunch signed integer", "S9(9)", 9, false, false},
		{"S overpunch no parens", "S999", 3, false, false},
		{"S overpunch with V", "S9(5)V9(2)", 7, false, false}, // S=0, 9(5)=5, V=0, 9(2)=2
		// S = sign: 1 byte under SIGN IS SEPARATE (opt-in via signSep)
		{"S separate signed integer", "S9(9)", 10, false, true},
		{"S separate no parens", "S999", 4, false, true},
		{"S separate with V", "S9(5)V9(2)", 8, false, true}, // S=1, 9(5)=5, V=0, 9(2)=2
		// trailing dot stripped
		{"trailing dot stripped", "X(5).", 5, false, false},
		// errors
		{"unclosed paren", "X(10", 0, true, false},
		{"non-numeric count", "X(abc)", 0, true, false},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := parsePICCount(tt.in, tt.signSep)
			if tt.wantErr {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)
			require.Equal(t, tt.want, got, "parsePICCount(%q)", tt.in)
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

func Test_parseOccursCount(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name    string
		tok     token
		want    int
		wantErr bool
	}{
		{"plain count", token{value: "OCCURS 12."}, 12, false},
		{"no dot", token{value: "OCCURS 3"}, 3, false},
		{"bad number", token{value: "OCCURS abc."}, 0, true},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := parseOccursCount(tt.tok)
			if tt.wantErr {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)
			require.Equal(t, tt.want, got)
		})
	}
}
