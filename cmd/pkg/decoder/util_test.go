package decoder

import (
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_parsePICCount(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want int
	}{
		{
			name: "StringWithParenthesesSingleDigit",
			in:   "X(2).",
			want: 2,
		}, {
			name: "RepeatedString",
			in:   "XXX.",
			want: 3,
		}, {
			name: "StringWithParenthesesMultipleDigits",
			in:   "X(99).",
			want: 99,
		}, {
			name: "NumberWithParenthesesMultipleDigits",
			in:   "9(99).",
			want: 99,
		}, {
			name: "NumberWithParenthesesSingleDigit",
			in:   "9(9).",
			want: 9,
		}, {
			name: "SignedNumberWithParenthesesSingleDigit",
			in:   "S9(9).",
			want: 10,
		}, {
			name: "SignedNumberWithMultiParentheses",
			in:   "S9(9)V9(9).",
			want: 20,
		}, {
			name: "SignedNumberWithMultiParenthesesLiteralPeriod",
			in:   "S9(9).9(9).",
			want: 20,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			got, err := parsePICCount(tt.in)
			require.NoError(t, err)
			require.Equal(t, tt.want, got)
		})
	}
}

func Test_parsePICType(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want reflect.Kind
	}{
		{
			name: "StringWithParenthesesSingleDigit",
			in:   "X(2).",
			want: reflect.String,
		}, {
			name: "RepeatedString",
			in:   "XXX.",
			want: reflect.String,
		}, {
			name: "StringWithParenthesesMultipleDigits",
			in:   "X(99).",
			want: reflect.String,
		}, {
			name: "NumberWithParenthesesMultipleDigits",
			in:   "9(99).",
			want: reflect.Uint,
		}, {
			name: "NumberWithParenthesesSingleDigit",
			in:   "9(9).",
			want: reflect.Uint,
		}, {
			name: "SWithParenthesesSingleDigit",
			in:   "S9(9).",
			want: reflect.Int,
		}, {
			name: "SWithParenthesesSingleDigitDecimalPlace",
			in:   "S9(9)V9(9).",
			want: reflect.Int,
		}, {
			name: "MultiParenthesesWithAlphanumeric",
			in:   "S(9)VX(9).",
			want: reflect.String,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			got := parsePICType(tt.in)
			require.Equal(t, tt.want, got)
		})
	}
}
