package decoder

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_getLineType(t *testing.T) {
	tests := []struct {
		name string
		line string
		want lineKind
	}{
		{
			name: "BasicPICStringRepeated",
			line: "12345 01 NAME PIC XX. 054321",
			want: pic,
		}, {
			name: "BasicPICStringParenthesesCount",
			line: "12345 01 NAME PIC X(9). 054321",
			want: pic,
		}, {
			name: "BasicPICNumberRepeated",
			line: "12345 01 NAME PIC 99. 054321",
			want: pic,
		}, {
			name: "BasicPICNumberParenthesesCount",
			line: "12345 01 NAME PIC 9(9). 054321",
			want: pic,
		}, {
			name: "BasicPICStringNoPeriodPotentialOccursTarget",
			line: "12345 01 NAME PIC XX 054321",
			want: occursMulti,
		}, {
			name: "BasicPICNumberNoPeriodPotentialOccursTarget",
			line: "12345 01 NAME PIC 9(9) 054321",
			want: occursMulti,
		}, {
			name: "RegularPICLiteralDot",
			line: "12345 01 NAME PIC 9(9).9(9). 054321",
			want: pic,
		},
	}
	for _, test := range tests {
		tt := test
		t.Run(tt.name, func(t *testing.T) {
			got := getLineType(tt.line)
			require.Equal(t, tt.want, got)
		})
	}
}
