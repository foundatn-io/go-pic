package lex

import (
	"reflect"
	"strconv"
	"strings"
)

type picType int

const (
	unknown picType = iota
	unsigned
	signed
	decimal
	alpha

	alphaIndicators     = "XA"
	decimalIndicators   = ".VP"
	signedIntIndicators = "S"
	intIndicators       = "9"
)

var (
	types = map[picType]reflect.Kind{
		unknown:  reflect.Invalid,
		unsigned: reflect.Uint,
		signed:   reflect.Int,
		decimal:  reflect.Float64,
		alpha:    reflect.String,
	}
)

// parsePICType identifies an equivalent Go type from the given substring
// that contains a PIC definition
func parsePICType(s string) reflect.Kind {
	picType := unknown
	s = strings.TrimRight(s, ".")
	if strings.ContainsAny(s, alphaIndicators) {
		if alpha > picType {
			picType = alpha
			return types[picType]
		}
	}

	if strings.ContainsAny(s, decimalIndicators) {
		if decimal > picType {
			picType = decimal
			return types[picType]
		}
	}

	if strings.ContainsAny(s, signedIntIndicators) {
		if signed > picType {
			picType = signed
			return types[picType]
		}
	}

	if strings.ContainsAny(s, intIndicators) {
		picType = unsigned
		return types[picType]
	}

	return types[picType]
}

// parsePICCount identifies the fixed width, or length, of the given
// PIC definition such as: X(2)., XX., 9(9)., etc.
func parsePICCount(s string) (int, error) {
	// prepare a slice of runes, representing the string
	s = strings.TrimRight(s, ".")
	c := []rune(s)

	size := 0

	// S9(9)V9(9)
	// SV = 2 + 18 = 20
	for strings.Contains(s, "(") {
		left := strings.Index(s, "(")
		right := strings.Index(s, ")")
		// capture type when using parentheses "9(99)" should be stripped to
		// "" so that it results in 99+0, not 99+len("9")
		start := left - 1
		end := right + 1
		amount, err := strconv.Atoi(s[left+1 : right])
		if err != nil {
			return 0, err
		}

		size += amount
		c = append(c[:start], c[end:]...)
		s = string(c)
	}

	return size + len(c), nil
}

// parseOccursCount captures N where N is the OCCURS count
// e.g. OCCURS 12. returns 12
func parseOccursCount(i item) (int, error) {
	s := strings.TrimPrefix(i.val, "OCCURS ")
	n := strings.TrimSuffix(s, ".")
	return strconv.Atoi(n)
}
