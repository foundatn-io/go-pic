package lex

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"
)

const (
	alphaIndicators     = "XA"
	decimalIndicators   = ".VP"
	signedIntIndicators = "S"
	intIndicators       = "9"
)

// parsePICType returns the Go reflect.Kind that best represents the given
// PIC definition string (e.g. "X(5)", "9(9).9(2)", "S9(4)").
func parsePICType(s string) reflect.Kind {
	s = strings.TrimRight(s, ".")
	switch {
	case strings.ContainsAny(s, alphaIndicators):
		return reflect.String
	case strings.ContainsAny(s, decimalIndicators):
		return reflect.Float64
	case strings.ContainsAny(s, signedIntIndicators):
		return reflect.Int
	case strings.ContainsAny(s, intIndicators):
		return reflect.Uint
	default:
		return reflect.Invalid
	}
}

// parsePICCount returns the total byte-width of the given PIC definition.
//
// Rules:
//   - A bare type character (X, 9, S, V, …) counts as one position.
//   - A parenthesised repetition T(N) replaces the preceding type character
//     with N positions (the type char is subtracted, N is added).
//   - An explicit decimal point '.' between groups counts as one position.
//
// The function operates in a single O(n) left-to-right pass.
func parsePICCount(s string) (int, error) {
	s = strings.TrimRight(s, ".")
	total := 0
	for i := 0; i < len(s); {
		if s[i] == '(' {
			// Undo the position already counted for the preceding type char.
			total--
			j := strings.IndexByte(s[i:], ')')
			if j < 0 {
				return 0, fmt.Errorf("unmatched '(' in PIC definition %q", s)
			}
			count, err := strconv.Atoi(s[i+1 : i+j])
			if err != nil {
				return 0, fmt.Errorf("failed to convert PIC count %q: %w", s[i+1:i+j], err)
			}
			total += count
			i += j + 1
		} else {
			total++
			i++
		}
	}
	return total, nil
}

// parseOccursCount captures N where N is the OCCURS count
// e.g. OCCURS 12. returns 12
func parseOccursCount(t token) (int, error) {
	countStr := strings.TrimSuffix(strings.TrimPrefix(t.value, "OCCURS "), ".")
	val, err := strconv.Atoi(countStr)
	if err != nil {
		return 0, fmt.Errorf("parse count: %w", err)
	}
	return val, nil
}
