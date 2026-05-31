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

// parsePICCount returns the total number of physical storage bytes for the
// given PIC definition string.
//
// COBOL symbol behaviour:
//   - Bare type characters (X, 9, A, S) each count as one physical byte.
//   - V (implicit decimal point) and P (assumed decimal scaling) contribute
//     zero physical storage bytes.
//   - A parenthesised repetition T(N) replaces the preceding type character
//     with N positions. If T was V or P (zero-storage), the entire T(N)
//     contributes zero bytes.
//   - An explicit '.' between digit groups counts as one physical byte.
//
// The function operates in a single O(n) left-to-right pass.
func parsePICCount(s string) (int, error) {
	s = strings.TrimRight(s, ".")
	total := 0
	prevWasNoStorage := false // true if the previous char was V or P
	for i := 0; i < len(s); {
		ch := s[i]
		switch ch {
		case '(':
			j := strings.IndexByte(s[i:], ')')
			if j < 0 {
				return 0, fmt.Errorf("unmatched '(' in PIC definition %q", s)
			}
			if prevWasNoStorage {
				// V(N) or P(N): the preceding char was not counted and the
				// repetition also contributes no storage — skip entirely.
				prevWasNoStorage = false
				i += j + 1
			} else {
				// Normal T(N): undo the single byte already counted for T,
				// then add the actual repetition count.
				total--
				count, err := strconv.Atoi(s[i+1 : i+j])
				if err != nil {
					return 0, fmt.Errorf("failed to convert PIC count %q: %w", s[i+1:i+j], err)
				}
				total += count
				prevWasNoStorage = false
				i += j + 1
			}
		case 'V', 'P':
			// Implicit decimal / assumed decimal scaling: no physical byte.
			prevWasNoStorage = true
			i++
		default:
			total++
			prevWasNoStorage = false
			i++
		}
	}
	return total, nil
}

// parseOccursCount captures N where N is the OCCURS count.
// e.g. OCCURS 12. returns 12
func parseOccursCount(t token) (int, error) {
	countStr := strings.TrimSuffix(strings.TrimPrefix(t.value, "OCCURS "), ".")
	val, err := strconv.Atoi(countStr)
	if err != nil {
		return 0, fmt.Errorf("parse count: %w", err)
	}
	return val, nil
}
