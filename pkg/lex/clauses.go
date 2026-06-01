// Package lex lexes and parses COBOL copybook definitions into a Record tree.
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

// parsePICCount returns the number of physical storage bytes occupied by the
// given PIC definition string, in a single O(n) left-to-right pass.
//
// COBOL symbol behavior:
//   - Digit and character symbols (9, X, A) each occupy one byte.
//   - A parenthesised repetition T(N) replaces the preceding symbol with N
//     positions (so "9(4)" is four bytes, not five).
//   - V (implicit decimal point) and P (assumed decimal scaling) are positional
//     only and occupy zero bytes; a V(N)/P(N) repetition is likewise zero.
//   - An explicit '.' between digit groups is a real character and occupies one
//     byte.
//   - S (operational sign) occupies zero bytes by default, because USAGE DISPLAY
//     overpunches the sign onto a digit. It occupies one byte only when the
//     copybook declares SIGN IS SEPARATE, which the grammar does not parse — so
//     callers pass signSeparate=true to opt into that interpretation.
func parsePICCount(s string, signSeparate bool) (int, error) {
	s = strings.TrimRight(s, ".")
	total := 0
	prevWasNoStorage := false // true if the previous char occupied no storage (V, P, or default S)
	for i := 0; i < len(s); {
		ch := s[i]
		switch ch {
		case '(':
			j := strings.IndexByte(s[i:], ')')
			if j < 0 {
				return 0, fmt.Errorf("unmatched '(' in PIC definition %q", s)
			}
			if prevWasNoStorage {
				// The preceding symbol contributed no storage, so its
				// repetition contributes none either — skip entirely.
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
		case 'S':
			// Operational sign: a byte only under SIGN IS SEPARATE (see godoc).
			if signSeparate {
				total++
			}
			prevWasNoStorage = !signSeparate
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
