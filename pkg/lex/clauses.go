package lex

import (
	"fmt"
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
	typeMap = map[picType]reflect.Kind{
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

	switch {
	case strings.ContainsAny(s, alphaIndicators):
		if alpha > picType {
			picType = alpha
		}
	case strings.ContainsAny(s, decimalIndicators):
		if decimal > picType {
			picType = decimal
		}
	case strings.ContainsAny(s, signedIntIndicators):
		if signed > picType {
			picType = signed
		}
	case strings.ContainsAny(s, intIndicators):
		picType = unsigned
	}
	return typeMap[picType]
}

// parsePICCount identifies the fixed width, or length, of the given
// PIC definition such as: X(2)., XX., 9(9)., etc.
//
// TODO: this can be simplified further, and should include tests
func parsePICCount(s string) (int, error) {
	s = strings.TrimRight(s, ".")
	picChars := []rune(s)
	totalLength := 0

	for strings.Contains(string(picChars), "(") {
		left := strings.Index(string(picChars), "(")
		right := strings.Index(string(picChars), ")")
		start := left - 1
		end := right + 1
		length, err := strconv.Atoi(string(picChars[left+1 : right]))
		if err != nil {
			return 0, fmt.Errorf("failed to convert PIC count '%s' to int: %w", string(picChars[left+1:right]), err)
		}
		totalLength += length
		picChars = append(picChars[:start], picChars[end:]...)
	}
	return totalLength + len(picChars), nil
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
