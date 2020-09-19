package decoder

import (
	"errors"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

// parsePICType identifies an equivalent Go type from the given substring
// that contains a PIC definition
func parsePICType(pic string) (reflect.Kind, error) {
	switch pic[0:1] {
	case "X":
		return reflect.String, nil
	case "9":
		return reflect.Int, nil
	}

	return reflect.Invalid, errors.New("unexpected PIC type")
}

// parsePICCount identifies the fixed width, or length, of the given
// PIC definition such as: X(2)., XX., 9(9)., etc.
func parsePICCount(pic string) (int, error) {
	// Contains a numerical length within parentheses...
	if strings.Contains(pic, "(") {
		leftPos := strings.Index(pic, "(") + 1
		rightPos := strings.Index(pic, ")")

		size, err := strconv.Atoi(pic[leftPos:rightPos])
		if err != nil {
			return 0, err
		}

		return size, nil
	}

	// ...or is a type-repeated length definition e.g. XXXX. (as 4)
	s := strings.Trim(pic, ".")
	return len(s), nil
}

// parseOccursCount captures the numerical definition of n occurrences
// e.g. definitions like "OCCURS 12." are passed in as "12." and return 12
func parseOccursCount(in string) (int, error) {
	return strconv.Atoi(strings.Trim(in, "."))
}

// trimExtraWhitespace flattens all whitespace down to a single space.
func trimExtraWhitespace(in string) string {
	return strings.Trim(
		regexp.MustCompile(`\s+`).ReplaceAllString(in, " "),
		" ")
}
