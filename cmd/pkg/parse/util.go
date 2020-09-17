package parse

import (
	"errors"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

func parsePICType(pic string) (reflect.Kind, error) {
	switch pic[0:1] {
	case "X":
		return reflect.String, nil
	case "9":
		return reflect.Int, nil
	}

	return reflect.Invalid, errors.New("unexpected PIC type")
}

func parsePICCount(pic string) (int, error) {
	// X(2).
	// left = 1+1, right = 3 s[2:3], size = 2
	// XX.
	// XX, size = 2
	if strings.Contains(pic, "(") {
		leftPos := strings.Index(pic, "(") + 1 // FROM index, not including
		rightPos := strings.Index(pic, ")")
		s := pic[leftPos:rightPos]
		size, err := strconv.Atoi(s)
		if err != nil {
			return 0, err
		}

		return size, nil
	}

	s := strings.Trim(pic, ".")
	return len(s), nil
}

func parseOccursCount(in string) (int, error) {
	s := strings.Trim(in, ".")
	return strconv.Atoi(s)
}

func trimExtraWhitespace(in string) string {
	return strings.Trim(
		regexp.MustCompile(`\s+`).ReplaceAllString(in, " "),
		" ")
}
