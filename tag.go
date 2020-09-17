package pic

import (
	"reflect"
	"strconv"
	"strings"
	"sync"
)

var fieldRepCache sync.Map // map[reflect.Type]structSpec

type structRepresentation struct {
	len    int
	fields []fieldRepresentation
}

type fieldRepresentation struct {
	setFunc         setFunc
	len, start, end int
	err             error
}

func parseTag(tag string, prev int) (int, int, int, int, error) {
	var occursSize int
	ss := strings.Split(tag, ",")
	if len(ss) == 2 {
		o, err := strconv.Atoi(ss[1])
		if err != nil {
			return 0, 0, 0, 0, err
		}
		occursSize = o
	}

	len, err := strconv.Atoi(ss[0])
	if err != nil {
		return 0, 0, 0, 0, err
	}

	if occursSize > 0 {
		len *= occursSize
	}

	return len, prev + 1, len + prev, occursSize, nil
}

func makeStructRepresentation(t reflect.Type) structRepresentation {
	sr := structRepresentation{
		fields: make([]fieldRepresentation, t.NumField()),
	}

	last := 0
	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)

		l, s, e, occursSize, err := parseTag(f.Tag.Get("pic"), last)
		last = e

		sr.fields[i].len = l
		sr.fields[i].start = s
		sr.fields[i].end = e
		sr.fields[i].err = err
		sr.fields[i].setFunc = newSetFunc(f.Type, occursSize)
		if sr.fields[i].end > sr.len {
			sr.len = sr.fields[i].end
		}
	}

	return sr
}

// cachedStructSpec is like buildStructSpec but cached to prevent duplicate work.
func cachedStructRepresentation(t reflect.Type) structRepresentation {
	if f, ok := fieldRepCache.Load(t); ok {
		return f.(structRepresentation)
	}

	f, _ := fieldRepCache.LoadOrStore(t, makeStructRepresentation(t))
	return f.(structRepresentation)
}
