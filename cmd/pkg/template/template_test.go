package template

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/foundatn-io/go-pic/pkg/lex"
)

func TestTranslateToGoType(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		name string
		rec  *lex.Record
		want string
	}{
		{"string", &lex.Record{Typ: reflect.String}, "string"},
		{"int", &lex.Record{Typ: reflect.Int}, "int"},
		{"uint", &lex.Record{Typ: reflect.Uint}, "uint"},
		{"float64", &lex.Record{Typ: reflect.Float64}, "float64"},
		{"struct uses sanitised name", &lex.Record{Typ: reflect.Struct, Name: "MY-GROUP"}, "MYGROUP"},
		{"slice prefix when occurs", &lex.Record{Typ: reflect.String, Occurs: 3}, "[]string"},
	} {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			require.Equal(t, tt.want, translateToGoType(tt.rec))
		})
	}
}

func TestTranslateToGoType_PanicsOnUnknown(t *testing.T) {
	t.Parallel()
	require.Panics(t, func() {
		translateToGoType(&lex.Record{Typ: reflect.Complex128})
	})
}

func TestSanitiseName(t *testing.T) {
	t.Parallel()
	require.Equal(t, "DUMMYGROUP1", sanitiseName("DUMMY-GROUP-1"))
	require.Equal(t, "ABC", sanitiseName("A.B.C"))
	require.Equal(t, "plain", sanitiseName("plain"))
}

func TestCheckIfStruct(t *testing.T) {
	t.Parallel()
	require.True(t, checkIfStruct(&lex.Record{Typ: reflect.Struct}))
	require.False(t, checkIfStruct(&lex.Record{Typ: reflect.String}))
}

func TestGeneratePicTag(t *testing.T) {
	t.Parallel()
	ts := newTemplateState()
	// Consecutive fields advance the running start/end cursor.
	require.Equal(t, "`pic:\"1,5\"`", ts.generatePicTag(5, 0))
	require.Equal(t, "`pic:\"6,7\"`", ts.generatePicTag(2, 0))

	// An occurs count multiplies the element size and adds a 3rd tag element.
	occ := newTemplateState()
	require.Equal(t, "`pic:\"1,15,3\"`", occ.generatePicTag(5, 3))
}

func TestGenerateIndexComment(t *testing.T) {
	t.Parallel()
	require.Equal(t, " // start:1 end:5", newTemplateState().generateIndexComment(5, 0))
	require.Equal(t, " // start:1 end:6", newTemplateState().generateIndexComment(3, 2))
}

func TestCalculateElementSize(t *testing.T) {
	t.Parallel()
	ts := newTemplateState()
	require.Equal(t, 5, ts.calculateElementSize(5, 0))
	require.Equal(t, 15, ts.calculateElementSize(5, 3))
}

// TestCopybookTemplate_Execute renders a small record tree end-to-end, covering
// the struct/group template builders and the constructStruct/retrieveStructs
// helpers that the per-function tests cannot reach directly.
func TestCopybookTemplate_Execute(t *testing.T) {
	t.Parallel()
	root := &lex.Record{
		Name: "root",
		Typ:  reflect.Struct,
		Children: []*lex.Record{
			{Name: "FIELD-A", Typ: reflect.Uint, Length: 5},
			{
				Name: "GRP",
				Typ:  reflect.Struct,
				Children: []*lex.Record{
					{Name: "FIELD-B", Typ: reflect.String, Length: 3},
				},
			},
		},
	}
	data := struct {
		Package string
		Root    *lex.Record
	}{Package: "demo", Root: root}

	var b bytes.Buffer
	require.NoError(t, Copybook().Execute(&b, data))

	out := b.String()
	require.Contains(t, out, "package demo")
	require.Contains(t, out, "type root struct")
	require.Contains(t, out, "FIELDA uint")
	require.Contains(t, out, "type GRP struct")
	require.Contains(t, out, "FIELDB string")
}
