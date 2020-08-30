# go-pic
COBOL PIC (Picture) clause parsing library

## Tag-based unmarshalling 

go-pic's main focus is enabling simpler 1:1 mapping of PIC definitions to Go structs. 

For example, if your PIC definitions look like:
```
000180             15  DUMMY-1      PIC 9(4).               00000117
000190             15  DUMMY-2      PIC X.                  00000118
000340             15  DUMMY-3      PIC 9(4).               00000133
000350             15  DUMMY-4      PIC X(40).              00000134
000360             15  DUMMY-5      PIC X(40).              00000135
```
You can create a struct with the following tags to unmarshal your data into.
```go
package dummy 

type Copybook struct{
    Dummy1 int      `pic:"4"`
    Dummy2 string   `pic:"1"`
    Dummy3 int      `pic:"4"`
    Dummy4 string   `pic:"40"`
    Dummy5 string   `pic:"40"`
}
```

Or you can make use of `go-pics` other feature below, so that you don't have to define the struct yourself...

## Generating tagged structs

`go-pic` provides support to generate flattened Go struct representations of your COBOL copybooks, tagged with length statements and assigned the appropriate type for unmarshalling files that match your COBOL copybook definitions.

`cmd` contains go struct generation tool from textual PIC definitions

Example struct gen usage:  

A file:  
`gopic file -o mycopybookstruct -i cobolstuff/copybook.txt`

A directory of files:  
`gopic dir -o mystructsdir -i cobolstuff`

When using `go-pic` for struct generation, additional, non-functional values are tagged to the PIC tags, for legibility's sake. 

For example, the example struct above, if generated with `go-pic` becomes:

```go
type Copybook struct{
    Dummy1 int      `pic:"4,1,4"`
    Dummy2 string   `pic:"1,5,5"`
    Dummy3 int      `pic:"4,6,9"`
    Dummy4 string   `pic:"40,10,49"`
    Dummy5 string   `pic:"40,50,89"`
}
```

where the values represent:
```go
Dummy1 int      `pic:"4,1,4"`
```
- 4, pos 1 = length
- 1, pos 2 = starting index
- 4, pos 3 = ending index


### TODO:
- Missing `OCCURS` support