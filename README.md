# go-pic
COBOL PIC (Picture) clause parsing library

[![Go Report Card](https://goreportcard.com/badge/github.com/pgmitche/go-pic)](https://goreportcard.com/report/github.com/pgmitche/go-pic)
[![GoDoc](https://pkg.go.dev/badge/github.com/pgmitche/go-pic?status.svg)](https://pkg.go.dev/github.com/pgmitche/go-pic?tab=doc)
[![Sourcegraph](https://sourcegraph.com/github.com/pgmitche/go-pic/-/badge.svg)](https://sourcegraph.com/github.com/pgmitche/go-pic?badge)
[![Release](https://img.shields.io/github/release/pgmitche/go-pic.svg?style=flat-square)](https://github.com/pgmitche/go-pic/releases)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/pgmitche/go-pic/blob/master/LICENSE)

## Install

Get started using `gopic` by installing the struct-generation tool from the root directory by running:

`$ make install`

## Tag-based unmarshalling 

`gopic`'s main focus is enabling simpler 1:1 mapping of PIC definitions to Go structs. 

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

Or you can make use of `gopic`s other feature below, so that you don't have to define the struct yourself...

## Generating tagged structs

`gopic` provides support to generate flattened Go struct representations of your COBOL copybooks, tagged with length statements and assigned the appropriate type for unmarshalling files that match your COBOL copybook definitions.

`cmd` contains go struct generation tool from textual PIC definitions

Example struct gen usage:  

A file:  
`gopic file -o mycopybookstruct -i cobolstuff/copybook.txt`

A directory of files:  
`gopic dir -o mystructsdir -i cobolstuff`

When using `gopic` for struct generation, additional, non-functional values are tagged to the PIC tags, for legibility's sake. 

For example, the example struct above, if generated with `gopic` becomes:

```go
type Copybook struct{
    Dummy1 int      `pic:"4"`  // start:1 end:4
    Dummy2 string   `pic:"1"`  // start:5 end:5
    Dummy3 int      `pic:"4"`  // start:6 end:9
    Dummy4 string   `pic:"40"` // start:10 end:49
    Dummy5 string   `pic:"40"` // start:50 end:89
}
```

where the values...
```go
Dummy1 int      `pic:"4"` // start:1 end:4
```
Represent:
- 4 = length
- 1 = start index
- 4 = ending index
