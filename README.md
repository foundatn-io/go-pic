# go-pic
COBOL PIC (Picture) clause parsing library

[![Go Report Card](https://goreportcard.com/badge/github.com/pgmitche/go-pic)](https://goreportcard.com/report/github.com/pgmitche/go-pic)
[![GoDoc](https://pkg.go.dev/badge/github.com/pgmitche/go-pic?status.svg)](https://pkg.go.dev/github.com/pgmitche/go-pic?tab=doc)
[![Sourcegraph](https://sourcegraph.com/github.com/pgmitche/go-pic/-/badge.svg)](https://sourcegraph.com/github.com/pgmitche/go-pic?badge)
[![Release](https://img.shields.io/github/release/pgmitche/go-pic.svg?style=flat-square)](https://github.com/pgmitche/go-pic/releases)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/pgmitche/go-pic/blob/master/LICENSE)

## What is go-pic

`gopic` is both a tool and package, it's purpose is to help you process COBOL copybook files with ease

#### Unmarshaller

`gopic` can be used to enable simpler 1:1 mapping of PIC definitions to Go structs. 

Simply open your data file and pass it to a gopic decoder with the struct that represents the source
copybook

<details><summary><b>Show usage</b></summary>

1. Import gopic

    ```go
    import (
        pic "github.com/pgmitche/go-pic"
    )
    ```

2. Tag your structs

    Say your copybook data looks like
    ```
    000180      15  PropertyA    PIC X(5).     00000117
    000190      15  PropertyB    PIC X(2).     00000118
    ```
    You would tag your struct like so
    ```go
    type yourStruct struct {
        PropertyA string `pic:"5"` 
        PropertyB string `pic:"2"`
    }
    ```

3. Prepare a decoder and unmarshal your input

    ```go
    d := pic.NewDecoder(f) // where f is your io.Reader / data
    typ := yourStruct{} // with pic tags
    if err := d.Decode(typ); err != nil {
            log.Fatal(err)
    }
    ```

</details>

#### Struct generator

`gopic` can be used to generate simpler 1:1 mapping of PIC definitions to Go structs. 

`gopic` provides support to generate flattened Go struct representations of your COBOL copybooks, tagged with length statements and assigned the appropriate type for unmarshalling files that match your COBOL copybook definitions, so you need not manually create or tag your structs when using `go-pic` for unmarshalling.

<details><summary><b>Show usage</b></summary>

1. Install gopic!

    Get started using `gopic` by for struct generation by running:

    ```shell script
    git clone github.com/pgmitche/go-pic
    cd go-pic
    make install
    ```
   
2. Generate structs from a copybook file (long-form flags)
    
    ```shell script
    gopic file --package=shipping --output=shipping --input=cobolstuff/copybook-shipping.txt
    ```
    
3. Generate many structs from a directory containing only copybooks (short-form flags)

    ```shell script
    gopic dir -p mystructsdir -o mystructsdir -i cobolstuff
    ```

</details>

When using `gopic` for struct generation, additional, non-functional values are tagged to the PIC tags, for legibility's sake. 

For example, the example copybook clauses:

```
000180      15  PropertyA    PIC 9(5).     00000117
000190      15  PropertyB    PIC X(2).     00000118
```

if generated with `gopic` becomes:

```go
type Copybook struct{
    PropertyA uint      `pic:"9"`  // start:1 end:9
    PropertyB string    `pic:"2"`  // start:10 end:11
}
```
