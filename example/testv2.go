////////////////////////////////
//     AUTOGENERATED FILE     //
// File generated with go-pic //
////////////////////////////////

// nolint
package main

// Copybooktest contains a representation of your provided Copybook
type root struct {
	DUMMYGROUP1 DUMMYGROUP1
	DUMMYGROUP2 DUMMYGROUP2
}

type DUMMYSUBGROUP1 struct {
	DUMMYGROUP1OBJECTA uint   `pic:"4"`  // start:1 end:4
	DUMMYGROUP1OBJECTB string `pic:"1"`  // start:5 end:5
	DUMMYGROUP1OBJECTC uint   `pic:"4"`  // start:6 end:9
	DUMMYGROUP1OBJECTD string `pic:"40"` // start:10 end:49
	DUMMYGROUP1OBJECTE string `pic:"8"`  // start:50 end:57
	DUMMYGROUP1OBJECTG string `pic:"2"`  // start:58 end:59
	DUMMYGROUP1OBJECTH uint   `pic:"4"`  // start:60 end:63
}

type DUMMYGROUP1 struct {
	DUMMYSUBGROUP1 DUMMYSUBGROUP1
}

type DUMMYSUBGROUP2GETSDROPPED struct {
	DUMMYSUBGROUP2OBJECTA []string `pic:"12,12"` // start:97 end:240
}

type DUMMYGROUP2 struct {
	DUMMYGROUP2OBJECTA        string `pic:"14"` // start:64 end:77
	DUMMYGROUP2OBJECTB        uint   `pic:"7"`  // start:78 end:84
	DUMMYGROUP2OBJECTC        string `pic:"4"`  // start:85 end:88
	DUMMYGROUP2OBJECTD        string `pic:"1"`  // start:89 end:89
	DUMMYGROUP2OBJECTF        string `pic:"7"`  // start:90 end:96
	DUMMYSUBGROUP2GETSDROPPED DUMMYSUBGROUP2GETSDROPPED
}