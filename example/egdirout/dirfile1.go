////////////////////////////////
//     AUTOGENERATED FILE     //
// File generated with go-pic //
////////////////////////////////

// nolint
package main

// dirfile1 contains a representation of your provided Copybook
type dirfile1 struct {
	DUMMYGROUP1 DUMMYGROUP1 `pic:"1,63"`
	DUMMYGROUP3 DUMMYGROUP3 `pic:"64,264"`
}

// DUMMYSUBGROUP1 contains a representation of the nested group DUMMY-SUB-GROUP-1
type DUMMYSUBGROUP1 struct {
	DUMMYGROUP1OBJECTA uint   `pic:"1,4"`   // start:1 end:4
	DUMMYGROUP1OBJECTB string `pic:"5,5"`   // start:5 end:5
	DUMMYGROUP1OBJECTC uint   `pic:"6,9"`   // start:6 end:9
	DUMMYGROUP1OBJECTD string `pic:"10,49"` // start:10 end:49
	DUMMYGROUP1OBJECTE string `pic:"50,57"` // start:50 end:57
	DUMMYGROUP1OBJECTG string `pic:"58,59"` // start:58 end:59
	DUMMYGROUP1OBJECTH uint   `pic:"60,63"` // start:60 end:63
}

// DUMMYGROUP1 contains a representation of the nested group DUMMY-GROUP-1
type DUMMYGROUP1 struct {
	DUMMYSUBGROUP1 DUMMYSUBGROUP1 `pic:"1,63"`
}

// DUMMYSUBGROUP2GETSDROPPED contains a representation of the nested group DUMMY-SUBGROUP-2-GETSDROPPED
type DUMMYSUBGROUP2GETSDROPPED struct {
	DUMMYSUBGROUP2OBJECTA []string `pic:"1,144,12"` // start:97 end:240
}

// DUMMYGROUP3 contains a representation of the nested group DUMMY-GROUP-3
type DUMMYGROUP3 struct {
	DUMMYGROUP2OBJECTA        string                    `pic:"1,14"`  // start:64 end:77
	DUMMYGROUP2OBJECTB        uint                      `pic:"15,21"` // start:78 end:84
	DUMMYGROUP2OBJECTC        string                    `pic:"22,25"` // start:85 end:88
	DUMMYGROUP2OBJECTD        string                    `pic:"26,26"` // start:89 end:89
	DUMMYGROUP2OBJECTF        string                    `pic:"27,33"` // start:90 end:96
	DUMMYSUBGROUP2GETSDROPPED DUMMYSUBGROUP2GETSDROPPED `pic:"34,177"`
	DUMMYGROUP2OBJECTG        string                    `pic:"178,189"` // start:241 end:252
	DUMMYGROUP2OBJECTH        string                    `pic:"190,201"` // start:253 end:264
}
