package parse

import (
	"regexp"
)

type lineKind int

const (
	xxx lineKind = iota
	pic
	picIncomplete
	redefinesMulti
	redefinesSingle
	occursMulti
	occursSingle
)

var (
	// ^NUM LEVEL NAME ( \. | PIC X~ | PIC X(9) | REDEFINES NAME)\. CLOSERNUM$
	// NUM 											[0-9]+
	// LEVEL 										[0-9]{2}
	// NAME 										[a-zA-Z0-9\-]+
	// Termination of clause, indicates group 		.
	// PIC X~ | PIC X(9) 							(PIC ([X9]+|[X9]\([0-9]+\))
	// REDEFINES NAME								REDEFINES +[a-zA-Z0-9\-]+
	// CLOSERNUM									0+[0-9]+

	// Defines picture clause
	// 000600         10  DUMMY-1       PIC X.                  00000167
	// 000620         10  DUMMY-2       PIC 9(7).               00000169
	picLine = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\))\. +0+[0-9]+$`)

	// Defines picture clause that deviates from typical pattern
	//          10  DUMMY-1       PIC X.                  00000167
	//          10  DUMMY-2       PIC 9(7).               00000169
	incompletePICLine = regexp.MustCompile(`[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\))\.`)

	// Matches same-line REDEFINES definitions
	// 000550     05  DUMMY-1  PIC X(340).             00000162
	// 000590     05  DUMMY-2  REDEFINES  DUMMY-1.     00000166
	redefinesLine = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +REDEFINES +[a-zA-Z0-9\-]+\. +0+[0-9]+$`)

	// Matches multi-line REDEFINES definitions
	// 000420             15  DUMMY-1  PIC XX.                 00000141
	// 000420             15  DUMMY-2  REDEFINES               00000142
	// 000420                 DUMMY-1                          00000143
	redefinesStatement = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +REDEFINES +0+[0-9]+$`)

	// OCCURS statements can be found in 3 different forms
	// Intra-line - 001350           15  DUMMY-1 PIC X  OCCURS 12.       00000247
	// Multi-line - 001290           15  DUMMY-1 PIC X(12)               00000241
	// 				001300               OCCURS 12.                      00000242
	// Verbose    - 001630           15  DUMMY-1 OCCURS 7 TIMES.         00000347
	occursLine = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\)) OCCURS [0-9]+\. +0+[0-9]+$`)
	// used to detect when to scan ahead to next line
	occursStatement = regexp.MustCompile(`^[0-9]+ +[0-9]{2} +[a-zA-Z0-9\-]+ +PIC ([X9]+|[X9]\([0-9]+\)) +0+[0-9]+$`)
	// used when scanning ahead to a following line to find OCCURS clause
	occursWord = regexp.MustCompile(`^[0-9]+ +OCCURS [0-9]+\. +0+[0-9]+$`)
)

func getLineType(line string) lineKind {
	if redefinesStatement.MatchString(line) {
		return redefinesMulti
	}

	if redefinesLine.MatchString(line) {
		return redefinesSingle
	}

	if picLine.MatchString(line) {
		return pic
	}

	if incompletePICLine.MatchString(line) {
		return picIncomplete
	}

	if occursLine.MatchString(line) {
		return occursSingle
	}

	if occursStatement.MatchString(line) {
		return occursMulti
	}

	return xxx
}