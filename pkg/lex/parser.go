package lex

import (
	"fmt"
	"reflect"
	"strings"
)

const (
	picPrefix = "PIC "
)

// Token positions within the recognised line patterns (see dictionary.go).
// Every pattern is a fixed sequence of token kinds, so the parser addresses a
// field by its index in that sequence rather than by re-scanning.
const (
	// Common leading layout of number-delimited lines:
	//   [0]seq# [1]space [2]level [3]space [4]name ...
	idxLevel = 2
	idxName  = 4

	// picPattern and occursPattern both carry the PIC clause at index 6:
	//   ... [4]name [5]space [6]"PIC X(n)" ...
	idxPICClause = 6
	// occursPattern: ... [6]"PIC X(n)" [7]space [8]"OCCURS n." ...
	idxOccursClause = 8

	// redefinesPattern: ... [8]targetName [9]space [10]"PIC X(n)" ...
	idxRedefinesTarget = 8
	idxRedefinesPIC    = 10

	// groupRedefinesPattern: ... [8]"targetName." (trailing dot, no PIC) ...
	idxGroupRedefinesTarget = 8
)

// lineParser is a function type that takes a Tree, a line, and a Record,
// and returns a new Record. It is used to parse different types of lines.
type lineParser func(tree *Tree, currentLine line, rootRecord *Record) (*Record, error)

// noop is a lineParser that does nothing and returns nil.
// It is used when the current line does not require any action.
func noop(_ *Tree, _ line, _ *Record) (*Record, error) {
	return nil, nil
}

// parsePIC builds a Record from a plain PIC definition line, deriving the
// field's Go type and byte length from its picture clause.
func parsePIC(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := currentLine.tokens[idxPICClause].value[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition, tree.signSeparate)
	if err != nil {
		return nil, err
	}
	return &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.tokens[idxName].value,
		Length:   length,
		depth:    currentLine.tokens[idxLevel].value,
		Typ:      parsePICType(pictureNumberDefinition),
	}, nil
}

// parseRedefinitions builds a Record from a REDEFINES line that carries its own
// PIC clause, then overlays it onto the redefinition target already held in the
// root Record.
func parseRedefinitions(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	pictureNumberDefinition := currentLine.tokens[idxRedefinesPIC].value[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition, tree.signSeparate)
	if err != nil {
		return nil, err
	}

	newRecord := &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.tokens[idxName].value,
		Length:   length,
		Typ:      parsePICType(pictureNumberDefinition),
	}

	target := currentLine.tokens[idxRedefinesTarget].value
	destination, index, err := rootRecord.fromCache(target)
	if err != nil {
		return nil, err
	}
	if destination == nil {
		return nil, fmt.Errorf("redefinition target %s does not exist", target)
	}
	return rootRecord.redefine(index, destination, newRecord), nil
}

// parseGroupRedefinitions builds a struct Record from a REDEFINES line that
// targets a group (no PIC clause). It overlays the new group onto the target in
// the root Record and recurses to parse the group's children.
func parseGroupRedefinitions(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	// Trim the trailing period from the target name, if present
	target := strings.TrimSuffix(currentLine.tokens[idxGroupRedefinesTarget].value, ".")
	destination, index, err := rootRecord.fromCache(target)
	if err != nil {
		return nil, err
	}
	if destination == nil {
		return nil, fmt.Errorf("redefinition target %s does not exist", target)
	}

	if len(destination.depthMap) == 0 {
		parent, seenGroup := rootRecord.depthMap[destination.depth]
		if seenGroup {
			copyDepthMap(parent, destination)
			rootRecord = parent
		}
	}

	newRecord := &Record{
		Name:     currentLine.tokens[idxName].value,
		Typ:      reflect.Struct,
		depth:    currentLine.tokens[idxLevel].value,
		depthMap: destination.depthMap,
	}

	rootRecord.Length -= destination.Length

	newRecord = rootRecord.redefine(index, destination, newRecord)
	if err := tree.parseLines(newRecord); err != nil {
		return nil, err
	}
	rootRecord.Length += newRecord.Length
	return newRecord, nil
}

// parseOccurs builds a Record from an OCCURS line, capturing both the element's
// PIC length and its repetition count.
func parseOccurs(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := strings.TrimSpace(currentLine.tokens[idxPICClause].value)[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition, tree.signSeparate)
	if err != nil {
		return nil, err
	}
	occurrenceCount, err := parseOccursCount(currentLine.tokens[idxOccursClause])
	if err != nil {
		return nil, err
	}
	return &Record{
		Name:     currentLine.tokens[idxName].value,
		Length:   length,
		Occurs:   occurrenceCount,
		depth:    currentLine.tokens[idxLevel].value,
		depthMap: map[string]*Record{},
		Typ:      parsePICType(pictureNumberDefinition),
	}, nil
}

// parseRedefinesMulti is a lineParser that builds a Record from a multi-line
// REDEFINES definition. It reads the next line from the tree, verifies it,
// and then calls parseRedefinitions with the combined line.
func parseRedefinesMulti(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	if err := tree.nextLine(); err != nil {
		return nil, err
	}
	continuation := tree.currentLine.tokens
	if !equalWord(getWord(continuation), multiRedefinesPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line redefinition, but failed to verify next line")
	}
	currentLine.tokens = lineFromMultiRedefines(currentLine.tokens, continuation)
	return parseRedefinitions(tree, currentLine, rootRecord)
}

// parseOccursMulti is a lineParser that builds a Record from a multi-line
// OCCURS definition. It reads the next line from the tree, verifies it,
// and then calls parseOccurs with the combined line.
func parseOccursMulti(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	if err := tree.nextLine(); err != nil {
		return nil, err
	}
	continuation := tree.currentLine.tokens
	if !equalWord(getWord(continuation), multiOccursPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line occurs, but failed to verify next line: %v", continuation)
	}
	currentLine.tokens = lineFromMultiOccurs(currentLine.tokens, continuation)
	return parseOccurs(tree, currentLine, nil)
}

func parseDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record, recordIndicatorIndex, nameIndex, groupIndex int) (*Record, error) {
	if currentLine.tokens[recordIndicatorIndex].value == recordDescriptionIndicator {
		return noop(tree, currentLine, rootRecord)
	}
	return parseStruct(tree, currentLine, rootRecord, nameIndex, groupIndex)
}

// parseNumDelimitedStruct parses a group header that begins with a sequence
// number (numDelimitedStructPattern: [0]seq# [1]space [2]level [3]space
// [4]name ...). The level doubles as the group key.
func parseNumDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	var (
		recordIndicatorIndex = 2
		nameIndex            = 4
		groupIndex           = 2
	)
	return parseDelimitedStruct(tree, currentLine, rootRecord, recordIndicatorIndex, nameIndex, groupIndex)
}

// parseNonNumDelimitedStruct is a function that parses a struct from a line
// that is not delimited by numbers. It checks if the line is a record description,
// and if so, it returns a noop. Otherwise, it calls parseStruct with the
// appropriate indices for the name and group.
func parseNonNumDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	var (
		recordIndicatorIndex = 1
		nameIndex            = 3
		groupIndex           = 1
	)
	return parseDelimitedStruct(tree, currentLine, rootRecord, recordIndicatorIndex, nameIndex, groupIndex)
}

// parseStruct is a function that creates a new Record from a line. It uses
// the provided indices to extract the name and group from the line, and sets
// the type of the Record to reflect.Struct.
func parseStruct(_ *Tree, currentLine line, _ *Record, nameIndex, groupIndex int) (*Record, error) {
	return &Record{
		Name:     currentLine.tokens[nameIndex].value,
		Typ:      reflect.Struct,
		depth:    currentLine.tokens[groupIndex].value,
		depthMap: map[string]*Record{},
	}, nil
}

// delve is a function that adds a new Record to a root Record. It checks if
// the new Record's depth is already present in the root Record's depth map.
// If it is, it adds the new Record to the parent Record. If it isn't, it adds
// the new Record to the root Record. It also updates the length of the parent
// or root Record based on the length and occurrence of the new Record.
func delve(tree *Tree, rootRecord *Record, newRecord *Record) (*Record, error) {
	parentRecord, seenGroup := rootRecord.depthMap[newRecord.depth]
	if seenGroup {
		parentRecord.Children = append(parentRecord.Children, newRecord)
		if len(newRecord.depthMap) == 0 {
			copyDepthMap(parentRecord, newRecord)
		}
		if err := tree.parseLines(newRecord); err != nil {
			return nil, err
		}
		length := newRecord.Length
		if newRecord.Occurs > 0 {
			length *= newRecord.Occurs
		}
		parentRecord.Length += length
		return parentRecord, nil
	}
	if rootRecord.depthMap == nil {
		rootRecord.depthMap = make(map[string]*Record)
	}

	rootRecord.depthMap[newRecord.depth] = rootRecord
	copyDepthMap(rootRecord, newRecord)
	rootRecord.Children = append(rootRecord.Children, newRecord)

	if err := tree.parseLines(newRecord); err != nil {
		return nil, err
	}

	length := newRecord.Length
	if newRecord.Occurs > 0 {
		length *= newRecord.Occurs
	}
	rootRecord.Length += length
	return newRecord, nil
}

// copyDepthMap is a function that copies the depth map from a source Record
// to a destination Record. If the destination Record's depth map is nil or
// empty, it creates a new depth map for it. It then iterates over the source
// Record's depth map and copies each entry to the destination Record's depth map.
func copyDepthMap(sourceRecord, destinationRecord *Record) {
	if len(sourceRecord.depthMap) > 0 {
		if destinationRecord.depthMap == nil {
			destinationRecord.depthMap = make(map[string]*Record)
		}
		for depthKey, record := range sourceRecord.depthMap {
			destinationRecord.depthMap[depthKey] = record
		}
	}
}
