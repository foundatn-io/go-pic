package lex

import (
	"fmt"
	"log"
	"reflect"
	"strings"
)

const (
	picPrefix = "PIC "
)

// lineParser is a function type that takes a Tree, a line, and a Record,
// and returns a new Record. It is used to parse different types of lines.
type lineParser func(tree *Tree, currentLine line, rootRecord *Record) (*Record, error)

// noop is a lineParser that logs a no-operation message and returns nil.
// It is used when the current line does not require any action.
func noop(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	log.Printf("%s on copybook line %d resulted in no-operation", currentLine.typ, tree.lineIndex)
	return nil, nil
}

// parsePIC is a lineParser that builds a Record from a PIC definition line.
// It extracts the picture number definition, calculates its length, and
// creates a new Record with this information.
//
// TODO: explain magic number 6, 4, 2
func parsePIC(_ *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := currentLine.tokens[6].value[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}
	return &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.tokens[4].value,
		Length:   length,
		depth:    currentLine.tokens[2].value,
		Typ:      parsePICType(pictureNumberDefinition),
	}, nil
}

// parseRedefinitions is a lineParser that builds a Record from a REDEFINES
// definition line. It extracts the picture number definition, calculates its
// length, and creates a new Record with this information. It then replaces
// the redefinition target in the root Record with the new Record.
//
// TODO: explain magic number 10, 8
func parseRedefinitions(_ *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	pictureNumberDefinition := currentLine.tokens[10].value[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}

	newRecord := &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.tokens[4].value,
		Length:   length,
		Typ:      parsePICType(pictureNumberDefinition),
	}

	target := currentLine.tokens[8].value
	destination, index, err := rootRecord.fromCache(target)
	if err != nil {
		return nil, err
	}
	if destination == nil {
		return nil, fmt.Errorf("redefinition target %s does not exist", target)
	}
	return rootRecord.redefine(index, destination, newRecord)
}

// parseGroupRedefinitions is a lineParser that builds a Record from a
// REDEFINES definition line for groups. It creates a new Record and replaces
// the redefinition target in the root Record with the new Record.
//
// TODO: explain magic numbers 8, 4, 2
func parseGroupRedefinitions(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	// Trim the trailing period from the target name, if present
	target := strings.TrimSuffix(currentLine.tokens[8].value, ".")
	destination, index, err := rootRecord.fromCache(target)
	if err != nil {
		return nil, err
	}
	if destination == nil {
		return nil, fmt.Errorf("redefinition target %s does not exist", target)
	}

	if destination.depthMap == nil || len(destination.depthMap) == 0 {
		parent, seenGroup := rootRecord.depthMap[destination.depth]
		if seenGroup {
			copyDepthMap(parent, destination)
			rootRecord = parent
		}
	}

	newRecord := &Record{
		Name:     currentLine.tokens[4].value,
		Typ:      reflect.Struct,
		depth:    currentLine.tokens[2].value,
		depthMap: destination.depthMap,
	}

	rootRecord.Length -= destination.Length

	newRecord, err = rootRecord.redefine(index, destination, newRecord)
	if err != nil {
		return nil, err
	}
	if err := tree.parseLines(newRecord); err != nil {
		return nil, err
	}
	rootRecord.Length += newRecord.Length
	return newRecord, nil
}

// parseOccurs is a lineParser that builds a Record from an OCCURS definition
// line. It extracts the picture number definition and occurrence count,
// calculates the length, and creates a new Record with this information.
//
// TODO: explain magic numbers 6, 4, 2, 8
func parseOccurs(_ *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := strings.TrimSpace(currentLine.tokens[6].value)[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}
	occurrenceCount, err := parseOccursCount(currentLine.tokens[8])
	if err != nil {
		return nil, err
	}
	return &Record{
		Name:     currentLine.tokens[4].value,
		Length:   length,
		Occurs:   occurrenceCount,
		depth:    currentLine.tokens[2].value,
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
	_, index, ok := basicParserGet(tree.currentLine.tokens)
	if !ok || !equalWord(getWord(index), multiRedefinesPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line redefinition, but failed to verify next line")
	}
	currentLine.tokens = lineFromMultiRedefines(currentLine.tokens, index)
	return parseRedefinitions(nil, currentLine, rootRecord)
}

// parseOccursMulti is a lineParser that builds a Record from a multi-line
// OCCURS definition. It reads the next line from the tree, verifies it,
// and then calls parseOccurs with the combined line.
func parseOccursMulti(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	if err := tree.nextLine(); err != nil {
		return nil, err
	}
	_, index, ok := basicParserGet(tree.currentLine.tokens)
	if !ok || !equalWord(getWord(index), multiOccursPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line occurs, but failed to verify next line: %v", tree.currentLine.tokens)
	}
	currentLine.tokens = lineFromMultiOccurs(currentLine.tokens, index)
	return parseOccurs(nil, currentLine, nil)
}

func parseDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record, recordIndicatorIndex, nameIndex, groupIndex int) (*Record, error) {
	if currentLine.tokens[recordIndicatorIndex].value == recordDescriptionIndicator {
		return noop(tree, currentLine, rootRecord)
	}
	return parseStruct(tree, currentLine, rootRecord, nameIndex, groupIndex)
}

// parseNumDelimitedStruct is a function that parses a struct from a line
// that is delimited by numbers. It checks if the line is a record description,
// and if so, it returns a noop. Otherwise, it calls parseStruct with the
// appropriate indices for the name and group.
//
// TODO: explain magic numbers 2, 4, 2
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
		if newRecord.depthMap == nil || len(newRecord.depthMap) == 0 {
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
