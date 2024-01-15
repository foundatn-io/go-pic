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
	log.Printf("%s on copybook line %d resulted in no-operation", currentLine.typ, tree.lIdx)
	return nil, nil
}

// parsePIC is a lineParser that builds a Record from a PIC definition line.
// It extracts the picture number definition, calculates its length, and
// creates a new Record with this information.
//
// TODO: explain magic number 6, 4, 2
func parsePIC(_ *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := currentLine.items[6].val[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}
	return &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.items[4].val,
		Length:   length,
		depth:    currentLine.items[2].val,
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
	pictureNumberDefinition := currentLine.items[10].val[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}

	newRecord := &Record{
		depthMap: map[string]*Record{},
		Name:     currentLine.items[4].val,
		Length:   length,
		Typ:      parsePICType(pictureNumberDefinition),
	}

	target := currentLine.items[8].val
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
	target := strings.TrimSuffix(currentLine.items[8].val, ".")
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
		Name:     currentLine.items[4].val,
		Typ:      reflect.Struct,
		depth:    currentLine.items[2].val,
		depthMap: destination.depthMap,
	}

	rootRecord.Length -= destination.Length

	newRecord, err = rootRecord.redefine(index, destination, newRecord)
	if err != nil {
		return nil, err
	}
	tree.parseLines(newRecord)
	rootRecord.Length += newRecord.Length
	return newRecord, nil
}

// parseOccurs is a lineParser that builds a Record from an OCCURS definition
// line. It extracts the picture number definition and occurrence count,
// calculates the length, and creates a new Record with this information.
//
// TODO: explain magic numbers 6, 4, 2, 8
func parseOccurs(_ *Tree, currentLine line, _ *Record) (*Record, error) {
	pictureNumberDefinition := strings.TrimSpace(currentLine.items[6].val)[len(picPrefix):]
	length, err := parsePICCount(pictureNumberDefinition)
	if err != nil {
		return nil, err
	}
	occurrenceCount, err := parseOccursCount(currentLine.items[8])
	if err != nil {
		return nil, err
	}
	return &Record{
		Name:     currentLine.items[4].val,
		Length:   length,
		Occurs:   occurrenceCount,
		depth:    currentLine.items[2].val,
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
	_, index, ok := basicParserGet(tree.line.items)
	if !ok || !equalWord(getWord(index), multiRedefinesPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line redefinition, but failed to verify next line")
	}
	currentLine.items = lineFromMultiRedefines(currentLine.items, index)
	return parseRedefinitions(nil, currentLine, rootRecord)
}

// parseOccursMulti is a lineParser that builds a Record from a multi-line
// OCCURS definition. It reads the next line from the tree, verifies it,
// and then calls parseOccurs with the combined line.
func parseOccursMulti(tree *Tree, currentLine line, _ *Record) (*Record, error) {
	if err := tree.nextLine(); err != nil {
		return nil, err
	}
	_, index, ok := basicParserGet(tree.line.items)
	if !ok || !equalWord(getWord(index), multiOccursPartPattern) {
		return nil, fmt.Errorf("parser indicated multi-line occurs, but failed to verify next line: %v", tree.line.items)
	}
	currentLine.items = lineFromMultiOccurs(currentLine.items, index)
	return parseOccurs(nil, currentLine, nil)
}

func parseDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record, recordIndicatorIndex, nameIndex, groupIndex int) (*Record, error) {
	if currentLine.items[recordIndicatorIndex].val == recordDescriptionIndicator {
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
	return parseDelimitedStruct(tree, currentLine, rootRecord, 2, 4, 2)
}

// parseNonNumDelimitedStruct is a function that parses a struct from a line
// that is not delimited by numbers. It checks if the line is a record description,
// and if so, it returns a noop. Otherwise, it calls parseStruct with the
// appropriate indices for the name and group.
//
// TODO: explain magic numbers 1, 3, 1
func parseNonNumDelimitedStruct(tree *Tree, currentLine line, rootRecord *Record) (*Record, error) {
	return parseDelimitedStruct(tree, currentLine, rootRecord, 1, 3, 1)
}

// parseStruct is a function that creates a new Record from a line. It uses
// the provided indices to extract the name and group from the line, and sets
// the type of the Record to reflect.Struct.
func parseStruct(_ *Tree, currentLine line, _ *Record, nameIndex, groupIndex int) (*Record, error) {
	return &Record{
		Name:     currentLine.items[nameIndex].val,
		Typ:      reflect.Struct,
		depth:    currentLine.items[groupIndex].val,
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
		tree.parseLines(newRecord)
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
	tree.parseLines(newRecord)

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
