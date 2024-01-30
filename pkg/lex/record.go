package lex

import (
	"fmt"
	"reflect"
	"sync"
)

// Record represents a data structure in a hierarchical format.
// Each Record can have multiple child Records, forming a tree-like structure.
// The Record struct holds various properties:
// - Name: the identifier of the Record.
// - Length: the length of the Record.
// - Occurs: the number of times the Record occurs.
// - Typ: the type of the Record, represented as a reflect.Kind.
// - Children: a slice of pointers to child Records.
//
// The Record struct also contains several unexported fields:
// - depth: a string representing the depth of the Record in the tree.
// - depthMap: a map that associates depth strings with corresponding Records.
// - cache: a sync.Map used for caching Records for quick access.
// - mu: a sync.Mutex used for ensuring thread safety when modifying the Record.
type Record struct {
	Name     string
	Length   int
	Occurs   int
	Typ      reflect.Kind
	Children []*Record

	depth    string
	depthMap map[string]*Record
	cache    sync.Map
	mu       sync.Mutex
}

// toCache stores a child Record into the cache with a given index and returns the child Record.
// It does not check if a Record with the same name already exists in the cache.
func (r *Record) toCache(childRecord *Record, index int) *Record {
	r.cache.Store(childRecord.Name, index)
	return childRecord
}

// fromCache retrieves a Record by name from the cache.
// It returns the Record and its index if found, otherwise it returns nil and an error.
func (r *Record) fromCache(recordName string) (*Record, int, error) {
	index, ok := r.cache.Load(recordName)
	if !ok {
		return nil, 0, nil
	}
	recordIndex, ok := index.(int)
	if !ok {
		return nil, 0, fmt.Errorf("failed to cast cache return value to integer")
	}
	return r.Children[recordIndex], recordIndex, nil
}

// redefine modifies a Record (targetRecord) in place to match another Record (sourceRecord).
// It also updates the cache to reflect the changes.
// It returns the modified Record and an error if any.
func (r *Record) redefine(index int, targetRecord, sourceRecord *Record) (*Record, error) {
	r.mu.Lock()
	defer r.mu.Unlock()

	targetRecord = r.toCache(targetRecord, index)
	targetRecord.Name = sourceRecord.Name
	targetRecord.Length = sourceRecord.Length
	targetRecord.Typ = sourceRecord.Typ
	targetRecord.depthMap = sourceRecord.depthMap
	return targetRecord, nil
}
