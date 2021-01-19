package lex

import (
	"fmt"
	"log"
	"reflect"
	"sync"
)

type Record struct {
	Children []*Record
	Name     string
	Length   int
	Occurs   int
	Typ      reflect.Kind
	depth    string
	depthMap map[string]*Record
	cache    sync.Map
}

// toCache returns a Record, just stored into or previously loaded from the cache
func (r *Record) toCache(child *Record, idx int) *Record {
	r.cache.Store(child.Name, idx)
	return child
}

// fromCache loads a Record, by name, from the cache if present
func (r *Record) fromCache(name string) (*Record, int) {
	idx, ok := r.cache.Load(name)
	if !ok {
		return nil, 0
	}

	i, ok := idx.(int)
	if !ok {
		log.Fatalln("failed to cast cache return value to integer")
	}

	return r.Children[i], i
}

func (r *Record) redefine(target string, src *Record) *Record {
	dst, i := r.fromCache(target)
	if dst == nil {
		log.Fatalln(fmt.Sprintf("redefinition target %s does not exist", target))
	}

	r.cache.Delete(dst.Name)
	dst.Name = src.Name
	dst.Length = src.Length
	dst.Typ = src.Typ
	dst.depthMap = src.depthMap
	return r.toCache(dst, i)
}
