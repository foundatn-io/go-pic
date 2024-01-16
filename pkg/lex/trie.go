package lex

const (
	itemTypeSize = 14
)

// Trie is the structure in which clause/line type patterns are stored
type Trie struct {
	root *Node
}

// Node is a child in the trie, which may indicate whether it is the final
// token in a clause/line type pattern, returning the associated parser, word
// and lineType with that pattern.
// Otherwise, the Node contains links to children, itself, to illustrate whether
// it is part of, but not the final token of, a pattern that has been entered in
// the trie.
type Node struct {
	children [itemTypeSize]*Node
	isEnd    *entry
}

// NewTrie ...
func NewTrie() *Trie {
	return &Trie{root: &Node{}}
}

// Insert will take a word and add it to the trie
func (t *Trie) Insert(word word, p lineParser, typ lineType) {
	current := t.root
	for i := 0; i < len(word); i++ {
		// if the tokenKind is not in the children...
		if current.children[word[i]] == nil {
			// ... add it to the children
			current.children[word[i]] = &Node{}
		}
		// iterate, adding each character in the word to the new
		// node (or existing node of the previous character)
		current = current.children[word[i]]
	}
	current.isEnd = &entry{
		wordPattern: word,
		parseFunc:   p,
		lineType:    typ,
	}
}

// Search will search for the given word
func (t *Trie) Search(word word) *entry { //nolint:golint
	current := t.root
	for i := 0; i < len(word); i++ {
		// if the tokenKind is not in the children...
		if current.children[word[i]] == nil {
			// ... then the word does not exist
			return nil
		}
		// iterate, adding each character in the word to the new
		// node (or existing node of the previous character)
		current = current.children[word[i]]
	}
	return current.isEnd
}
