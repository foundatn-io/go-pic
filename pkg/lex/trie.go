package lex

// Trie is the structure in which clause/line type patterns are stored
type Trie struct {
	root *Node
}

// Node is a child in the trie. isEnd is non-nil when this node terminates a
// recognised pattern. children is indexed by tokenKind, so its size is kept
// in sync with the set of token kinds via the tokenKindCount sentinel.
type Node struct {
	children [tokenKindCount]*Node
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
