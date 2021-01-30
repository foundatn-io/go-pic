package lex

const (
	itemTypeSize = 13
)

// Trie ...
type Trie struct {
	root *Node
}

// Node ...
type Node struct {
	children [itemTypeSize]*Node
	isEnd    *datum
}

// NewTrie ...
func NewTrie() *Trie {
	return &Trie{root: &Node{}}
}

// Insert will take a word and add it to the trie
func (t *Trie) Insert(word fingerprint, p parser, typ lineType) {
	cur := t.root
	for i := 0; i < len(word); i++ {
		// if the itemType is not in the children...
		if cur.children[word[i]] == nil {
			// ... add it to the children
			cur.children[word[i]] = &Node{}
		}
		// iterate, adding each character in the word to the new
		// node (or existing node of the previous character)
		cur = cur.children[word[i]]
	}
	cur.isEnd = &datum{
		fp:  word,
		fn:  p,
		typ: typ,
	}
}

// Search will search for the given word
func (t *Trie) Search(word fingerprint) *datum { // nolint:golint
	cur := t.root
	for i := 0; i < len(word); i++ {
		// if the itemType is not in the children...
		if cur.children[word[i]] == nil {
			// ... then the word does not exist
			return nil
		}
		// iterate, adding each character in the word to the new
		// node (or existing node of the previous character)
		cur = cur.children[word[i]]
	}

	return cur.isEnd
}
