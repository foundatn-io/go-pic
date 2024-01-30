package lex

import (
	"errors"
	"fmt"
	"io"
	"log"
	"reflect"
)

// Tree represents a parse tree with a lexer, tokens, lines, and a state.
type Tree struct {
	lexer        Lexer
	currentToken token
	lines        []line
	state        *Record
	currentLine  line
	lineIndex    int
}

// NewTree creates a new parse tree with the provided lexer.
func NewTree(inputLexer Lexer) *Tree {
	log.Println("Building new parse tree")
	root := &Record{Typ: reflect.Struct, Name: inputLexer.getName(), depthMap: make(map[string]*Record)}
	return &Tree{
		lexer:     inputLexer,
		state:     root,
		lineIndex: -1,
	}
}

// Parse processes the lexer tokens and returns the root record.
func (tree *Tree) Parse() (*Record, error) {
	log.Println("Parsing lexer tokens")
	for lineTokens := tree.scanLine(); tree.currentToken.kind != tokenKindEOF; lineTokens = tree.scanLine() {
		if line := buildLine(lineTokens); line != nil {
			tree.lines = append(tree.lines, *line)
		}
	}
	log.Println("Reached EOF token, input lexed.")
	if err := tree.parseLines(tree.state); err != nil {
		return nil, fmt.Errorf("error parsing lines: %w", err)
	}
	return tree.state, nil
}

// scanLine scans a line and returns its tokens.
func (tree *Tree) scanLine() []token {
	var lineTokens []token
	for tree.currentToken = tree.nextToken(); isParseableToken(tree.currentToken); tree.currentToken = tree.nextToken() {
		lineTokens = append(lineTokens, tree.currentToken)
	}
	return lineTokens
}

// nextToken returns the next token from the lexer.
func (tree *Tree) nextToken() token {
	return tree.lexer.getNext()
}

// parseLines processes the lines and adds them to the tree data.
func (tree *Tree) parseLines(rootRecord *Record) error { //nolint: gocyclo // TODO: Refactor for simplicity
	for !errors.Is(tree.nextLine(), io.EOF) {
		switch tree.currentLine.typ {
		case lineJunk, lineEnum:
			log.Printf("%s on copybook line %d resulted in no-op", tree.currentLine.typ, tree.lineIndex)
			continue
		case lineRedefines, lineMultilineRedefines, lineGroupRedefines:
			if _, err := tree.currentLine.fn(tree, tree.currentLine, rootRecord); err != nil {
				return fmt.Errorf("error redefining line: %w", err)
			}
		case lineStruct:
			record, err := tree.currentLine.fn(tree, tree.currentLine, rootRecord)
			if err != nil {
				return fmt.Errorf("error parsing line: %w", err)
			}
			if record == nil {
				continue
			}
			parentRecord, ok := rootRecord.depthMap[record.depth]
			if ok {
				rootRecord = parentRecord
			}
			if _, err := delve(tree, rootRecord, record); err != nil {
				return err
			}
		default:
			record, err := tree.currentLine.fn(tree, tree.currentLine, rootRecord)
			if err != nil {
				return fmt.Errorf("error parsing line: %w", err)
			}
			if record == nil {
				return fmt.Errorf("parser returned nil record for line: %+v", tree.currentLine)
			}
			parentRecord, ok := rootRecord.depthMap[record.depth]
			if ok {
				rootRecord = parentRecord
			}
			childIndex := len(rootRecord.Children)
			length := record.Length
			if record.Occurs > 0 {
				length *= record.Occurs
			}
			rootRecord.Length += length
			rootRecord.Children = append(rootRecord.Children, rootRecord.toCache(record, childIndex))
		}
	}
	return nil
}

// nextLine moves to the next line in the tree.
func (tree *Tree) nextLine() error {
	tree.lineIndex++
	if tree.lineIndex >= len(tree.lines) {
		return io.EOF
	}
	tree.currentLine = tree.lines[tree.lineIndex]
	return nil
}

func isParseableToken(t token) bool {
	return !(t == (token{}) || t.kind == tokenKindError || t.kind == tokenKindEOL || tokenKindEOF == t.kind)
}
