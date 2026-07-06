package tree_sitter_nanoc_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_nanoc "github.com/vincent-lafouasse/nanoc/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_nanoc.Language())
	if language == nil {
		t.Errorf("Error loading NanoC grammar")
	}
}
