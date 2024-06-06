package tree_sitter_onyx_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-onyx"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_onyx.Language())
	if language == nil {
		t.Errorf("Error loading Onyx grammar")
	}
}
