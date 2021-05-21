package parser

import (
	_ "embed"
	"testing"

	"github.com/brunokim/logic-engine/test_helpers"

	"github.com/google/go-cmp/cmp"
)

//go:embed parser.pl
var text string

func TestParseSelf(t *testing.T) {
	got, err := ParseClauses(text)
	if err != nil {
		t.Fatalf("got err: %v", err)
	}
	if diff := cmp.Diff(grammar, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("-want, +got:\n%s", diff)
	}
}
