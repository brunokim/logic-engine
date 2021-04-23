package test_helpers

import (
	"github.com/brunokim/logic-engine/logic"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

var (
    // go-cmp Option to ignore logic term's unexported fields for comparison.
	IgnoreUnexported = cmp.Options{
		cmpopts.IgnoreUnexported(logic.Comp{}),
		cmpopts.IgnoreUnexported(logic.List{}),
		cmpopts.IgnoreUnexported(logic.Assoc{}),
		cmpopts.IgnoreUnexported(logic.Dict{}),
		cmpopts.IgnoreUnexported(logic.Clause{}),
	}
)
