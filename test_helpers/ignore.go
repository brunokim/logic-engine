package test_helpers

import (
	"github.com/brunokim/logic-engine/logic"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

var (
	IgnoreUnexported = cmp.Options{
		cmpopts.IgnoreUnexported(logic.Comp{}),
		cmpopts.IgnoreUnexported(logic.List{}),
		cmpopts.IgnoreUnexported(logic.Assoc{}),
		cmpopts.IgnoreUnexported(logic.Dict{}),
		cmpopts.IgnoreUnexported(logic.Clause{}),
	}
)
