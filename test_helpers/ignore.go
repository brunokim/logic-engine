package test_helpers

import (
	"reflect"

	"github.com/brunokim/logic-engine/logic"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

var (
	IgnoreUnexported = cmp.Options{
		// Export Var's unexported field (suffix)
		cmp.Exporter(func(t reflect.Type) bool {
			return t == reflect.TypeOf(logic.Var{})
		}),
		// Ignore logic term's unexported fields for comparison (has_var_).
		cmpopts.IgnoreUnexported(logic.Comp{}),
		cmpopts.IgnoreUnexported(logic.List{}),
		cmpopts.IgnoreUnexported(logic.Assoc{}),
		cmpopts.IgnoreUnexported(logic.Dict{}),
		cmpopts.IgnoreUnexported(logic.Clause{}),
	}
)
