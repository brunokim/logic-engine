package logic_test

import (
	"fmt"

	. "github.com/brunokim/logic-engine/logic"
)

func ExampleAtom() {
	fmt.Println(Atom{"a123"}, Atom{"space-> <-"}, Atom{"Upper"}, Atom{"123"})
	// Output: a123 'space-> <-' 'Upper' '123'
}
