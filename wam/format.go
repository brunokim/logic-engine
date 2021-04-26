package wam

import (
	"fmt"
	"strings"
)

func formatComplex(b *strings.Builder, stack *[]interface{}, body []Cell, tail, empty Cell, open, close_ string) {
	b.WriteString(open)
	*stack = append(*stack, close_)
	if tail != empty {
		*stack = append(*stack, tail, "|")
	}
	for i := len(body) - 1; i >= 0; i-- {
		*stack = append(*stack, body[i])
		if i > 0 {
			*stack = append(*stack, ", ")
		}
	}
}

func formatCell(cell Cell) string {
	if c, ok := cell.(Constant); ok {
		return c.String()
	}
	var b strings.Builder
	seen := make(map[Cell]int)
	stack := []interface{}{cell}
	for len(stack) > 0 {
		n := len(stack)
		cell := stack[n-1]
		stack = stack[:n-1]
		if s, ok := cell.(string); ok {
			b.WriteString(s)
			continue
		}
		if cell == nil {
			b.WriteString("<nil>")
			continue
		}
		c := cell.(Cell)
		// Check for self-reference.
		if pos, ok := seen[c]; ok {
			fmt.Fprintf(&b, "_S%d", pos)
			continue
		}
		seen[c] = len(seen)
		// Cell may be a constant or a complex type.
		// Components and separators/delimiters of complex types are placed
		// in reverse order on the stack.
		switch c := c.(type) {
		case Constant:
			b.WriteString(c.String())
		case *Ref:
			if c.Cell == nil {
				fmt.Fprintf(&b, "_X%d", c.id)
				continue
			}
			b.WriteRune('&')
			stack = append(stack, c.Cell)
		case *Struct:
			b.WriteString(c.Name)
			formatComplex(&b, &stack, c.Args, nil, nil, "(", ")")
		case *Pair:
			switch c.Tag {
			case AssocPair:
				stack = append(stack, c.Tail, ":", c.Head)
			case ListPair:
				terms, tail := unroll(c)
				formatComplex(&b, &stack, terms, tail, WAtom("[]"), "[", "]")
			case DictPair:
				assocs, parent := unroll(c)
				formatComplex(&b, &stack, assocs, parent, WAtom("{}"), "{", "}")
			}
		default:
			fmt.Fprintf(&b, "<!cell %T>", c)
		}
	}
	return b.String()
}
