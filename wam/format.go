package wam

import (
	"fmt"
	"strings"
)

func formatCell(cell Cell) string {
	ctx := &formatCtx{
		b:       new(strings.Builder),
		parents: make(map[Cell]struct{}),
		loops:   make(map[Cell]string),
	}
	ctx.formatCell(cell)
	return ctx.b.String()
}

type formatCtx struct {
	b       *strings.Builder
	parents map[Cell]struct{}
	loops   map[Cell]string
	id      int
}

func (ctx *formatCtx) formatCell(cell Cell) {
	if cell == nil {
		ctx.b.WriteString("<nil>")
		return
	}
	if c, ok := cell.(Constant); ok {
		ctx.b.WriteString(c.String())
		return
	}
	// Handle self-reference.
	if _, ok := ctx.parents[cell]; ok {
		ctx.id++
		x := fmt.Sprintf("_S%d", ctx.id)
		ctx.loops[cell] = x
		ctx.b.WriteString(x)
		return
	}
	// Add cell to parent set, and remove after return.
	ctx.parents[cell] = struct{}{}
	defer delete(ctx.parents, cell)
	// Handle complex types
	switch c := cell.(type) {
	case *Ref:
		if c.Cell == nil {
			fmt.Fprintf(ctx.b, "_X%d", c.id)
			return
		}
		ctx.b.WriteString("&")
		ctx.formatCell(c.Cell)
	case *Struct:
		ctx.b.WriteString(c.Name)
		ctx.formatComplex(c.Args, nil, nil, "(", ")")
	case *Pair:
		switch c.Tag {
		case AssocPair:
			ctx.formatCell(c.Head)
			ctx.b.WriteString(":")
			ctx.formatCell(c.Tail)
		case ListPair:
			terms, tail := unroll(c)
			ctx.formatComplex(terms, tail, WAtom("[]"), "[", "]")
		case DictPair:
			assocs, parent := unroll(c)
			ctx.formatComplex(assocs, parent, WAtom("{}"), "{", "}")
		}
	}
	// Annotate parents that loop.
	if label, ok := ctx.loops[cell]; ok {
		delete(ctx.loops, cell)
		fmt.Fprintf(ctx.b, "=%s", label)
	}
}

func (ctx *formatCtx) formatComplex(body []Cell, tail, empty Cell, open, close_ string) {
	ctx.b.WriteString(open)
	for i, cell := range body {
		ctx.formatCell(cell)
		if i < len(body)-1 {
			ctx.b.WriteString(", ")
		}
	}
	if tail != empty {
		ctx.b.WriteString("|")
		ctx.formatCell(tail)
	}
	ctx.b.WriteString(close_)
}
