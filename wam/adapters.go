package wam

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
)

func toFunctor(indicator logic.Indicator) Functor {
	return Functor{Name: indicator.Name, Arity: indicator.Arity}
}

func toConstant(term logic.Term) Constant {
	switch t := term.(type) {
	case logic.Atom:
		return WAtom(t.Name)
	case logic.Int:
		return WInt(t.Value)
	default:
		panic(fmt.Sprintf("toConstant: unhandled type %T (%v)", term, term))
	}
}

func fromConstant(c Constant) logic.Term {
	switch c := c.(type) {
	case WAtom:
		return logic.Atom{Name: string(c)}
	case WInt:
		return logic.Int{Value: int(c)}
	default:
		panic(fmt.Sprintf("fromConstant: unhandled type %T (%v)", c, c))
	}
}

func fromCells(xs []logic.Var, cells []Cell) map[logic.Var]logic.Term {
	ctx := new(convertCtx)
	ctx.parents = make(map[Cell]struct{})
	ctx.looping = make(map[Cell]logic.Var)
	ctx.bindings = make(map[logic.Var]logic.Term)
	for i, x := range xs {
		ctx.looping[cells[i]] = x
	}
	for _, cell := range cells {
		ctx.fromCell(cell)
	}
	return ctx.bindings
}

type convertCtx struct {
	id       int
	parents  map[Cell]struct{}
	looping  map[Cell]logic.Var
	bindings map[logic.Var]logic.Term
}

func (ctx *convertCtx) newVar() logic.Var {
	ctx.id++
	x := logic.NewVar("_S").WithSuffix(ctx.id)
	return x
}

func (ctx *convertCtx) fromCell(cell Cell) logic.Term {
	if c, ok := cell.(Constant); ok {
		return fromConstant(c)
	}
	// 1. Cell already appears as a parent of itself, create and return a var.
	if _, ok := ctx.parents[cell]; ok {
		x, ok := ctx.looping[cell]
		if !ok {
			x = ctx.newVar()
			ctx.looping[cell] = x
		}
		return x
	}
	// 2. Build complex term
	term := ctx.complexTerm(cell)
	// 3. Cell was referenced within children, returns the var and add itself
	// to the bindings.
	if x, ok := ctx.looping[cell]; ok {
		ctx.bindings[x] = term
		return x
	}
	return term
}

func (ctx *convertCtx) complexTerm(cell Cell) logic.Term {
	// Add cell to the parent set, and remove itself on exit.
	ctx.parents[cell] = struct{}{}
	defer delete(ctx.parents, cell)

	switch c := cell.(type) {
	case *Ref:
		if c.Cell == nil {
			return logic.NewVar("_X").WithSuffix(c.id)
		}
		return ctx.fromCell(c.Cell)
	case *Struct:
		args := make([]logic.Term, len(c.Args))
		for i, arg := range c.Args {
			args[i] = ctx.fromCell(arg)
		}
		return logic.NewComp(c.Name, args...)
	case *Pair:
		head := ctx.fromCell(c.Head)
		tail := ctx.fromCell(c.Tail)
		switch c.Tag {
		case AssocPair:
			return logic.NewAssoc(head, tail)
		case ListPair:
			return logic.NewIncompleteList([]logic.Term{head}, tail)
		case DictPair:
			assoc := head.(*logic.Assoc)
			return logic.NewIncompleteDict([]*logic.Assoc{assoc}, tail)
		default:
			panic(fmt.Sprintf("(*convertCtx).complexTerm: unhandled pair type %T (%v)", c.Tag, c))
		}
	default:
		panic(fmt.Sprintf("(*convertCtx).complexTerm: unhandled type %T (%v)", cell, cell))
	}
}
