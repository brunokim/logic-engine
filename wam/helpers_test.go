package wam_test

import (
	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/wam"
)

const (
	assoc_pair = wam.AssocPair
	list_pair  = wam.ListPair
	dict_pair  = wam.DictPair
)

type (
	functor  = wam.Functor
	reg      = wam.RegAddr
	stack    = wam.StackAddr
	instr    = wam.InstrAddr
	constant = wam.Constant
	watom    = wam.WAtom
	wint     = wam.WInt
	wptr     = wam.WPtr
)

var (
	atom      = dsl.Atom
	int_      = dsl.Int
	ptr       = dsl.Ptr
	comp      = dsl.Comp
	ilist     = dsl.IList
	indicator = dsl.Indicator
	list      = dsl.List
	var_      = dsl.Var
	svar      = dsl.SVar
	dict      = dsl.Dict
	idict     = dsl.IDict
	assoc     = dsl.Assoc
)
