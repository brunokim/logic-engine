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

	put_struct         = wam.PutStruct
	put_variable       = wam.PutVariable
	put_value          = wam.PutValue
	put_constant       = wam.PutConstant
	put_pair           = wam.PutPair
	get_struct         = wam.GetStruct
	get_variable       = wam.GetVariable
	get_value          = wam.GetValue
	get_constant       = wam.GetConstant
	get_pair           = wam.GetPair
	set_variable       = wam.SetVariable
	set_value          = wam.SetValue
	set_constant       = wam.SetConstant
	set_void           = wam.SetVoid
	unify_variable     = wam.UnifyVariable
	unify_value        = wam.UnifyValue
	unify_constant     = wam.UnifyConstant
	unify_void         = wam.UnifyVoid
	call               = wam.Call
	call_meta          = wam.CallMeta
	execute            = wam.Execute
	execute_meta       = wam.ExecuteMeta
	proceed            = wam.Proceed
	halt               = wam.Halt
	allocate           = wam.Allocate
	deallocate         = wam.Deallocate
	try_me_else        = wam.TryMeElse
	retry_me_else      = wam.RetryMeElse
	trust_me           = wam.TrustMe
	try                = wam.Try
	retry              = wam.Retry
	trust              = wam.Trust
	switch_on_term     = wam.SwitchOnTerm
	switch_on_constant = wam.SwitchOnConstant
	switch_on_struct   = wam.SwitchOnStruct
	neck_cut           = wam.NeckCut
	cut                = wam.Cut
)

var (
	atom  = dsl.Atom
	int_  = dsl.Int
	comp  = dsl.Comp
	ilist = dsl.IList
	list  = dsl.List
	var_  = dsl.Var
)

func toReg(addr wam.Addr) reg {
	if r, ok := addr.(reg); ok {
		return r
	}
	return 0
}

func registers(instr wam.Instruction) (reg, reg) {
	switch i := instr.(type) {
	case put_struct:
		return 0, i.ArgAddr
	case put_variable:
		return toReg(i.Addr), i.ArgAddr
	case put_value:
		return toReg(i.Addr), i.ArgAddr
	case put_constant:
		return 0, i.ArgAddr
	case put_pair:
		return 0, i.ArgAddr
	case get_struct:
		return 0, i.ArgAddr
	case get_variable:
		return toReg(i.Addr), i.ArgAddr
	case get_value:
		return toReg(i.Addr), i.ArgAddr
	case get_constant:
		return 0, i.ArgAddr
	case get_pair:
		return 0, i.ArgAddr
	case set_variable:
		return toReg(i.Addr), 0
	case set_value:
		return toReg(i.Addr), 0
	case unify_variable:
		return toReg(i.Addr), 0
	case unify_value:
		return toReg(i.Addr), 0
	}
	return 0, 0
}

func clause(functor wam.Functor, instructions ...wam.Instruction) *wam.Clause {
	var numRegisters reg
	for _, instr := range instructions {
		reg1, reg2 := registers(instr)
		if reg1 >= numRegisters {
			numRegisters = reg1 + 1
		}
		if reg2 >= numRegisters {
			numRegisters = reg2 + 1
		}
	}
	return &wam.Clause{
		Functor:      functor,
		NumRegisters: int(numRegisters),
		Code:         instructions,
	}
}
