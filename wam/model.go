// Package wam implements an interpreter for a Warren Abstract Machine.
//
// The WAM is a specification to implement a register-based Prolog machine,
// that enjoys good performance and ease of translation to machine code.
//
// The literal specification expects a huge area of contiguous memory to
// contain the machine's stack, heap and registers. We sidestep this model
// to make use of regular Go pointers whenever possible, leveraging the
// runtime's garbage collector.
//
// The machine is composed of a list of registers and two stacks: the 
// environment (or AND-)stack, that stores local variables of function calls,
// and the choicepoint (or OR-)stack, that stores the sequence of possible
// alternate steps to take on failure.
//
// Learn more in "Warren’s Abstract Machine: A tutorial reconstrution", Hassan Aït-Kici
package wam

import (
	"fmt"
	"sort"
	"strings"

    "github.com/brunokim/logic-engine/logic"
)

// ---- Address types

// Addr represents an address within the machine's memory.
type Addr interface {
	fmt.Stringer
	isAddr()
}

// RegAddr is the index of a machine register.
type RegAddr int

// StackAddr is the index of a local variable in the current environment.
type StackAddr int

func (a RegAddr) isAddr()   {}
func (a StackAddr) isAddr() {}

func (a RegAddr) String() string   { return fmt.Sprintf("X%d", a) }
func (a StackAddr) String() string { return fmt.Sprintf("Y%d", a) }

// ---- Basic types

// Functor represents a functor's name and arity.
type Functor struct {
	Name  string
	Arity int
}

func (i Functor) String() string {
	return fmt.Sprintf("%s/%d", i.Name, i.Arity)
}

// ---- Instructions

// Instruction represents an instruction of the abstract machine.
type Instruction interface {
	fmt.Stringer
	isInstruction()
}

// PutStruct instruction: put_struct <f/n>, <reg X>
type PutStruct struct {
	Functor Functor
	ArgAddr RegAddr
}

// PutVariable instruction: put_variable <addr>, <reg X>
type PutVariable struct {
	Addr    Addr
	ArgAddr RegAddr
}

// PutValue instruction: put_value <addr>, <reg X>
type PutValue struct {
	Addr    Addr
	ArgAddr RegAddr
}

// PutConstant instruction: put_constant <const>, <reg X>
type PutConstant struct {
	Constant *Constant
	ArgAddr  RegAddr
}

// PutList instruction: put_list <reg X>
type PutList struct {
	ArgAddr RegAddr
}

// GetStruct instruction: get_struct <f/n>, <reg X>
type GetStruct struct {
	Functor Functor
	ArgAddr RegAddr
}

// GetVariable instruction: get_variable <addr>, <reg X>
type GetVariable struct {
	Addr    Addr
	ArgAddr RegAddr
}

// GetValue instruction: get_value <addr>, <reg X>
type GetValue struct {
	Addr    Addr
	ArgAddr RegAddr
}

// GetConstant instruction: get_constant <const>, <reg X>
type GetConstant struct {
	Constant *Constant
	ArgAddr  RegAddr
}

// GetList instruction: get_list <reg X>
type GetList struct {
	ArgAddr RegAddr
}

// SetVariable instruction: set_variable <addr>
type SetVariable struct {
	Addr Addr
}

// SetValue instruction: set_value <addr>
type SetValue struct {
	Addr Addr
}

// SetConstant instruction: set_constant <const>
type SetConstant struct {
	Constant *Constant
}

// SetVoid instruction: set_void <n>
type SetVoid struct {
	NumVars int
}

// UnifyVariable instruction: unify_variable <addr>
type UnifyVariable struct {
	Addr Addr
}

// UnifyValue instruction: unify_value <addr>
type UnifyValue struct {
	Addr Addr
}

// UnifyConstant instruction: unify_constant <const>
type UnifyConstant struct {
	Constant *Constant
}

// UnifyVoid instruction: unify_void <n>
type UnifyVoid struct {
	NumVars int
}

// Call instruction: call <f/n>
type Call struct {
	Functor Functor
}

// CallMeta instruction: call_meta <addr>, [<addr1>, <addr2>, <addr3>]
type CallMeta struct {
	Addr   Addr
	Params []Addr
}

// Execute instruction: execute <f/n>
type Execute struct {
	Functor Functor
}

// ExecuteMeta instruction: execute_meta <addr>, [<addr1>, <addr2>, <addr3>]
type ExecuteMeta struct {
	Addr   Addr
	Params []Addr
}

// Proceed instruction: proceed
type Proceed struct{}

// Halt instruction: halt
type Halt struct{}

// Allocate instruction: allocate <n>
type Allocate struct {
	NumVars int
}

// Deallocate instruction: deallocate
type Deallocate struct{}

// TryMeElse instruction: try_me_else <instr i>
type TryMeElse struct {
	Alternative InstrAddr
}

// RetryMeElse instruction: retry_me_else <instr i>
type RetryMeElse struct {
	Alternative InstrAddr
}

// TrustMe instruction: trust_me
type TrustMe struct{}

// Try instruction: try <instr i>
type Try struct {
	Continuation InstrAddr
}

// Retry instruction: retry <instr i>
type Retry struct {
	Continuation InstrAddr
}

// Trust instruction: trust <instr i>
type Trust struct {
	Continuation InstrAddr
}

// SwitchOnTerm instruction: switch_on_term <instr ifVar> <instr ifConst> <instr ifList> <instr ifStruct>
type SwitchOnTerm struct {
	IfVar, IfConstant, IfList, IfStruct InstrAddr
}

// SwitchOnConstant instruction: switch_on_constant map{"p": <instr i1>, "q": <instr i2>}
type SwitchOnConstant struct {
	Continuation map[string]InstrAddr
}

// SwitchOnStruct instruction: switch_on_constant map{"f/1": <instr i1>, "f/2": <instr i2>}
type SwitchOnStruct struct {
	Continuation map[Functor]InstrAddr
}

// NeckCut instruction: neck_cut
type NeckCut struct{}

// Cut instruction: cut
type Cut struct{}

// Fail instruction: fail
type Fail struct{}

func (i PutStruct) isInstruction()        {}
func (i PutVariable) isInstruction()      {}
func (i PutValue) isInstruction()         {}
func (i PutConstant) isInstruction()      {}
func (i PutList) isInstruction()          {}
func (i GetStruct) isInstruction()        {}
func (i GetVariable) isInstruction()      {}
func (i GetValue) isInstruction()         {}
func (i GetConstant) isInstruction()      {}
func (i GetList) isInstruction()          {}
func (i SetVariable) isInstruction()      {}
func (i SetValue) isInstruction()         {}
func (i SetConstant) isInstruction()      {}
func (i SetVoid) isInstruction()          {}
func (i UnifyVariable) isInstruction()    {}
func (i UnifyValue) isInstruction()       {}
func (i UnifyConstant) isInstruction()    {}
func (i UnifyVoid) isInstruction()        {}
func (i Call) isInstruction()             {}
func (i CallMeta) isInstruction()         {}
func (i Execute) isInstruction()          {}
func (i ExecuteMeta) isInstruction()      {}
func (i Proceed) isInstruction()          {}
func (i Halt) isInstruction()             {}
func (i Allocate) isInstruction()         {}
func (i Deallocate) isInstruction()       {}
func (i TryMeElse) isInstruction()        {}
func (i RetryMeElse) isInstruction()      {}
func (i TrustMe) isInstruction()          {}
func (i Try) isInstruction()              {}
func (i Retry) isInstruction()            {}
func (i Trust) isInstruction()            {}
func (i SwitchOnTerm) isInstruction()     {}
func (i SwitchOnConstant) isInstruction() {}
func (i SwitchOnStruct) isInstruction()   {}
func (i NeckCut) isInstruction()          {}
func (i Cut) isInstruction()              {}
func (i Fail) isInstruction()             {}

func (i PutStruct) String() string {
	return fmt.Sprintf("put_struct %v, A%d", i.Functor, i.ArgAddr)
}

func (i PutVariable) String() string {
	return fmt.Sprintf("put_variable %v, A%d", i.Addr, i.ArgAddr)
}

func (i PutValue) String() string {
	return fmt.Sprintf("put_value %v, A%d", i.Addr, i.ArgAddr)
}

func (i PutConstant) String() string {
	return fmt.Sprintf("put_constant %v, A%d", i.Constant, i.ArgAddr)
}

func (i PutList) String() string {
	return fmt.Sprintf("put_list A%d", i.ArgAddr)
}

func (i GetStruct) String() string {
	return fmt.Sprintf("get_struct %v, A%d", i.Functor, i.ArgAddr)
}

func (i GetVariable) String() string {
	return fmt.Sprintf("get_variable %v, A%d", i.Addr, i.ArgAddr)
}

func (i GetValue) String() string {
	return fmt.Sprintf("get_value %v, A%d", i.Addr, i.ArgAddr)
}

func (i GetConstant) String() string {
	return fmt.Sprintf("get_constant %v, A%d", i.Constant, i.ArgAddr)
}

func (i GetList) String() string {
	return fmt.Sprintf("get_list A%d", i.ArgAddr)
}

func (i SetVariable) String() string {
	return fmt.Sprintf("set_variable %v", i.Addr)
}

func (i SetValue) String() string {
	return fmt.Sprintf("set_value %v", i.Addr)
}

func (i SetConstant) String() string {
	return fmt.Sprintf("set_constant %v", i.Constant)
}

func (i SetVoid) String() string {
	return fmt.Sprintf("set_void %d", i.NumVars)
}

func (i UnifyVariable) String() string {
	return fmt.Sprintf("unify_variable %v", i.Addr)
}

func (i UnifyValue) String() string {
	return fmt.Sprintf("unify_value %v", i.Addr)
}

func (i UnifyConstant) String() string {
	return fmt.Sprintf("unify_constant %v", i.Constant)
}

func (i UnifyVoid) String() string {
	return fmt.Sprintf("unify_void %d", i.NumVars)
}

func (i Call) String() string {
	return fmt.Sprintf("call %v", i.Functor)
}

func (i CallMeta) String() string {
	return fmt.Sprintf("call_meta %v, %v", i.Addr, i.Params)
}

func (i Execute) String() string {
	return fmt.Sprintf("execute %v", i.Functor)
}

func (i ExecuteMeta) String() string {
	return fmt.Sprintf("execute_meta %v, %v", i.Addr, i.Params)
}

func (i Proceed) String() string {
	return "proceed"
}

func (i Halt) String() string {
	return "halt"
}

func (i Allocate) String() string {
	return fmt.Sprintf("allocate %d", i.NumVars)
}

func (i Deallocate) String() string {
	return "deallocate"
}

func (i TryMeElse) String() string {
	return fmt.Sprintf("try_me_else %v", i.Alternative)
}

func (i RetryMeElse) String() string {
	return fmt.Sprintf("retry_me_else %v", i.Alternative)
}

func (i TrustMe) String() string {
	return "trust_me"
}

func (i Try) String() string {
	return fmt.Sprintf("try %v", i.Continuation)
}

func (i Retry) String() string {
	return fmt.Sprintf("retry %v", i.Continuation)
}

func (i Trust) String() string {
	return fmt.Sprintf("trust %v", i.Continuation)
}

func (i SwitchOnTerm) String() string {
	return fmt.Sprintf(`switch_on_term
	variable: %v
	constant: %v
	list: %v
	struct: %v`, i.IfVar, i.IfConstant, i.IfList, i.IfStruct)
}

func (instr SwitchOnConstant) String() string {
	entries := make([]string, len(instr.Continuation))
	i := 0
	for c, cont := range instr.Continuation {
		entries[i] = fmt.Sprintf("%v: %v", c, cont)
		i++
	}
	sort.Strings(entries)
	return fmt.Sprintf("switch_on_constant\n\t%s", strings.Join(entries, "\n\t"))
}

func (instr SwitchOnStruct) String() string {
	entries := make([]string, len(instr.Continuation))
	i := 0
	for c, cont := range instr.Continuation {
		entries[i] = fmt.Sprintf("%v: %v", c, cont)
		i++
	}
	sort.Strings(entries)
	return fmt.Sprintf("switch_on_struct\n\t%s", strings.Join(entries, "\n\t"))
}

func (i NeckCut) String() string {
	return "neck_cut"
}

func (i Cut) String() string {
	return "cut"
}

func (i Fail) String() string {
	return "fail"
}

// ---- Clauses and code

// Clause represents a single compiled clause.
type Clause struct {
	Functor      Functor
	NumRegisters int
	Code         []Instruction
}

// InstrAddr represents the address of an instruction within a clause.
type InstrAddr struct {
	Clause *Clause
	Pos    int
}

func (ia InstrAddr) isValid() bool {
	return ia.Pos >= 0 && ia.Clause != nil && ia.Pos < len(ia.Clause.Code)
}

func (ia InstrAddr) instr() Instruction {
	if !ia.isValid() {
		return nil
	}
	return ia.Clause.Code[ia.Pos]
}

func (ia InstrAddr) inc() InstrAddr {
	return InstrAddr{ia.Clause, ia.Pos + 1}
}

func (ia InstrAddr) fullString() string {
	if !ia.isValid() {
		return ""
	}
	var b strings.Builder
	fmt.Fprintf(&b, "\n%% %v\n", ia.Clause)
	for i, instr := range ia.Clause.Code {
		n, _ := b.WriteString(instr.String())
		if i == ia.Pos {
			width := 25 - n
			if width < 0 {
				width = 0
			}
			fmt.Fprintf(&b, "%*c", width, '<')
		}
		if i < len(ia.Clause.Code)-1 {
			b.WriteRune('\n')
		}
	}
	return b.String()
}

func (c *Clause) String() string {
	return fmt.Sprintf("(%v @ %p)", c.Functor, c)
}

func (a InstrAddr) String() string {
	return fmt.Sprintf("%v[%d]", a.Clause, a.Pos)
}

// ---- Heap cells

// Cell represents a term in the Go heap.
type Cell interface {
	fmt.Stringer
	isCell()
}

// Ref represents a variable indirection. When it's unbound, the
// Cell field is nil; otherwise, it points to another heap cell.
type Ref struct {
	Cell Cell
	id   int
}

// Struct represents a compound term. 
type Struct struct {
	Name string
	Args []Cell
}

// Constant represents an immutable value, such as an atom, int or
// Go pointer.
type Constant struct {
	Value string
}

// List represents a list term as a linked list of cons cells.
//
//     [a, b, c] -> List{Atom{"a"}, List{Atom{"b"}, List{Atom{"c"}, Atom{"[]"}}}}
type List struct {
	Head, Tail Cell
}

func (c *Struct) isCell()   {}
func (c *Ref) isCell()      {}
func (c *Constant) isCell() {}
func (c *List) isCell()     {}

// Functor returns the f/n notation of a struct.
func (c *Struct) Functor() Functor {
	return Functor{c.Name, len(c.Args)}
}

func (c *Ref) String() string {
	if c.Cell == nil {
		return fmt.Sprintf("_X%d", c.id)
	}
	return fmt.Sprintf("&%v", c.Cell)
}

func (c *Struct) String() string {
	args := make([]string, len(c.Args))
	for i, arg := range c.Args {
		args[i] = fmt.Sprintf("%v", arg)
	}
	return fmt.Sprintf("%s(%s)", c.Name, strings.Join(args, ", "))
}

func (c *Constant) String() string {
	return logic.FormatAtom(c.Value)
}

// Unroll a List into a sequence of cells.
func (c *List) toSlice() ([]Cell, Cell) {
	cells, tail := []Cell{c.Head}, c.Tail
	l, ok := tail.(*List)
	for ok {
		cells = append(cells, l.Head)
		tail = l.Tail
		l, ok = tail.(*List)
	}
	return cells, tail
}

func (c *List) String() string {
	cells, tail := c.toSlice()
	args := make([]string, len(cells))
	for i, cell := range cells {
		args[i] = fmt.Sprintf("%v", cell)
	}
	body := strings.Join(args, ", ")
	if c, ok := tail.(*Constant); ok && c.Value == "[]" {
		return fmt.Sprintf("[%s]", body)
	}
	return fmt.Sprintf("[%s|%v]", body, tail)
}

// ---- Stack frames

// Env represents an AND-stack frame with the environment associated to a call.
type Env struct {
	// Previous environment.
	Prev *Env
	// Continuation pointer.
	Continuation InstrAddr
	// Permanent vars stored in stack to survive between calls.
	PermanentVars []Cell
	// Saved choice point if there's a deep cut in clause.
	CutChoice *ChoicePoint
}

// ChoicePoint represents an OR-stack frames with the state associated to an alternative code path.
type ChoicePoint struct {
	// Previous choice point.
	Prev *ChoicePoint
	// Next clause to try.
	NextAlternative InstrAddr

	// Machine vars to restore
	Args         []Cell
	TrailSize    int
	LastRefID    int
	Env          *Env
	CutChoice    *ChoicePoint
	Continuation InstrAddr
}

//go:generate stringer -type=UnificationMode

// UnificationMode is an enum for the current machine's read or write unification approach.
type UnificationMode int
const (
	Read UnificationMode = iota
	Write
)

// Machine represents an abstract machine state.
type Machine struct {
	// Instruction list. A query is represented by an empty functor.
	Code map[Functor]*Clause

	// Current instruction index.
	CodePtr InstrAddr

	// Location to return after call.
	Continuation InstrAddr

	// Temporary cell region. Should be max of all Clause.NumRegister's.
	Reg []Cell

	// Trail of variables that may need to be unbound on backtrack.
    // TODO: move this to the choicepoint themselves?
	Trail []*Ref

	// Read or write mode for term unification.
	Mode UnificationMode

	// Current compound arg being processed.
	Compound Cell
	ArgIndex int

	// Latest environment.
	Env *Env

	// Latest choice point.
	ChoicePoint *ChoicePoint

	// Choice point to restore after a cut.
	CutChoice *ChoicePoint

	// Limit of iterations to run.
	IterLimit int

	// File to output debug information.
	DebugFilename string

	// Incrementing ID to identify generated variables.
	LastRefID int

	// Optimization for debugging: keep encoder between calls.
	encoder             *machineEncoder
	shouldEncodeClauses bool

	// Debugging info: annotate calls that failed.
	hasBacktracked bool
}

func formatCells(cells []Cell) string {
	if len(cells) == 0 {
		return ""
	}
	xs := make([]string, len(cells))
	for i, cell := range cells {
		xs[i] = fmt.Sprintf("#%d: %v", i, cell)
	}
	return "\n\t" + strings.Join(xs, "\n\t")
}

func indent(s string) string {
	return "\t" + strings.ReplaceAll(s, "\n", "\n\t")
}

func (env *Env) String() string {
	return fmt.Sprintf(`%% %p
continuation:
	%v
vars:%s`,
		env, env.Continuation, formatCells(env.PermanentVars))
}

func (cpt *ChoicePoint) String() string {
	return fmt.Sprintf(`%% %p
env: %p
last_ref_id: %d
trail_size: %d
next_alternative: %v
args:%s
continuation:
	%v`,
		cpt, cpt.Env, cpt.LastRefID, cpt.TrailSize, cpt.NextAlternative, formatCells(cpt.Args), cpt.Continuation)
}

func (m *Machine) String() string {
	codePtr := indent(m.CodePtr.fullString())
	continuationPtr := indent(m.Continuation.fullString())
	// Registers
	regs := formatCells(m.Reg[:m.CodePtr.Clause.NumRegisters])
	// Trail
	refs := make([]Cell, len(m.Trail))
	for i, x := range m.Trail {
		refs[i] = x
	}
	trail := formatCells(refs)
	// Environments
	var envs []string
	env := m.Env
	for env != nil {
		envs = append(envs, indent(env.String()))
		env = env.Prev
	}
	environments := strings.Join(envs, "\n")
	// Choice points
	var cpts []string
	cpt := m.ChoicePoint
	for cpt != nil {
		cpts = append(cpts, indent(cpt.String()))
		cpt = cpt.Prev
	}
	choicePoints := strings.Join(cpts, "\n")
	// Formatting
	return fmt.Sprintf(`%% %p
clause:%s
continuation:%s
registers:%s
trail:%s
unification_mode: %s
compound_term: %v
arg_index: %d
environments:%s
choice_points:%s`,
		m, codePtr, continuationPtr, regs, trail, m.Mode, m.Compound, m.ArgIndex, environments, choicePoints)
}
