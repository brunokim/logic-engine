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

type putStruct struct {
	Functor Functor
	ArgAddr RegAddr
}

type putVariable struct {
	Addr    Addr
	ArgAddr RegAddr
}

type putValue struct {
	Addr    Addr
	ArgAddr RegAddr
}

type putConstant struct {
	Constant Constant
	ArgAddr  RegAddr
}

type putPair struct {
	Tag     PairTag
	ArgAddr RegAddr
}

type getStruct struct {
	Functor Functor
	ArgAddr RegAddr
}

type getVariable struct {
	Addr    Addr
	ArgAddr RegAddr
}

type getValue struct {
	Addr    Addr
	ArgAddr RegAddr
}

type getConstant struct {
	Constant Constant
	ArgAddr  RegAddr
}

type getPair struct {
	Tag     PairTag
	ArgAddr RegAddr
}

type unifyVariable struct {
	Addr Addr
}

type unifyValue struct {
	Addr Addr
}

type unifyConstant struct {
	Constant Constant
}

type unifyVoid struct{}

type call struct {
	Functor Functor
}

type callMeta struct {
	Addr   Addr
	Params []Addr
}

type execute struct {
	Functor Functor
}

type executeMeta struct {
	Addr   Addr
	Params []Addr
}

type proceed struct {
	Mode ExecutionMode
}

type halt struct{}

type allocate struct {
	NumVars int
}

type deallocate struct{}

type tryMeElse struct {
	Alternative InstrAddr
}

type retryMeElse struct {
	Alternative InstrAddr
}

type trustMe struct{}

type try struct {
	Continuation InstrAddr
}

type retry struct {
	Continuation InstrAddr
}

type trust struct {
	Continuation InstrAddr
}

type switchOnTerm struct {
	IfVar, IfConstant, IfStruct, IfList, IfAssoc, IfDict InstrAddr
}

type switchOnConstant struct {
	Continuation map[Constant]InstrAddr
}

type switchOnStruct struct {
	Continuation map[Functor]InstrAddr
}

type neckCut struct{}

type cut struct{}

type fail struct{}

type builtin struct {
	Name string
	Func func(m *Machine) error
}

type putAttr struct {
	Addr      Addr
	Attribute Addr
}

type getAttr struct {
	Addr      Addr
	Attribute Addr
}

func (i putStruct) isInstruction()        {}
func (i putVariable) isInstruction()      {}
func (i putValue) isInstruction()         {}
func (i putConstant) isInstruction()      {}
func (i putPair) isInstruction()          {}
func (i getStruct) isInstruction()        {}
func (i getVariable) isInstruction()      {}
func (i getValue) isInstruction()         {}
func (i getConstant) isInstruction()      {}
func (i getPair) isInstruction()          {}
func (i unifyVariable) isInstruction()    {}
func (i unifyValue) isInstruction()       {}
func (i unifyConstant) isInstruction()    {}
func (i unifyVoid) isInstruction()        {}
func (i call) isInstruction()             {}
func (i callMeta) isInstruction()         {}
func (i execute) isInstruction()          {}
func (i executeMeta) isInstruction()      {}
func (i proceed) isInstruction()          {}
func (i halt) isInstruction()             {}
func (i allocate) isInstruction()         {}
func (i deallocate) isInstruction()       {}
func (i tryMeElse) isInstruction()        {}
func (i retryMeElse) isInstruction()      {}
func (i trustMe) isInstruction()          {}
func (i try) isInstruction()              {}
func (i retry) isInstruction()            {}
func (i trust) isInstruction()            {}
func (i switchOnTerm) isInstruction()     {}
func (i switchOnConstant) isInstruction() {}
func (i switchOnStruct) isInstruction()   {}
func (i neckCut) isInstruction()          {}
func (i cut) isInstruction()              {}
func (i fail) isInstruction()             {}
func (i builtin) isInstruction()          {}
func (i putAttr) isInstruction()          {}
func (i getAttr) isInstruction()          {}

func (i putStruct) String() string {
	return fmt.Sprintf("put_struct %v, A%d", i.Functor, i.ArgAddr)
}

func (i putVariable) String() string {
	return fmt.Sprintf("put_variable %v, A%d", i.Addr, i.ArgAddr)
}

func (i putValue) String() string {
	return fmt.Sprintf("put_value %v, A%d", i.Addr, i.ArgAddr)
}

func (i putConstant) String() string {
	return fmt.Sprintf("put_constant %v, A%d", i.Constant, i.ArgAddr)
}

func (i putPair) String() string {
	return fmt.Sprintf("put_pair %v, A%d", i.Tag, i.ArgAddr)
}

func (i getStruct) String() string {
	return fmt.Sprintf("get_struct %v, A%d", i.Functor, i.ArgAddr)
}

func (i getVariable) String() string {
	return fmt.Sprintf("get_variable %v, A%d", i.Addr, i.ArgAddr)
}

func (i getValue) String() string {
	return fmt.Sprintf("get_value %v, A%d", i.Addr, i.ArgAddr)
}

func (i getConstant) String() string {
	return fmt.Sprintf("get_constant %v, A%d", i.Constant, i.ArgAddr)
}

func (i getPair) String() string {
	return fmt.Sprintf("get_pair %v, A%d", i.Tag, i.ArgAddr)
}

func (i unifyVariable) String() string {
	return fmt.Sprintf("unify_variable %v", i.Addr)
}

func (i unifyValue) String() string {
	return fmt.Sprintf("unify_value %v", i.Addr)
}

func (i unifyConstant) String() string {
	return fmt.Sprintf("unify_constant %v", i.Constant)
}

func (i unifyVoid) String() string {
	return "unify_void"
}

func (i call) String() string {
	return fmt.Sprintf("call %v", i.Functor)
}

func (i callMeta) String() string {
	return fmt.Sprintf("call_meta %v, %v", i.Addr, i.Params)
}

func (i execute) String() string {
	return fmt.Sprintf("execute %v", i.Functor)
}

func (i executeMeta) String() string {
	return fmt.Sprintf("execute_meta %v, %v", i.Addr, i.Params)
}

func (i proceed) String() string {
	return fmt.Sprintf("proceed %v", i.Mode)
}

func (i halt) String() string {
	return "halt"
}

func (i allocate) String() string {
	return fmt.Sprintf("allocate %d", i.NumVars)
}

func (i deallocate) String() string {
	return "deallocate"
}

func (i tryMeElse) String() string {
	return fmt.Sprintf("try_me_else %v", i.Alternative)
}

func (i retryMeElse) String() string {
	return fmt.Sprintf("retry_me_else %v", i.Alternative)
}

func (i trustMe) String() string {
	return "trust_me"
}

func (i try) String() string {
	return fmt.Sprintf("try %v", i.Continuation)
}

func (i retry) String() string {
	return fmt.Sprintf("retry %v", i.Continuation)
}

func (i trust) String() string {
	return fmt.Sprintf("trust %v", i.Continuation)
}

func (i switchOnTerm) String() string {
	return fmt.Sprintf(`switch_on_term
	variable: %v
	constant: %v
	list: %v
	assoc: %v
	dict: %v
	struct: %v`, i.IfVar, i.IfConstant, i.IfList, i.IfAssoc, i.IfDict, i.IfStruct)
}

func (instr switchOnConstant) String() string {
	entries := make([]string, len(instr.Continuation))
	i := 0
	for c, cont := range instr.Continuation {
		entries[i] = fmt.Sprintf("%v: %v", c, cont)
		i++
	}
	sort.Strings(entries)
	return fmt.Sprintf("switch_on_constant\n\t%s", strings.Join(entries, "\n\t"))
}

func (instr switchOnStruct) String() string {
	entries := make([]string, len(instr.Continuation))
	i := 0
	for c, cont := range instr.Continuation {
		entries[i] = fmt.Sprintf("%v: %v", c, cont)
		i++
	}
	sort.Strings(entries)
	return fmt.Sprintf("switch_on_struct\n\t%s", strings.Join(entries, "\n\t"))
}

func (i neckCut) String() string {
	return "neck_cut"
}

func (i cut) String() string {
	return "cut"
}

func (i fail) String() string {
	return "fail"
}

func (i builtin) String() string {
	return fmt.Sprintf("builtin %s, <func %p>", i.Name, i.Func)
}

func (i putAttr) String() string {
	return fmt.Sprintf("put_attr %v, %v", i.Addr, i.Attribute)
}

func (i getAttr) String() string {
	return fmt.Sprintf("get_attr %v, %v", i.Addr, i.Attribute)
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
// Go pointer. The underlying type must be comparable.
type Constant interface {
	Cell
	isConstant()
}

type WAtom string
type WInt int
type WPtr struct {
	ptr interface{}
}

func (c WAtom) isConstant() {}
func (c WInt) isConstant()  {}
func (c WPtr) isConstant()  {}

// PairTag marks the logic type of this cons cell.
type PairTag int

//go:generate stringer -type=PairTag -linecomment
const (
	AssocPair PairTag = iota // assoc
	ListPair                 // list
	DictPair                 // dict
)

// Pair represents a cons cell, which is used to represent lists, assocs and dicts.
//
//     [a, b, c]    -> .(a, .(b, .(c, [])))
//     {a: 1, b: 2} -> .(a:1, .(b:2, {})) -> .(.(a, 1), .(.(b, 2), {}))
type Pair struct {
	Tag        PairTag
	Head, Tail Cell
}

func (c *Struct) isCell() {}
func (c *Ref) isCell()    {}
func (c *Pair) isCell()   {}
func (c WAtom) isCell()   {}
func (c WInt) isCell()    {}
func (c WPtr) isCell()    {}

// Functor returns the f/n notation of a struct.
func (c *Struct) Functor() Functor {
	return Functor{c.Name, len(c.Args)}
}

func (c WAtom) String() string {
	return logic.FormatAtom(string(c))
}

func (c WInt) String() string {
	return fmt.Sprintf("%d", c)
}

func (c WPtr) String() string {
	return fmt.Sprintf("<ptr %p>", c)
}

func (c *Ref) String() string {
	return formatCell(c)
}

func (c *Struct) String() string {
	return formatCell(c)
}

func (c *Pair) String() string {
	return formatCell(c)
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
	// Trail of variables that need to be unbound on backtrack.
	Trail []*Ref
	// Trail of attributes that were changed since this choicepoint.
	AttrTrail map[*Ref]map[string]Cell

	// Machine vars to restore
	Args         []Cell
	LastRefID    int
	Env          *Env
	CutChoice    *ChoicePoint
	Continuation InstrAddr
}

type UnificationFrame struct {
	Prev         *UnificationFrame
	Continuation InstrAddr
	CutChoice    *ChoicePoint
	Bindings     []Binding
	Index        int
	Attributes   []Cell
	FirstRun     bool
}

// ExecutionMode sets the machine mode between running code or unifying.
type ExecutionMode int

//go:generate stringer -type=ExecutionMode --linecomment
const (
	Run   ExecutionMode = iota // run
	Unify                      // unify
)

// UnificationMode sets the machine mode for unifying complex terms' args.
type UnificationMode int

//go:generate stringer -type=UnificationMode --linecomment
const (
	NoUnificationMode UnificationMode = iota // none
	Write                                    // write
	Read                                     // read
)

// ComplexArg wraps the state for unifying args of a complex term.
type ComplexArg struct {
	// Complex term to compose.
	Cell Cell

	// Whether args are being written to or read from Complex.
	Mode UnificationMode

	// Argument index within Complex. Between [0, len(Args)) for
	// *Struct, and [0, 1] for *Pair.
	Index int
}

// Binding is an entry of a value associated to a ref.
type Binding struct {
	Ref   *Ref
	Value Cell
}

// Machine represents an abstract machine state.
type Machine struct {
	//
	Mode ExecutionMode

	// Instruction list. A query is represented by an empty functor.
	Code map[Functor]*Clause

	// Current instruction index.
	CodePtr InstrAddr

	// Location to return after call.
	Continuation InstrAddr

	// Temporary cell region. Should be max of all Clause.NumRegister's.
	Reg []Cell

	// State for a complex term unification, that is accomplished over several
	// instructions.
	ComplexArg ComplexArg

	// Latest unification frame.
	UnificationFrame *UnificationFrame

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

	//
	Bindings []Binding

	// Attributes indexed by ref ID and attribute name.
	attributes map[int]map[string]Cell

	// interrupt checks for a signal that the current operation should be aborted.
	interrupt chan struct{}

	// Keep variables of original query between RunQuery/NextSolution calls.
	xs []logic.Var

	// Optimization for debugging: keep encoder between calls.
	encoder             *machineEncoder
	shouldEncodeClauses bool

	// Debugging info: annotate calls that failed.
	hasBacktracked bool
}

func asCells(xs []*Ref) []Cell {
	cs := make([]Cell, len(xs))
	for i, x := range xs {
		cs[i] = x
	}
	return cs
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
trail:%s
next_alternative: %v
args:%s
continuation:
	%v`,
		cpt, cpt.Env, cpt.LastRefID, formatCells(asCells(cpt.Trail)), cpt.NextAlternative, formatCells(cpt.Args), cpt.Continuation)
}

func (a ComplexArg) String() string {
	return fmt.Sprintf("complex_arg:{mode:%v, index:%d, cell:%v}", a.Mode, a.Index, a.Cell)
}

func (m *Machine) String() string {
	codePtr := indent(m.CodePtr.fullString())
	continuationPtr := indent(m.Continuation.fullString())
	// Registers
	regs := formatCells(m.Reg[:m.CodePtr.Clause.NumRegisters])
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
complex_arg:
	term: %v
	mode: %v
	index: %v
environments:%s
choice_points:%s`,
		m, codePtr, continuationPtr, regs, m.ComplexArg.Cell, m.ComplexArg.Mode, m.ComplexArg.Index, environments, choicePoints)
}
