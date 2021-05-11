package wam

import (
	"encoding/json"
	"fmt"
	"reflect"
	"sort"
)

func (i RegAddr) MarshalText() ([]byte, error) {
	return []byte(i.String()), nil
}

func (i StackAddr) MarshalText() ([]byte, error) {
	return []byte(i.String()), nil
}

func (i ConstantAddr) MarshalText() ([]byte, error) {
	return []byte(i.String()), nil
}

func (f Functor) MarshalText() ([]byte, error) {
	return []byte(f.String()), nil
}

func (c WAtom) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (c WInt) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (c WPtr) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (c *Ref) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (c *Struct) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (c *Pair) MarshalText() ([]byte, error) {
	return []byte(c.String()), nil
}

func (t ExecutionMode) MarshalText() ([]byte, error) {
	return []byte(t.String()), nil
}

func (t UnificationMode) MarshalText() ([]byte, error) {
	return []byte(t.String()), nil
}

func (t PairTag) MarshalText() ([]byte, error) {
	return []byte(t.String()), nil
}

func (m *Machine) MarshalJSON() ([]byte, error) {
	enc := newMachineEncoder(m)
	var clauses []interface{}
	if m.shouldEncodeClauses {
		clauses = enc.clauses_()
	}
	obj := map[string]interface{}{
		"Mode":         m.Mode,
		"Clauses":      clauses,
		"CodePtr":      enc.instrAddr(m.CodePtr),
		"Continuation": enc.instrAddr(m.Continuation),
		"Reg":          m.Reg,
		"ComplexArg":   m.ComplexArg,
		"UnifFrames":   enc.unifFrames_(),
		"EnvPos":       enc.getEnvPos(m.Env),
		"Envs":         enc.envs_(),
		"ChoicePos":    enc.getChoicePos(m.ChoicePoint),
		"ChoicePoints": enc.choices_(),
		"CutChoicePos": enc.getChoicePos(m.CutChoice),
		"LastRefID":    m.LastRefID,
		"Backtracked":  m.hasBacktracked,
		"Attributes":   enc.attributes_(enc.attributes),
	}
	return json.Marshal(obj)
}

type attribute struct {
	refID int
	name  string
	value Cell
}

type machineEncoder struct {
	clausePos  map[*Clause]int
	envPos     map[*Env]int
	choicePos  map[*ChoicePoint]int
	clauses    []*Clause
	envs       []*Env
	choices    []*ChoicePoint
	unifFrames []*UnificationFrame
	attributes []attribute
}

func newMachineEncoder(m *Machine) *machineEncoder {
	enc := new(machineEncoder)
	// Clauses
	if m.encoder != nil {
		enc.clausePos = m.encoder.clausePos
		enc.clauses = m.encoder.clauses
	} else {
		clauses := make([]*Clause, len(m.Code))
		i := 0
		for _, clause := range m.Code {
			clauses[i] = clause
			i++
		}
		enc.clausePos = clausePositions(clauses)
		enc.clauses = make([]*Clause, len(enc.clausePos))
		for clause, i := range enc.clausePos {
			enc.clauses[i] = clause
		}
	}
	// Choices
	enc.choicePos = choicePositions(m.ChoicePoint)
	enc.choices = make([]*ChoicePoint, len(enc.choicePos))
	for choice, i := range enc.choicePos {
		enc.choices[i] = choice
	}
	// Envs
	envs := []*Env{m.Env}
	for _, choice := range enc.choices {
		envs = append(envs, choice.Env)
	}
	enc.envPos = envPositions(envs)
	enc.envs = make([]*Env, len(enc.envPos))
	for env, i := range enc.envPos {
		enc.envs[i] = env
	}
	// Unification frames
	var frames []*UnificationFrame
	frame := m.UnificationFrame
	for frame != nil {
		frames = append(frames, frame)
		frame = frame.Prev
	}
	enc.unifFrames = frames
	// Attributes
	var attributes []attribute
	for refID, attrs := range m.attributes {
		for name, attr := range attrs {
			attributes = append(attributes, attribute{refID, name, attr})
		}
	}
	enc.attributes = attributes
	m.encoder = enc
	return enc
}

func (enc *machineEncoder) getEnvPos(env *Env) interface{} {
	pos, ok := enc.envPos[env]
	if !ok {
		return nil
	}
	return pos
}

func (enc *machineEncoder) getChoicePos(choice *ChoicePoint) interface{} {
	pos, ok := enc.choicePos[choice]
	if !ok {
		return nil
	}
	return pos
}

func (enc *machineEncoder) instrAddr(ins InstrAddr) interface{} {
	if ins.Clause == nil {
		return nil
	}
	var ref string
	if ins.Pos >= 0 {
		ref = fmt.Sprintf("pos %d", ins.Pos)
	} else {
		ref = fmt.Sprintf("label %d", -ins.Pos)
	}
	return map[string]interface{}{
		"ClausePos": enc.clausePos[ins.Clause],
		"Pos":       ins.pos(),
		"Ref":       ref,
	}
}

func clausePositions(stack []*Clause) map[*Clause]int {
	idxs := make(map[*Clause]int)
	id := 0
	for len(stack) > 0 {
		var clause *Clause
		clause, stack = stack[0], stack[1:]
		if _, ok := idxs[clause]; ok || clause == nil {
			continue
		}
		idxs[clause] = id
		id++
		for _, instr := range clause.Code {
			stack = append(clausePtrs(instr), stack...)
		}
	}
	return idxs
}

func clausePtrs(instr Instruction) []*Clause {
	switch instr := instr.(type) {
	case tryMeElse:
		return []*Clause{instr.Alternative.Clause}
	case retryMeElse:
		return []*Clause{instr.Alternative.Clause}
	case try:
		return []*Clause{instr.Continuation.Clause}
	case retry:
		return []*Clause{instr.Continuation.Clause}
	case trust:
		return []*Clause{instr.Continuation.Clause}
	case switchOnTerm:
		return []*Clause{
			instr.IfVar.Clause,
			instr.IfConstant.Clause,
			instr.IfList.Clause,
			instr.IfAssoc.Clause,
			instr.IfDict.Clause,
			instr.IfStruct.Clause}
	case switchOnConstant:
		clauses := make([]*Clause, len(instr.Continuation))
		i := 0
		for _, instrAddr := range instr.Continuation {
			clauses[i] = instrAddr.Clause
			i++
		}
		return clauses
	case switchOnStruct:
		clauses := make([]*Clause, len(instr.Continuation))
		i := 0
		for _, instrAddr := range instr.Continuation {
			clauses[i] = instrAddr.Clause
			i++
		}
		return clauses
	}
	return nil
}

func choicePositions(choicePoint *ChoicePoint) map[*ChoicePoint]int {
	id := 0
	m := make(map[*ChoicePoint]int)
	for choicePoint != nil {
		m[choicePoint] = id
		id++
		choicePoint = choicePoint.Prev
	}
	return m
}

func envPositions(stack []*Env) map[*Env]int {
	idxs := make(map[*Env]int)
	id := 0
	for len(stack) > 0 {
		var env *Env
		env, stack = stack[0], stack[1:]
		if _, ok := idxs[env]; ok || env == nil {
			continue
		}
		idxs[env] = id
		id++
		stack = append([]*Env{env.Prev}, stack...)
	}
	return idxs
}

func (enc *machineEncoder) clauses_() []interface{} {
	s := make([]interface{}, len(enc.clauses))
	for i, clause := range enc.clauses {
		s[i] = map[string]interface{}{
			"Functor":      clause.Functor,
			"NumRegisters": clause.NumRegisters,
			"Code":         enc.instructions(clause.Code),
		}
	}
	return s
}

func (enc *machineEncoder) instructions(ins []Instruction) []interface{} {
	s := make([]interface{}, len(ins))
	for i, instr := range ins {
		m := make(map[string]interface{})
		v := reflect.ValueOf(instr)
		t := v.Type()
		m["Type"] = t.Name()
		for k := 0; k < v.NumField(); k++ {
			field := t.Field(k)
			value := v.Field(k).Interface()
			m[field.Name] = enc.instructionField(value)
		}
		s[i] = m
	}
	return s
}

func (enc *machineEncoder) instructionField(v interface{}) interface{} {
	switch v := v.(type) {
	default:
		return v
	case InstrAddr:
		return enc.instrAddr(v)
	case map[Constant]InstrAddr:
		im := make(map[string]interface{})
		for key, instrAddr := range v {
			im[key.String()] = enc.instrAddr(instrAddr)
		}
		return im
	case map[Functor]InstrAddr:
		im := make(map[Functor]interface{})
		for key, instrAddr := range v {
			im[key] = enc.instrAddr(instrAddr)
		}
		return im
	case func(*Machine, []Addr) error:
		return funcName(v)
	case map[int]map[string]Cell:
		im := make(map[string]interface{})
		for key, index := range v {
			x := (&Ref{id: key}).String()
			im[x] = index
		}
		return im
	}
}

func (enc *machineEncoder) envs_() []interface{} {
	s := make([]interface{}, len(enc.envs))
	for i, env := range enc.envs {
		s[i] = map[string]interface{}{
			"PrevPos":       enc.getEnvPos(env.Prev),
			"Continuation":  enc.instrAddr(env.Continuation),
			"PermanentVars": env.PermanentVars,
			"CutChoicePos":  enc.getChoicePos(env.CutChoice),
		}
	}
	return s
}

func (enc *machineEncoder) choices_() []interface{} {
	s := make([]interface{}, len(enc.choices))
	for i, choice := range enc.choices {
		s[i] = map[string]interface{}{
			"PrevPos":         enc.getChoicePos(choice.Prev),
			"NextAlternative": enc.instrAddr(choice.NextAlternative),
			"Args":            choice.Args,
			"Trail":           enc.trail(choice.Trail),
			"Attributes":      enc.attrTrail(choice.AttrTrail),
			"LastRefID":       choice.LastRefID,
			"EnvPos":          enc.getEnvPos(choice.Env),
			"CutChoicePos":    enc.getChoicePos(choice.CutChoice),
			"Continuation":    enc.instrAddr(choice.Continuation),
		}
	}
	return s
}

func (enc *machineEncoder) unifFrames_() []interface{} {
	s := make([]interface{}, len(enc.unifFrames))
	for i, frame := range enc.unifFrames {
		m := map[string]interface{}{
			"Attributes": frame.Attributes,
		}
		if frame.Index < len(frame.Bindings) {
			m["AttributedRef"] = frame.Bindings[frame.Index].Ref
			m["BindingValue"] = frame.Bindings[frame.Index].Value
			m["Bindings"] = frame.Bindings[frame.Index+1:]
		}
		s[i] = m
	}
	return s
}

func (enc *machineEncoder) trail(refs []*Ref) []interface{} {
	s := make([]interface{}, len(refs))
	for i, ref := range refs {
		s[i] = map[string]interface{}{
			"Id":   ref.id,
			"Term": ref.String(),
		}
	}
	return s
}

func (enc *machineEncoder) attrTrail(trail map[*Ref]map[string]Cell) []interface{} {
	var as []attribute
	for x, attrs := range trail {
		for name, attr := range attrs {
			as = append(as, attribute{x.id, name, attr})
		}
	}
	return enc.attributes_(as)
}

func (enc *machineEncoder) attributes_(as []attribute) []interface{} {
	sort.Slice(as, func(i, j int) bool {
		a1, a2 := as[i], as[j]
		if a1.refID < a2.refID {
			return true
		}
		if a1.refID > a2.refID {
			return false
		}
		if a1.name < a2.name {
			return true
		}
		if a1.name > a2.name {
			return false
		}
		// Should never happen: duplicate attribute
		return false
	})
	s := make([]interface{}, len(as))
	for i, attr := range as {
		s[i] = map[string]interface{}{
			"Id":        attr.refID,
			"Attribute": attr.name,
			"Value":     attr.value,
		}
	}
	return s
}
