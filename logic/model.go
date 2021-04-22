package logic

import (
	"fmt"
	"sort"
	"strings"
)

// ---- Basic types

type Term interface {
	fmt.Stringer
	short() string
	vars(seen map[Var]struct{}, xs []Var) []Var
	hasVar() bool
}

type Atom struct {
	Name string
}

type Int struct {
	Value int
}

type Var struct {
	Name   string
	suffix int
}

type Comp struct {
	Functor string
	Args    []Term
	hasVar_ bool
}

type List struct {
	Terms   []Term
	Tail    Term
	hasVar_ bool
}

type Assoc struct {
	Key, Val Term
	hasVar_  bool
}

type AssocSet []*Assoc

type Dict struct {
	Assocs  AssocSet
	Parent  Term
	hasVar_ bool
}

type Clause struct {
	Head    Term   // May be Atom or Comp
	Body    []Term // May be Atom, Var or Comp
	hasVar_ bool
}

// ---- Public vars

var (
	AnonymousVar = NewVar("_")
	EmptyList = Atom{"[]"}
	EmptyDict = Atom{"{}"}
)

// ---- Vars

func NewVar(name string) Var {
	return Var{name, 0}
}

func (x Var) WithSuffix(suffix int) Var {
	if x.Name == "_" {
		return x
	}
	return Var{x.Name, suffix}
}

// ---- Compound terms

func NewComp(functor string, terms ...Term) *Comp {
	var hasVar bool
	for _, term := range terms {
		if term.hasVar() {
			hasVar = true
			break
		}
	}
	return &Comp{Functor: functor, Args: terms, hasVar_: hasVar}
}

type Indicator struct {
	Name  string
	Arity int
}

func (c *Comp) Indicator() Indicator {
	return Indicator{c.Functor, len(c.Args)}
}

// ---- Lists

func NewList(terms ...Term) Term {
	return NewIncompleteList(terms, EmptyList)
}

func NewIncompleteList(terms []Term, tail Term) Term {
	if len(terms) == 0 {
		return tail
	}
	if l, ok := tail.(*List); ok {
		tmp := make([]Term, len(terms)+len(l.Terms))
		copy(tmp, terms)
		copy(tmp[len(terms):], l.Terms)
		terms = tmp
		tail = l.Tail
	}
	var hasVar bool
	for _, term := range terms {
		if term.hasVar() {
			hasVar = true
			break
		}
	}
	if !hasVar {
		hasVar = tail.hasVar()
	}
	return &List{Terms: terms, Tail: tail, hasVar_: hasVar}
}

func (l *List) Slice(n int) Term {
	if n == len(l.Terms) {
		return l.Tail
	}
	if !l.hasVar_ {
		return &List{Terms: l.Terms[n:], Tail: l.Tail, hasVar_: false}
	}
	return NewIncompleteList(l.Terms[n:], l.Tail)
}

// ---- Assoc and Dict

func NewAssoc(key, val Term) *Assoc {
	return &Assoc{Key: key, Val: val, hasVar_: key.hasVar() || val.hasVar()}
}

func NewAssocSet(as []*Assoc) (AssocSet, error) {
	tmp := make(AssocSet, len(as))
	copy(tmp, as)
	sort.Slice(tmp, func(i, j int) bool { return tmp[i].Less(tmp[j]) })
	for i := 0; i < len(tmp)-1; i++ {
		if Eq(tmp[i].Key, tmp[i+1].Key) {
			return nil, fmt.Errorf("duplicate keys in AssocSet: %v", tmp[i].Key)
		}
	}
	return tmp, nil
}

func (as AssocSet) index(key Term) (int, bool) {
	i := sort.Search(len(as), func(i int) bool { return !Less(as[i].Key, key) })
	return i, i < len(as) && Eq(key, as[i].Key)
}

func (as AssocSet) addIfAbsent(a *Assoc) AssocSet {
	i, ok := as.index(a.Key)
	if ok {
		// Does not override existing keys.
		return as
	}
	as = append(as, &Assoc{})
	copy(as[i+1:], as[i:])
	as[i] = a
	return as
}

func (as AssocSet) get(key Term) Term {
	i, ok := as.index(key)
	if !ok {
		return nil
	}
	return as[i]
}

func (as AssocSet) addDifferenceSet(other AssocSet) AssocSet {
	for _, a := range other {
		as = as.addIfAbsent(a)
	}
	return as
}

func intersectionAndDifferences(as1, as2 AssocSet) ([]Term, AssocSet, AssocSet) {
	var keys []Term
	var diff1, diff2 AssocSet
	var i, j int
	for i < len(as1) && j < len(as2) {
		a1, a2 := as1[i], as2[j]
		switch compare(a1.Key, a2.Key) {
		case equal:
			keys = append(keys, a1.Key)
			i++
			j++
		case less:
			diff1 = append(diff1, a1)
			i++
		case more:
			diff2 = append(diff2, a2)
			j++
		}
	}
	if i < len(as1) {
		diff1 = append(diff1, as1[i:]...)
	}
	if j < len(as2) {
		diff2 = append(diff2, as2[j:]...)
	}
	return keys, diff1, diff2
}

// ---- Dicts

func NewDict(assocs ...*Assoc) Term {
	return NewIncompleteDict(assocs, EmptyDict)
}

func NewIncompleteDict(assocs []*Assoc, parent Term) Term {
	if len(assocs) == 0 {
		return parent
	}
	assocSet, err := NewAssocSet(assocs)
	if err != nil {
		panic(err)
	}
	if d, ok := parent.(*Dict); ok {
		assocSet = assocSet.addDifferenceSet(d.Assocs)
		parent = d.Parent
	}
	return newIncompleteDict(assocSet, parent)
}

func newIncompleteDict(assocs AssocSet, parent Term) *Dict {
	var hasVar bool
	for _, assoc := range assocs {
		if assoc.hasVar() {
			hasVar = true
			break
		}
	}
	if !hasVar {
		hasVar = parent.hasVar()
	}
	return &Dict{Assocs: assocs, Parent: parent, hasVar_: hasVar}
}

// ---- Clauses

func NewClause(head Term, body ...Term) *Clause {
	var hasVar bool
	for _, term := range body {
		if term.hasVar() {
			hasVar = true
			break
		}
	}
	if !hasVar {
		hasVar = head.hasVar()
	}
	return &Clause{Head: head, Body: body, hasVar_: hasVar}
}

// Normalize transforms the clause to contain only comp terms.
//
// Example:
//    p :- f(X), q, X.
//    ->
//    p() :- f(X), q(), call(X).
func (c *Clause) Normalize() (*Clause, error) {
	var head Term
	switch h := c.Head.(type) {
	case Atom:
		head = NewComp(h.Name)
	case *Comp:
		head = h
	default:
		return nil, fmt.Errorf("invalid head term for clause: %v (must be atom or comp)", c.Head)
	}
	body := make([]Term, len(c.Body))
	for i, term := range c.Body {
		switch t := term.(type) {
		case Atom:
			body[i] = NewComp(t.Name)
		case Var:
			body[i] = NewComp("call", t)
		case *Comp:
			body[i] = t
		default:
			return nil, fmt.Errorf("invalid body term for clause: %v (must be atom, var or comp)", term)
		}
	}
	return NewClause(head, body...), nil
}

// ---- vars()

// Vars returns a set with all term variables, in insertion order.
func Vars(term Term) []Var {
	if !term.hasVar() {
		return nil
	}
	var xs []Var
	seen := make(map[Var]struct{})
	switch t := term.(type) {
	case Atom:
		xs = t.vars(seen, xs)
	case Int:
		xs = t.vars(seen, xs)
	case Var:
		xs = t.vars(seen, xs)
	case *Comp:
		xs = t.vars(seen, xs)
	case *List:
		xs = t.vars(seen, xs)
	case *Assoc:
		xs = t.vars(seen, xs)
	case *Dict:
		xs = t.vars(seen, xs)
	default:
		panic(fmt.Sprintf("logic.Vars: unhandled type %T", term))
	}
	return xs
}

func (t Atom) vars(seen map[Var]struct{}, xs []Var) []Var    { return xs }
func (t Int) vars(seen map[Var]struct{}, xs []Var) []Var     { return xs }

func (t Var) vars(seen map[Var]struct{}, xs []Var) []Var {
	if _, ok := seen[t]; ok {
		return xs
	}
	seen[t] = struct{}{}
	return append(xs, t)
}

func (t *Comp) vars(seen map[Var]struct{}, xs []Var) []Var {
	for _, term := range t.Args {
		xs = term.vars(seen, xs)
	}
	return xs
}

func (t *List) vars(seen map[Var]struct{}, xs []Var) []Var {
	for _, term := range t.Terms {
		xs = term.vars(seen, xs)
	}
	xs = t.Tail.vars(seen, xs)
	return xs
}

func (t *Assoc) vars(seen map[Var]struct{}, xs []Var) []Var {
	xs = t.Key.vars(seen, xs)
	xs = t.Val.vars(seen, xs)
	return xs
}

func (t *Dict) vars(seen map[Var]struct{}, xs []Var) []Var {
	for _, assoc := range t.Assocs {
		xs = assoc.vars(seen, xs)
	}
	xs = t.Parent.vars(seen, xs)
	return xs
}

func (t *Clause) Vars() []Var {
	seen := make(map[Var]struct{})
	var xs []Var
	xs = t.Head.vars(seen, xs)
	for _, term := range t.Body {
		xs = term.vars(seen, xs)
	}
	return xs
}

// ---- hasVar()

func (t Atom) hasVar() bool    { return false }
func (t Int) hasVar() bool     { return false }
func (t Var) hasVar() bool     { return true }
func (t *Comp) hasVar() bool   { return t.hasVar_ }
func (t *List) hasVar() bool   { return t.hasVar_ }
func (t *Assoc) hasVar() bool  { return t.hasVar_ }
func (t *Dict) hasVar() bool   { return t.hasVar_ }
func (c *Clause) hasVar() bool { return c.hasVar_ }

// ---- Comparisons

func termOrder(t Term) int {
	switch t.(type) {
	case Var:
		return 1
	case Int:
		return 2
	case Atom:
		return 3
	case *Comp:
		return 4
	case *List:
		return 5
	case *Assoc:
		return 6
	case *Dict:
		return 7
	default:
		panic(fmt.Sprintf("logic.termOrder: unhandled type %T", t))
	}
}

type ordering int

const (
	less ordering = iota
	equal
	more
)

func compareStrings(s1, s2 string) ordering {
	if s1 < s2 {
		return less
	}
	if s1 > s2 {
		return more
	}
	return equal
}

func compareInts(a, b int) ordering {
	if a < b {
		return less
	}
	if a > b {
		return more
	}
	return equal
}

func compare(t1, t2 Term) ordering {
	switch u := t1.(type) {
	case Atom:
		if v, ok := t2.(Atom); ok {
			return u.compare(v)
		}
	case Int:
		if v, ok := t2.(Int); ok {
			return u.compare(v)
		}
	case Var:
		if v, ok := t2.(Var); ok {
			return u.compare(v)
		}
	case *Comp:
		if v, ok := t2.(*Comp); ok {
			return u.compare(v)
		}
	case *List:
		if v, ok := t2.(*List); ok {
			return u.compare(v)
		}
	case *Assoc:
		if v, ok := t2.(*Assoc); ok {
			return u.compare(v)
		}
	case *Dict:
		if v, ok := t2.(*Dict); ok {
			return u.compare(v)
		}
	default:
		panic(fmt.Sprintf("logic.compare: unhandled type %T", t1))
	}
	return compareInts(termOrder(t1), termOrder(t2))
}

func (a Atom) compare(other Atom) ordering {
	return compareStrings(a.Name, other.Name)
}

func (i Int) compare(other Int) ordering {
	return compareInts(i.Value, other.Value)
}

func (x Var) compare(other Var) ordering {
	if o := compareStrings(x.Name, other.Name); o != equal {
		return o
	}
	return compareInts(x.suffix, other.suffix)
}

func (c *Comp) compare(other *Comp) ordering {
	if o := compareInts(len(c.Args), len(other.Args)); o != equal {
		return o
	}
	if o := compareStrings(c.Functor, other.Functor); o != equal {
		return o
	}
	for i := 0; i < len(c.Args); i++ {
		if o := compare(c.Args[i], other.Args[i]); o != equal {
			return o
		}
	}
	return equal
}

func (l *List) compare(other *List) ordering {
	n := min(len(l.Terms), len(other.Terms))
	for i := 0; i < n; i++ {
		if o := compare(l.Terms[i], other.Terms[i]); o != equal {
			return o
		}
	}
	return compare(
		NewIncompleteList(l.Terms[n:], l.Tail),
		NewIncompleteList(other.Terms[n:], other.Tail))
}

func (a *Assoc) compare(other *Assoc) ordering {
	if o := compare(a.Key, other.Key); o != equal {
		return o
	}
	return compare(a.Val, other.Val)
}

func (d *Dict) compare(other *Dict) ordering {
	n := min(len(d.Assocs), len(other.Assocs))
	for i := 0; i < n; i++ {
		if o := compare(d.Assocs[i], other.Assocs[i]); o != equal {
			return o
		}
	}
	return compare(
		NewIncompleteDict(d.Assocs[n:], d.Parent),
		NewIncompleteDict(other.Assocs[n:], other.Parent))
}

// ---- Less()

func Less(t1, t2 Term) bool {
	return compare(t1, t2) == less
}

func (t Atom) Less(other Atom) bool       { return t.Name < other.Name }
func (t Int) Less(other Int) bool         { return t.Value < other.Value }

func (t Var) Less(other Var) bool {
	if t.Name != other.Name {
		return t.Name < other.Name
	}
	return t.suffix < other.suffix
}

func (t *Comp) Less(other *Comp) bool   { return t.compare(other) == less }
func (t *List) Less(other *List) bool   { return t.compare(other) == less }
func (t *Assoc) Less(other *Assoc) bool { return t.compare(other) == less }
func (t *Dict) Less(other *Dict) bool   { return t.compare(other) == less }

// ---- Eq()

func Eq(t1, t2 Term) bool {
	return compare(t1, t2) == equal
}

func (t Atom) Eq(other Atom) bool       { return t == other }
func (t Int) Eq(other Int) bool         { return t == other }
func (t Var) Eq(other Var) bool         { return t == other }
func (t *Comp) Eq(other *Comp) bool     { return t.compare(other) == equal }
func (t *List) Eq(other *List) bool     { return t.compare(other) == equal }
func (t *Assoc) Eq(other *Assoc) bool   { return t.compare(other) == equal }
func (t *Dict) Eq(other *Dict) bool     { return t.compare(other) == equal }

// ---- String()

func (t Atom) String() string {
	return fmt.Sprintf("%q", t.Name)
}

func (t Int) String() string {
	return fmt.Sprintf("%d", t.Value)
}

func (t Var) String() string {
	suffix := ""
	if t.suffix > 0 {
		suffix = fmt.Sprintf("_%d_", t.suffix)
	}
	return fmt.Sprintf("%s%s", t.Name, suffix)
}

func (t *Comp) String() string {
	args := make([]string, len(t.Args))
	for i, arg := range t.Args {
		args[i] = arg.String()
	}
	return fmt.Sprintf("%s(%s)", t.Functor, strings.Join(args, ", "))
}

func (t *List) String() string {
	terms := make([]string, len(t.Terms))
	for i, term := range t.Terms {
		terms[i] = term.String()
	}
	xs := strings.Join(terms, ", ")
	if t.Tail == EmptyList {
		return fmt.Sprintf("[%s]", xs)
	}
	return fmt.Sprintf("[%s|%v]", xs, t.Tail)
}

func (t *Assoc) String() string {
	return fmt.Sprintf("%v:%v", t.Key, t.Val)
}

func (t *Dict) String() string {
	assocs := make([]string, len(t.Assocs))
	for i, assoc := range t.Assocs {
		assocs[i] = assoc.String()
	}
	xs := strings.Join(assocs, ", ")
	if t.Parent == EmptyDict {
		return fmt.Sprintf("{%s}", xs)
	}
	return fmt.Sprintf("{%s|%v}", xs, t.Parent)
}

func (c *Clause) String() string {
	head := c.Head.String()
	if len(c.Body) == 0 {
		return head + "."
	}
	body := make([]string, len(c.Body))
	for i, comp := range c.Body {
		body[i] = comp.String()
	}
	return fmt.Sprintf("%s :-\n  %s.", head, strings.Join(body, ",\n  "))
}

// ---- short()

func (t Atom) short() string    { return t.String() }
func (t Int) short() string     { return t.String() }
func (t Var) short() string     { return t.String() }

func (t *Comp) short() string {
	return fmt.Sprintf("%s/%d", t.Functor, len(t.Args))
}

func (t *List) short() string {
	if t.Tail == EmptyList {
		return fmt.Sprintf("[,%d]", len(t.Terms))
	}
	return fmt.Sprintf("[,%d|%s]", len(t.Terms), t.Tail.short())
}

func (t *Assoc) short() string {
	return fmt.Sprintf("%s:%s", t.Key.short(), t.Val.short())
}

func (t *Dict) short() string {
	if t.Parent == EmptyDict {
		return fmt.Sprintf("{,%d}", len(t.Assocs))
	}
	return fmt.Sprintf("{,%d|%s}", len(t.Assocs), t.Parent.short())
}

// ---- replaceVars()

type varTransform func(Var) Term

func replaceVars(term Term, f varTransform) Term {
	switch t := term.(type) {
	case Atom:
		return t.replaceVars(f)
	case Int:
		return t.replaceVars(f)
	case Var:
		return t.replaceVars(f)
	case *Comp:
		return t.replaceVars(f)
	case *List:
		return t.replaceVars(f)
	case *Assoc:
		return t.replaceVars(f)
	case *Dict:
		return t.replaceVars(f)
	default:
		panic(fmt.Sprintf("logic.replaceVars: unhandled type %T", term))
	}
}

func (t Atom) replaceVars(f varTransform) Atom {
	return t
}

func (t Int) replaceVars(f varTransform) Int {
	return t
}

func (t Var) replaceVars(f varTransform) Term {
	return f(t)
}

func (t *Comp) replaceVars(f varTransform) *Comp {
	if !t.hasVar() {
		return t
	}
	args := make([]Term, len(t.Args))
	for i, arg := range t.Args {
		args[i] = replaceVars(arg, f)
	}
	return NewComp(t.Functor, args...)
}

func (t *List) replaceVars(f varTransform) *List {
	if !t.hasVar() {
		return t
	}
	terms := make([]Term, len(t.Terms))
	for i, term := range t.Terms {
		terms[i] = replaceVars(term, f)
	}
	tail := replaceVars(t.Tail, f)
	return NewIncompleteList(terms, tail).(*List)
}

func (t *Assoc) replaceVars(f varTransform) *Assoc {
	if !t.hasVar() {
		return t
	}
	return NewAssoc(replaceVars(t.Key, f), replaceVars(t.Val, f))
}

func (t *Dict) replaceVars(f varTransform) *Dict {
	if !t.hasVar() {
		return t
	}
	assocs := make([]*Assoc, len(t.Assocs))
	for i, assoc := range t.Assocs {
		assocs[i] = assoc.replaceVars(f)
	}
	parent := replaceVars(t.Parent, f)
	return NewIncompleteDict(assocs, parent).(*Dict)
}

func (c *Clause) replaceVars(f varTransform) *Clause {
	if !c.hasVar() {
		return c
	}
	head := replaceVars(c.Head, f)
	body := make([]Term, len(c.Body))
	for i, term := range c.Body {
		body[i] = replaceVars(term, f)
	}
	return NewClause(head, body...)
}

func (c *Clause) setVarSuffix(suffix int) *Clause {
	return c.replaceVars(func(x Var) Term {
		return x.WithSuffix(suffix)
	})
}

// ---- conversions

func dictToList(t Term) (Term, error) {
	if t == EmptyDict {
		return EmptyList, nil
	}
	d, ok := t.(*Dict)
	if !ok {
		return nil, fmt.Errorf("not a dict")
	}
	if d.Parent != EmptyDict {
		return nil, fmt.Errorf("incomplete dict")
	}
	terms := make([]Term, len(d.Assocs))
	for i, assoc := range d.Assocs {
		terms[i] = assoc
	}
	return NewList(terms...), nil
}

func listToDict(t Term) (Term, error) {
	if t == EmptyList {
		return EmptyDict, nil
	}
	l, ok := t.(*List)
	if !ok {
		return nil, fmt.Errorf("not a list")
	}
	if l.Tail != EmptyList {
		return nil, fmt.Errorf("incomplete list")
	}
	assocs := make([]*Assoc, len(l.Terms))
	for i, term := range l.Terms {
		assocs[i], ok = term.(*Assoc)
		if !ok {
			return nil, fmt.Errorf("expected Assoc @ pos %d, found %T", i, term)
		}
	}
	return NewDict(assocs...), nil
}
