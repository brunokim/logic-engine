// Package logic implements the interface for a logic engine, with terms and solvers.
//
// A logic term can be fall in one of three categories:
//
// * atomic: a term that represents an immutable value.
//
// * variable: a term that represents an unbound, yet-to-be-resolved term.
//
// * complex: a term that contains other terms, recursively.
//
// A logic program takes a list of rules and, given a query, tries to obtain
// a list of variable assignments that satisfy this query, subject to the rules
// provided.
//
// The simplest rule is a fact, that simply enunciates some known relationship.
// For example, we can write the parent-child relationship of some members of the
// British royal family:
//
//     parent(elizabeth, charles).
//     parent(philip, charles).
//     parent(charles, william).
//     parent(diana, william).
//     parent(charles, harry).
//     parent(diana, harry).
//
// The fact 'parent(X, Y)' should be read as "X is parent of Y". We can use these
// facts to query for relationships that satisfy both the query and the fact database.
//
//     ?- parent(P, charles).  % Query for P that satisfy "P is parent of charles"
//     P = elizabeth ;
//     P = philip .
//
//     ?- parent(diana, C).    % Query for C that satisfy "diana is parent of C"
//     C = william ;
//     C = harry .
//
//     ?- parent(X, philip).   % Query for X that satisfy "X is parent of philip".
//     false                   % There's no fact in the database that satisfy that.
//
// More complex rules can be written as clauses like "X :- A, B, C.", that can be
// read as "X holds if A, B, C also hold".
//
//     % G is a grandparent of C if G is parent of (some) P, and P is parent of C.
//     grandparent(G, C) :- parent(G, P), parent(P, C).
//
//     % P1 is a partner of P2 if P1 and P2 have (some) child C, and P1 is not P2.
//     partner(P1, P2) :- parent(P1, C), parent(P2, C), P1 \= P2.
//
// We can query these rules just as facts. The logic engine will look for facts that
// satisfy the clause recursively and output all solutions.
//
//     ?- grandparent(G, harry).    % Query for grandparents G of harry
//     G = elizabeth ;
//     G = philip .
//
//     ?- partner(X, Y).            % Query for all partners in the database.
//     X = elizabeth, Y = philip ;
//     X = philip, Y = elizabeth ;
//     X = charles, Y = diana ;     % charles-diana appears twice because they
//     X = charles, Y = diana ;     % are partners in two ways: with
//     X = diana, Y = charles ;     % C = william and C = harry.
//     X = diana, Y = charles .
package logic

import (
	"fmt"
	"sort"
	"strings"
)

// ---- Basic types

// Term is a representation of a logic term.
type Term interface {
	fmt.Stringer
	short() string
	vars(seen map[Var]struct{}, xs []Var) []Var
	hasVar() bool
}

// Atom is an atomic term representing a symbol.
type Atom struct {
	// Name is the identifier for an atom.
	Name string
}

// Int is an atomic term representing an integer.
type Int struct {
	// Value is the (immutable) value of an int.
	Value int
}

// Var is a variable term.
type Var struct {
	// Name is the identifier for a var.
	Name   string
	suffix int
}

// Comp is a complex term, representing an immutable compound term.
type Comp struct {
	// Functor is the primary identifier of a comp.
	Functor string
	// Args is the list of terms within this term.
	Args    []Term
	hasVar_ bool
}

// List is a complex term, representing an ordered sequence of terms.
type List struct {
	// Terms are the contents of a list.
	Terms []Term
	// Tail is the continuation of a list, which is usually another
	// list, the empty list, or an unbound var.
	Tail    Term
	hasVar_ bool
}

// Assoc is a complex term, representing an association pair.
type Assoc struct {
	// Key is the association key
	Key Term
	// Val is the association value
	Val     Term
	hasVar_ bool
}

// AssocSet is a set of assocs, implemented as a sorted array.
type AssocSet []*Assoc

// Dict is a complex term, representing a set of associations with
// unique keys.
type Dict struct {
	// Set of associations of this dict.
	Assocs AssocSet
	// Parent is the representation of another dict with additional
	// values to this dict. Note that the keys in Assocs override the
	// ones in Parent. It usually is another dict, an unbound var, or
	// the empty dict.
	Parent  Term
	hasVar_ bool
}

// Clause is the representation of a logic rule.
// Note that Clause is not a Term, so it can't be used within complex terms.
type Clause struct {
	// Head is the consequent of a clause. May be Atom or Comp.
	Head Term
	// Body is the antecedent of a clause. May be Atom, Var or Comp.
	Body    []Term
	hasVar_ bool
}

// ---- Public vars

var (
	// AnonymousVar represents a variable to be ignored.
	AnonymousVar = NewVar("_")
	// EmptyList is an atom representing an empty list.
	EmptyList = Atom{"[]"}
	// EmptyDict is an atom representing an empty dict.
	EmptyDict = Atom{"{}"}
)

// ---- Vars

// NewVar creates a new var.
//
// It panics if the name doesn't start with an uppercase letter or an underscore.
func NewVar(name string) Var {
    if !IsVar(name) {
		panic("NewVar: invalid name: %q")
	}
    return Var{name, 0}
}

// WithSuffix creates a new var with the same name and provided suffix. Used to
// generate vars from the same template.
func (x Var) WithSuffix(suffix int) Var {
	if x.Name == "_" {
		return x
	}
	return Var{x.Name, suffix}
}

// ---- Compound terms

// NewComp creates a compound term.
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

// Indicator is a notation for a comp, usually shown as functor/arity, e.g., f/2.
type Indicator struct {
	// Name is the compound term's functor.
	Name string
	// Arity is the compound term's number of args.
	Arity int
}

// Indicator returns the functor's indicator.
func (c *Comp) Indicator() Indicator {
	return Indicator{c.Functor, len(c.Args)}
}

// ---- Lists

// NewList creates a List with the provided terms and EmptyList as tail.
func NewList(terms ...Term) Term {
	return NewIncompleteList(terms, EmptyList)
}

// NewIncompleteList creates a List with the provided terms and tail.
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

// Slice returns a new list starting from the n-th term, inclusive.
func (l *List) Slice(n int) Term {
	if n < 0 || n > len(l.Terms) {
		panic(fmt.Sprintf("(*List).Slice: invalid index %d", n))
	}
	if n == len(l.Terms) {
		return l.Tail
	}
	if !l.hasVar_ {
		return &List{Terms: l.Terms[n:], Tail: l.Tail, hasVar_: false}
	}
	return NewIncompleteList(l.Terms[n:], l.Tail)
}

// ---- Assoc and Dict

// NewAssoc returns an assoc with provided key and value.
func NewAssoc(key, val Term) *Assoc {
	return &Assoc{Key: key, Val: val, hasVar_: key.hasVar() || val.hasVar()}
}

// NewAssocSet returns an AssocSet with the provided assocs.
//
// It returns an error if there are any duplicate keys.
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

// NewDict returns a dict with the provided assocs and EmptyDict as parent.
//
// It panics if there are duplicate keys in assocs.
func NewDict(assocs ...*Assoc) Term {
	return NewIncompleteDict(assocs, EmptyDict)
}

// NewIncompleteDict returns a dict with the provided assocs and parent.
//
// It panics if there are duplicate keys in assocs.
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

// NewClause returns a clause with the provided head and terms as body.
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
// Atoms in the clause's head and body are converted to functors with 0 arity.
// Variables in the clause's body are converted to a 'call(X)' functor.
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

func (t Atom) vars(seen map[Var]struct{}, xs []Var) []Var { return xs }
func (t Int) vars(seen map[Var]struct{}, xs []Var) []Var  { return xs }

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

// Vars returns a set with all variables, in insertion order.
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

// Less returns the order between t1 and t2, following the standard of terms.
//
// The order of terms is: Vars < Ints < Atoms < Comps < List < Assoc < Dict
func Less(t1, t2 Term) bool {
	return compare(t1, t2) == less
}

// Less returns whether this atom is less than another, in lexicographic order.
func (t Atom) Less(other Atom) bool { return t.Name < other.Name }

// Less returns whether this int is less than another.
func (t Int) Less(other Int) bool { return t.Value < other.Value }

// Less returns whether this var is less than another, in lexicographic order.
func (t Var) Less(other Var) bool {
	if t.Name != other.Name {
		return t.Name < other.Name
	}
	return t.suffix < other.suffix
}

// Less returns whether this comp is less than another.
//
// Comps are first compared by arity, then by functor, then by args pairwise.
func (t *Comp) Less(other *Comp) bool { return t.compare(other) == less }

// Less returns whether this list is less than another.
//
// Lists are compared lexicographically.
func (t *List) Less(other *List) bool { return t.compare(other) == less }

// Less returns whether this assoc is less than another.
//
// Assocs are compared first by key, than by value.
func (t *Assoc) Less(other *Assoc) bool { return t.compare(other) == less }

// Less returns whether this dict is less than another.
//
// Dicts are compared in lexicographic order, with keys sorted.
func (t *Dict) Less(other *Dict) bool { return t.compare(other) == less }

// ---- Eq()

// Eq returns whether t1 and t2 are identical terms.
//
// Note that this only takes into account the structure of terms, not whether
// any binding may make them identical.
func Eq(t1, t2 Term) bool {
	return compare(t1, t2) == equal
}

// Eq returns whether this atom is equal to another.
func (t Atom) Eq(other Atom) bool { return t == other }

// Eq returns whether this int is equal to another.
func (t Int) Eq(other Int) bool { return t == other }

// Eq returns whether this var is equal to another.
func (t Var) Eq(other Var) bool { return t == other }

// Eq returns whether this comp is equal to another.
func (t *Comp) Eq(other *Comp) bool { return t.compare(other) == equal }

// Eq returns whether this list is equal to another.
func (t *List) Eq(other *List) bool { return t.compare(other) == equal }

// Eq returns whether this assoc is equal to another.
func (t *Assoc) Eq(other *Assoc) bool { return t.compare(other) == equal }

// Eq returns whether this dict is equal to another.
func (t *Dict) Eq(other *Dict) bool { return t.compare(other) == equal }

// ---- String()

func (t Atom) String() string {
	return FormatAtom(t.Name)
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

func (t Atom) short() string { return t.String() }
func (t Int) short() string  { return t.String() }
func (t Var) short() string  { return t.String() }

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
