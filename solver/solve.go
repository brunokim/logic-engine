// Package solver contains methods to execute logic programs and list their
// solutions.
package solver

import (
	"embed"
	"fmt"
	"io/fs"
	"sort"
	"strings"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/wam"
)

//go:embed lib
var f embed.FS

var libPkgs []*wam.Package

func init() {
	var pkgs []*wam.Package
	err := fs.WalkDir(f, ".", func(path string, d fs.DirEntry, err error) error {
		if d.IsDir() {
			return nil
		}
		bs, err := f.ReadFile(path)
		if err != nil {
			return err
		}
		rules, err := parser.ParseRules(string(bs))
		if err != nil {
			return err
		}
		pkg, err := wam.CompilePackage(rules)
		if err != nil {
			return err
		}
		pkgs = append(pkgs, pkg)
		return nil
	})
	if err != nil {
		panic(err)
	}
	libPkgs = pkgs
}

// Solver provides an asynchronous interface for enumerating the solutions
// of a logic query.
type Solver struct {
	// Err stores the error that terminated a query.
	Err error

	m *wam.Machine
}

// Solution (or bindings) is a substitution from vars to terms that produce
// a valid predicate.
type Solution map[logic.Var]logic.Term

func (s Solution) String() string {
	vars := make([]logic.Var, len(s))
	i := 0
	for x := range s {
		vars[i] = x
		i++
	}
	sort.Slice(vars, func(i, j int) bool { return vars[i].Less(vars[j]) })
	var b strings.Builder
	for i, x := range vars {
		fmt.Fprintf(&b, "%v = %v", x, s[x])
		if i < len(vars)-1 {
			b.WriteString(", ")
		}
	}
	return b.String()
}

// New creates a Solver with builtin packages.
func New() *Solver {
	solver := new(Solver)
	solver.m = wam.NewMachine()
	for _, pkg := range libPkgs {
		if err := solver.m.AddPackage(pkg); err != nil {
			panic(fmt.Errorf("Unexpected error adding builtin pkg: %v", err))
		}
	}
	return solver
}

// Consult parses and compiles the provided program into the solver.
func (s *Solver) Consult(text string) error {
	clauses, err := parser.ParseRules(text)
	if err != nil {
		return err
	}
	return s.ConsultRules(clauses)
}

// ConsultRules is like Consult, with already parsed clauses.
func (s *Solver) ConsultRules(rules []logic.Rule) error {
	pkg, err := wam.CompilePackage(rules)
	if err != nil {
		return err
	}
	if err := s.m.AddPackage(pkg); err != nil {
		return err
	}
	return nil
}

// SetIterLimit sets a maximum number of iterations to the internal machine.
func (solver *Solver) SetIterLimit(limit int) {
	solver.m.IterLimit = limit
}

// SetDebug sets a file to output execution information for the internal machine.
func (solver *Solver) SetDebug(filename string) {
	solver.m.DebugFilename = filename
}

// Query returns an (unbuffered) channel of solutions for the provided query,
// and a cancel function to interrupt the execution.
//
// As soon as a value is read from the channel, the next solution starts to
// be computed. If there's no need for additional solutions, you should call
// cancel().
//
// The last error found is stored in solver.Err.
func (solver *Solver) Query(text string) (<-chan Solution, func()) {
	terms, err := parser.ParseQuery(text)
	if err != nil {
		solver.Err = err
		return nil, func() {}
	}
	return solver.QueryTerms(terms...)
}

// QueryTerms is like Query, with already parsed terms.
func (solver *Solver) QueryTerms(terms ...logic.Term) (<-chan Solution, func()) {
	solver.Err = nil
	m := solver.m.Reset()
	stream := make(chan Solution)
	go func() {
		bindings, err := m.RunQuery(terms...)
		for err == nil {
			stream <- bindings
			bindings, err = m.NextSolution()
		}
		solver.Err = err
		close(stream)
	}()
	return stream, func() { m.Interrupt() }
}
