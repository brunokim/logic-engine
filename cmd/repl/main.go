package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/brunokim/logic-engine/solver"

	"github.com/chzyer/readline"
)

var (
	consultFiles = flag.String("consult-files", "", "Comma-separated files to consult, in order")
	query        = flag.String("query", "", "Initial query to issue")
)

type inputState int

const (
	readingQuery inputState = iota
	enumerateSolutions
)

type ctx struct {
	interrupt chan os.Signal
	solver    *solver.Solver
	readline  *readline.Instance
}

func main() {
	flag.Parse()

	ctx := ctx{}
	ctx.interrupt = make(chan os.Signal, 1)
	signal.Notify(ctx.interrupt, syscall.SIGINT)

	ctx.solver = solver.New()
	files := strings.Split(*consultFiles, ",")
	for _, file := range files {
		if len(file) == 0 {
			continue
		}
		consultFile(ctx.solver, file)
	}

	rl, err := readline.NewEx(&readline.Config{
		Prompt:                 "?- ",
		HistoryFile:            "/tmp/readline-history",
		DisableAutoSaveHistory: true,
	})
	if err != nil {
		log.Fatal(err)
	}
	defer rl.Close()
	ctx.readline = rl

	ctx.mainLoop()
}

func consultFile(s *solver.Solver, filename string) {
	bs, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Print(err)
		return
	}
	if err := s.Consult(string(bs)); err != nil {
		log.Print(err)
		return
	}
}

func (ctx ctx) mainLoop() {
	state := readingQuery
	var solutions <-chan solver.Solution
	var cancel func()
	if len(*query) > 0 {
		solutions, cancel = ctx.solver.Query(*query)
		state = enumerateSolutions
	}
	for {
		switch state {
		default:
			log.Print("Invalid state:", state)
			return
		case readingQuery:
			query, isClose := ctx.readQuery()
			if isClose {
				return
			}
			solutions, cancel = ctx.solver.Query(query)
			state = enumerateSolutions
		case enumerateSolutions:
			if isClose := ctx.solutionState(solutions, cancel); isClose {
				state = readingQuery
			}
		}
	}
}

func (ctx ctx) readQuery() (string, bool) {
	ctx.readline.SetPrompt("?- ")
	var lines []string
	for {
		line, err := ctx.readline.Readline()
		if err != nil {
			return "", true
		}
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		lines = append(lines, line)
		if !strings.HasSuffix(line, ".") {
			ctx.readline.SetPrompt("|  ")
			continue
		}
		break
	}
	query := strings.Join(lines, " ")
	ctx.readline.SaveHistory(query)
	query = query[:len(query)-1] + "," // Change final '.' to ',' for successful parsing.
	return query, false
}

func (ctx ctx) solutionState(solutions <-chan solver.Solution, cancel func()) bool {
	if ctx.solver.Err != nil {
		log.Print(ctx.solver.Err)
		return true
	}
	select {
	case solution, ok := <-solutions:
		if !ok {
			fmt.Println("false.")
			return true
		}
		if len(solution) == 0 {
			fmt.Println("true")
		} else {
			fmt.Println(solution)
		}
		if isClose := ctx.readCommand(); isClose {
			cancel()
			return true
		}
		return false
	case <-ctx.interrupt:
		cancel()
		return true
	}
}

func (ctx ctx) readCommand() bool {
	for {
		ctx.readline.SetPrompt("")
		line, err := ctx.readline.Readline()
		if err != nil {
			log.Fatal(err)
			return true
		}
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		if line == ";" {
			return false
		}
		if line == "." || line == "" {
			return true
		}
		log.Print("Expecting '.' or ';'")
	}
}
