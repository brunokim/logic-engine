package main

import (
	"flag"
	"io/ioutil"
	"log"
	"os"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/wam"
)

var (
	inputFilename  = flag.String("input", "", "Input file (required)")
	outputFilename = flag.String("output", "", "Output file (required)")
)

func main() {
	flag.Parse()
	if *inputFilename == "" {
		log.Fatalf("-input is required")
	}
	if *outputFilename == "" {
		log.Fatalf("-output is required")
	}
	input, err := os.Open(*inputFilename)
	if err != nil {
		log.Fatalf("open input: %v", err)
	}
	defer input.Close()
	bs, err := ioutil.ReadAll(input)
	if err != nil {
		log.Fatalf("input: %v", err)
	}
	var letters []logic.Term
	for _, ch := range string(bs) {
		letters = append(letters, dsl.Atom(string(ch)))
	}
	tree := dsl.Var("Tree")
	m := wam.NewMachine()
	m.AddPackage(parser.ParserPkg)
	m.DebugFilename = *outputFilename
	bindings, err := m.RunQuery(
		dsl.Comp("import", dsl.Atom("parser")),
		dsl.Comp("parse_kb", dsl.List(letters...), tree))
	if err != nil {
		log.Fatalf("parse: %v", err)
	}
	log.Printf("%v", bindings[tree])
}
