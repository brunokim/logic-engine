package test_helpers

import (
	"strings"
)

func numSpaces(s string) int {
	n := 0
	for _, ch := range s {
		if ch != ' ' {
			break
		}
		n++
	}
	return n
}

// TabIndent indents every line of s with a single tab.
func TabIndent(s string) string {
	lines := strings.Split(s, "\n")
	for i, line := range lines {
		n := numSpaces(line)
		levels := n / 2
		spaceIndent := strings.Repeat(" ", levels*2)
		tabIndent := strings.Repeat("\t", levels)
		lines[i] = tabIndent + strings.TrimPrefix(line, spaceIndent)
	}
	return strings.Join(lines, "\n")
}

// Dedent removes common whitespace of each line. Useful to remove indentation that is
// present only because of a `backtick` string indentation level.
func Dedent(s string) string {
	lines := strings.Split(s, "\n")
	minSpaces := len(s)
	for _, line := range lines {
		n := numSpaces(line)
		if n == 0 {
			continue
		}
		if n < minSpaces {
			minSpaces = n
		}
	}
	prefix := strings.Repeat(" ", minSpaces)
	for i, line := range lines {
		lines[i] = strings.TrimPrefix(line, prefix)
	}
	return strings.TrimSpace(strings.Join(lines, "\n"))
}
