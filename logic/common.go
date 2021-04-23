package logic

import (
	"fmt"
	"unicode/utf8"
)

func firstRune(s string) (rune, error) {
	r, size := utf8.DecodeRuneInString(s)
	if r == utf8.RuneError && size < 2 {
		return 0, fmt.Errorf("not UTF-8 or empty string: %q", s)
	}
	return r, nil
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
