// Package runes contains some generally useful operations on runes.
package runes

import (
	"unicode"
	"unicode/utf8"
)

// First returns the first rune of s. If the string is empty or not proper UTF-8, returns false.
func First(s string) (rune, bool) {
	r, size := utf8.DecodeRuneInString(s)
	if r == utf8.RuneError && size < 2 {
		return 0, false
	}
	return r, true
}

// Single returns the single rune of s. If the string doesn't have exactly one rune, returns
// false.
func Single(s string) (rune, bool) {
	r, size := utf8.DecodeRuneInString(s)
	return r, size == len(s)
}

// All lists all runes in a range table.
func All(table *unicode.RangeTable) []rune {
	var rs []rune
	for _, r16 := range table.R16 {
		for r := r16.Lo; r <= r16.Hi; r += r16.Stride {
			rs = append(rs, rune(r))
		}
	}
	for _, r32 := range table.R16 {
		for r := r32.Lo; r <= r32.Hi; r += r32.Stride {
			rs = append(rs, rune(r))
		}
	}
	return rs
}
