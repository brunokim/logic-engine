package logic

import (
	"strings"
	"unicode"

	"github.com/brunokim/logic-engine/runes"
)

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isIdent(ch rune) bool {
	return ch == '_' || unicode.IsLetter(ch) || isDigit(ch)
}

func isIdents(text string) bool {
	for _, ch := range text {
		if !isIdent(ch) {
			return false
		}
	}
	return true
}

func isVarFirst(ch rune) bool {
	return ch == '_' || unicode.IsUpper(ch)
}

// IsVar returns whether text is a valid name for a Var.
//
// A var must begin with an uppercase letter or an underscore, and the other
// letters must be identifier letters (e.g., letter, digit or underscore).
func IsVar(text string) bool {
	ch, err := runes.First(text)
	if err != nil {
		return false
	}
	if !isVarFirst(ch) {
		return false
	}
	return isIdents(text)
}

// IsInt returns whether text is a valid value for an Int.
//
// An int must contain only (Latin) digit letters.
func IsInt(text string) bool {
	if text == "" {
		return false
	}
	for _, ch := range text {
		if !isDigit(ch) {
			return false
		}
	}
	return true
}

// Returns whether text must be quoted as an atom.
func IsQuotedAtom(text string) bool {
	if text == "" {
		return true
	}
	ch, err := runes.First(text)
	if err != nil || isVarFirst(ch) || isDigit(ch) {
		return true
	}
	return !isIdents(text)
}

var (
	escapeChars = map[rune]string{
		'\n': `\n`,
		'\t': `\t`,
		'\v': `\v`,
		'\f': `\f`,
		'\r': `\r`,
		'\\': `\\`,
	}
	syntactic    = " \n\t\v\f\r%()[]{}\"',.:|_\\"
	syntacticSet map[rune]struct{}
)

func init() {
	syntacticSet = make(map[rune]struct{})
	for _, r := range syntactic {
		syntacticSet[r] = struct{}{}
	}
}

// Formats an atom, escaping and quoting the text if necessary.
func FormatAtom(text string) string {
	// Check if there's any character that needs escaping.
	var hasEscape bool
	for _, ch := range text {
		if _, ok := syntacticSet[ch]; ok {
			hasEscape = true
			break
		}
	}
	if !(hasEscape || IsVar(text) || IsInt(text)) {
		return text
	}
	// Build a quoted atom.
	var b strings.Builder
	b.WriteRune('\'')
	for _, ch := range text {
		if exp, ok := escapeChars[ch]; ok {
			b.WriteString(exp)
		} else if ch == '\'' {
			b.WriteString(`\'`)
		} else {
			b.WriteRune(ch)
		}
	}
	b.WriteRune('\'')
	return b.String()
}

func FormatString(text []rune) string {
	var b strings.Builder
	b.WriteRune('"')
	for _, ch := range text {
		if exp, ok := escapeChars[ch]; ok {
			b.WriteString(exp)
		} else if ch == '"' {
			b.WriteString(`\"`)
		} else {
			b.WriteRune(ch)
		}
	}
	b.WriteRune('"')
	return b.String()
}
