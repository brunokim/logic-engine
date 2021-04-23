package logic

import (
    "unicode"
    "strings"
)

func isIdent(ch rune) bool {
    return ch == '_' || unicode.IsLetter(ch) || unicode.IsDigit(ch)
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

func IsVar(text string) bool {
	ch, err := firstRune(text)
	if err != nil {
		return false
	}
	if !isVarFirst(ch) {
        return false
    }
    return isIdents(text)
}

func IsInt(text string) bool {
    if text == "" {
        return false
    }
    for _, ch := range text {
        if !unicode.IsDigit(ch) {
            return false
        }
    }
    return true
}

func IsQuotedAtom(text string) bool {
    if text == "" {
        return true
    }
    ch, err := firstRune(text)
    if err != nil || isVarFirst(ch) || unicode.IsDigit(ch) {
        return true
    }
    return !isIdents(text)
}

var escapeChars = map[rune]string{
	' ':  " ",
	'\n': "\\n",
	'\t': "\\t",
	'\v': "\\v",
	'\f': "\\f",
	'\r': "\\r",
	',':  ",",
	'(':  "(",
	')':  ")",
	'[':  "[",
	']':  "]",
	'"':  "\\\"",
	'\\': "\\\\",
	'_':  "_",
}

func FormatAtom(text string) string {
	// Check if there's any character that needs escaping.
	var hasEscape bool
	for _, ch := range text {
		if _, ok := escapeChars[ch]; ok {
			hasEscape = true
			break
		}
	}
	if !(hasEscape || IsVar(text) || IsInt(text)) {
		return text
	}
	// Build a quoted atom.
	var b strings.Builder
	b.WriteRune('"')
	for _, ch := range text {
		if exp, ok := escapeChars[ch]; ok {
			b.WriteString(exp)
		} else {
			b.WriteRune(ch)
		}
	}
	b.WriteRune('"')
	return b.String()
}

