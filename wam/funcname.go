package wam

import (
	"reflect"
	"runtime"
	"strings"
)

func funcName(f interface{}) string {
	ptr := reflect.ValueOf(f).Pointer()
	name := runtime.FuncForPC(ptr).Name()
	return strings.TrimPrefix(name, "github.com/brunokim/logic-engine/wam.")
}
