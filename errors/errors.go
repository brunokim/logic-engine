package errors

import (
	"fmt"
)

type err struct {
	msg  string
	args []interface{}
}

func (err err) Error() string {
	return fmt.Sprintf(err.msg, err.args...)
}

func (err err) Unwrap() error {
	for _, arg := range err.args {
		if wrapped, ok := arg.(error); ok {
			return wrapped
		}
	}
	return nil
}

func New(msg string, args ...interface{}) error {
	return err{msg, args}
}
