package backend

import (
	"strings"
)

// getLLVMFunctionName converts a Ballerina function name to a valid LLVM function name.
// This handles special function names that might conflict with LLVM reserved names
// or have special meaning in Ballerina (like module-level init, main, etc.)
func (cg *CodeGenerator) getLLVMFunctionName(name string) string {
	// Replace dots with underscores since dots are not valid in LLVM function names
	// This handles cases like "ballerina.io.println" -> "ballerina_io_println"
	funcName := strings.Replace(name, ".", "_", -1)

	// The user's main function becomes _ballerina_main (with leading underscore)
	// This avoids conflicts with the ballerina_main C entry point
	if funcName == "main" {
		return "_ballerina_main"
	} else if funcName == "init" {
		// Consistent naming for init function
		return "_ballerina_init"
	}

	return funcName
}
