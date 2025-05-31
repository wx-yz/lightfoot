package backend

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/types"
)

// AddFloatAndBoolPrintFunctions adds float and boolean printing functions to the module
func (cg *CodeGenerator) AddFloatAndBoolPrintFunctions() {
	// Add float println function
	floatPrintlnFunc := cg.module.NewFunc("ballerina_io_println_float", types.Void, ir.NewParam("value", types.Double))
	cg.functions["ballerina_io_println_float"] = floatPrintlnFunc

	// Add boolean println function
	boolPrintlnFunc := cg.module.NewFunc("ballerina_io_println_bool", types.Void, ir.NewParam("value", types.I1))
	cg.functions["ballerina_io_println_bool"] = boolPrintlnFunc

	// Add integer println function
	intPrintlnFunc := cg.module.NewFunc("ballerina_io_println_int", types.Void, ir.NewParam("value", types.I64))
	cg.functions["ballerina_io_println_int"] = intPrintlnFunc
}
