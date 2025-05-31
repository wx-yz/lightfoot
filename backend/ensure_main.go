// ensure_main.go
package backend

/*
#include <llvm-c/Core.h>
#include <stdlib.h>
*/
import "C"

import (
	"fmt"
	"unsafe"
)

// ensureMainExists makes sure the ballerina_main function exists and is exported correctly
func ensureMainExists(cg *CodeGenerator) {
	// Check if we already have a ballerina_main function
	if _, exists := cg.functions["ballerina_main"]; exists {
		fmt.Println("[DEBUG] ballerina_main function exists - module ready for compilation")
		return
	}

	// If we don't have ballerina_main yet, create one
	fmt.Println("[DEBUG] Creating ballerina_main function for runtime compatibility")

	// Generate the function using C API instead of Go IR
	if cg.llvmCtx != nil && cg.llvmModule != nil && cg.llvmModule.module != nil {
		// Create function type
		voidType := C.LLVMVoidTypeInContext(cg.llvmCtx.context)
		funcType := C.LLVMFunctionType(voidType, nil, 0, 0)

		// Add function to module
		funcVal := cg.llvmModule.AddFunction("ballerina_main", funcType)

		// Create entry block
		entryBlockName := C.CString("entry")
		defer C.free(unsafe.Pointer(entryBlockName))
		entryBlock := C.LLVMAppendBasicBlockInContext(cg.llvmCtx.context, funcVal, entryBlockName)

		// Position builder at the end of the entry block
		cg.builder.SetBlock(entryBlock)

		// Add a RetVoid instruction
		cg.builder.NewRetVoid()

		fmt.Println("[DEBUG] Created ballerina_main function in LLVM module")
	} else {
		fmt.Println("[WARNING] Could not create ballerina_main in C LLVM module - context or module is nil")
	}
}
