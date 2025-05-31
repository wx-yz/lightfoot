// ensure_ballerina_main.go
package backend

import (
	"fmt"

	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
)

// EnsureBallerinaMainFunction makes sure there's a ballerina_main function in the module.
// This function is called from the C runtime to start execution of the Ballerina program.
func (cg *CodeGenerator) EnsureBallerinaMainFunction() {
	// Check if the function already exists (what we need for C runtime)
	if _, ok := cg.functions["ballerina_main"]; ok {
		fmt.Println("[DEBUG] ballerina_main function already exists in module")
		return
	}

	fmt.Println("[DEBUG] Creating new ballerina_main function in module")

	// Create a void function with no parameters
	mainFunc := cg.module.NewFunc("ballerina_main", types.Void)

	// Create an entry block
	entryBlock := mainFunc.NewBlock("entry")

	// Create a block for calling main if init succeeds
	mainBlock := mainFunc.NewBlock("main_call")

	// Create a block for error handling if init fails
	errorBlock := mainFunc.NewBlock("error_handling")

	// Create a block for the final exit
	exitBlock := mainFunc.NewBlock("exit")

	// If we have an init function, call it and check for errors
	if initFunc, ok := cg.functions["_ballerina_init"]; ok {
		// Get the print function for any messages
		printFunc := cg.functions["ballerina_io_println"]

		// Call the init function and get its return value
		initResult := entryBlock.NewCall(initFunc)
		fmt.Println("[DEBUG] Added call to _ballerina_init function from ballerina_main")

		// Check if the result is NULL (no error) or non-NULL (error)
		isError := entryBlock.NewICmp(enum.IPredNE, initResult, constant.NewNull(types.I8Ptr))

		// Branch based on the result
		entryBlock.NewCondBr(isError, errorBlock, mainBlock)

		// In the error block, print the error message and exit without calling main
		if printFunc != nil {
			// Print the error using ballerina_error_print function if available
			errorPrintFunc := cg.functions["ballerina_error_print"]
			if errorPrintFunc != nil {
				errorBlock.NewCall(errorPrintFunc, initResult)
			}
		}
		errorBlock.NewBr(exitBlock) // Skip to exit, don't call main

		// In the main block, call the main function if present
		if userMain, ok := cg.functions["_ballerina_main"]; ok {
			mainBlock.NewCall(userMain)
			fmt.Println("[DEBUG] Added call to _ballerina_main function from ballerina_main")

			// Print success message
			if printFunc != nil {
				successMsg := cg.CreateString("Ballerina program completed successfully.", mainBlock)
				mainBlock.NewCall(printFunc, successMsg)
			}
		}
		mainBlock.NewBr(exitBlock)

		// Exit block just returns
		exitBlock.NewRet(nil)
	} else {
		// No init function, just call main directly and print success
		if userMain, ok := cg.functions["_ballerina_main"]; ok {
			entryBlock.NewCall(userMain)
			fmt.Println("[DEBUG] Added call to _ballerina_main function from ballerina_main without init")

			// Print success message
			printFunc := cg.functions["ballerina_io_println"]
			if printFunc != nil {
				successMsg := cg.CreateString("Ballerina program completed successfully.", entryBlock)
				entryBlock.NewCall(printFunc, successMsg)
			}
		}
		entryBlock.NewRet(nil)
	}

	// Add a return instruction
	entryBlock.NewRet(nil)

	// Store the function in our map
	cg.functions["ballerina_main"] = mainFunc

	fmt.Println("[DEBUG] Successfully created ballerina_main function")
}
