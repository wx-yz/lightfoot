// Add error handling functions to the runtime
package backend

import (
"fmt"

"github.com/llir/llvm/ir"
"github.com/llir/llvm/ir/types"
)

// addErrorFunctions adds functions for creating and handling Ballerina errors
func (cg *CodeGenerator) addErrorFunctions() {
	// Get the BallerinaString type pointer
	var ballerinaStringTypePtr types.Type
	if stringType, ok := cg.structTypes["BallerinaString"]; ok {
		ballerinaStringTypePtr = types.NewPointer(stringType)
	} else {
		fmt.Println("[ERROR] BallerinaString type not found, error functions won't work properly")
		return
	}

	// Create a function that creates an error from a string message
	// In Ballerina, errors are represented as a special kind of object
	// For simplicity, we'll just use the string pointer as the error value
errorCreateFunc := cg.module.NewFunc(
"ballerina_error_create",
types.I8Ptr, // Return an opaque pointer representing the error
ir.NewParam("message", ballerinaStringTypePtr),
)

// Store the function
cg.functions["ballerina_error_create"] = errorCreateFunc

// Function to print error messages
errorPrintFunc := cg.module.NewFunc(
"ballerina_error_print",
types.Void,
ir.NewParam("error", types.I8Ptr),
)

// Store the function
cg.functions["ballerina_error_print"] = errorPrintFunc

fmt.Println("[DEBUG] Added error handling functions to the runtime")
}
