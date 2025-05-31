// filepath: /Users/rch/src/lightfoot/backend/generate_ballerina_main.go
package backend

import (
"fmt"

"github.com/llir/llvm/ir"
"github.com/llir/llvm/ir/types"
)

// generateBallerinaMain generates the ballerina_main function.
func (cg *CodeGenerator) generateBallerinaMain() {
	fmt.Println("[DEBUG] Generating ballerina_main function")

	// First check if ballerina_main already exists - this would happen if
	// we've already transformed the user's main function to "ballerina_main"
	if _, ok := cg.functions["ballerina_main"]; ok {
		fmt.Println("[DEBUG] ballerina_main function already exists - no need to create it")
		
		// The current implementation transforms the user's main function to "ballerina_main"
// We'll leverage that - the main function already has the right name.
		// We just need to make sure init gets called before the main code.
		
		// Find init function
		var initFunc *ir.Func
		// Look for the function directly by Ballerina name
		if f, ok := cg.functions["init"]; ok {
			initFunc = f
			fmt.Println("[DEBUG] Found init function by name: init")
		}
		
		// Also try the transformed function name
		if initFunc == nil {
			transformedName := cg.getLLVMFunctionName("init")
			if f, ok := cg.functions[transformedName]; ok {
				initFunc = f
				fmt.Printf("[DEBUG] Found init function by transformed name: %s\n", transformedName)
			}
		}
		
		// Finally check if it's stored in the BIR package
if initFunc == nil && cg.birPackage != nil && cg.birPackage.ActualInitFunc != nil {
if f, ok := cg.functions[cg.birPackage.ActualInitFunc.Name]; ok {
initFunc = f
fmt.Printf("[DEBUG] Found init function from BIR package: %s\n", cg.birPackage.ActualInitFunc.Name)
}
}

// Call init function if found - we need a separate function that calls init first
if initFunc != nil {
// Create a new function that will call init, then call main
wrapperFunc := cg.module.NewFunc("_ballerina_init_then_main", types.Void)
wrapperBlock := wrapperFunc.NewBlock("entry")

// Call init
wrapperBlock.NewCall(initFunc)
fmt.Printf("[DEBUG] Added call to init function: %s in wrapper\n", initFunc.Name())

// Call the existing main function (which is now named "ballerina_main")
mainFunc := cg.functions["ballerina_main"]
wrapperBlock.NewCall(mainFunc)
wrapperBlock.NewRet(nil)

// Store the wrapper function
cg.functions["_ballerina_init_then_main"] = wrapperFunc

// Create a new ballerina_main that calls our wrapper
// The runtime expects this exact name
newMainFunc := cg.module.NewFunc("ballerina_main", types.Void)
newMainBlock := newMainFunc.NewBlock("main")
newMainBlock.NewCall(wrapperFunc)
newMainBlock.NewRet(nil)

// Replace the existing function in the map
cg.functions["ballerina_main"] = newMainFunc
cg.functions["_user_main"] = mainFunc  // Save the original under a new name

fmt.Println("[DEBUG] Created wrapper to ensure init is called before main")
}

return
}

// If we get here, we need to create a new ballerina_main function
// Create function type for main: void ()
mainFunc := cg.module.NewFunc("ballerina_main", types.Void)

// Create the entry basic block
entryBlock := mainFunc.NewBlock("entry")

// Find init function
var initFunc *ir.Func
// Look for the function directly by Ballerina name
if f, ok := cg.functions["init"]; ok {
initFunc = f
fmt.Println("[DEBUG] Found init function by name: init")
}

// Also try the transformed function name
if initFunc == nil {
transformedName := cg.getLLVMFunctionName("init")
if f, ok := cg.functions[transformedName]; ok {
initFunc = f
fmt.Printf("[DEBUG] Found init function by transformed name: %s\n", transformedName)
}
}

// Finally check if it's stored in the BIR package
	if initFunc == nil && cg.birPackage != nil && cg.birPackage.ActualInitFunc != nil {
		if f, ok := cg.functions[cg.birPackage.ActualInitFunc.Name]; ok {
			initFunc = f
			fmt.Printf("[DEBUG] Found init function from BIR package: %s\n", cg.birPackage.ActualInitFunc.Name)
		}
	}

	// Call init function if found
	if initFunc != nil {
		entryBlock.NewCall(initFunc)
		fmt.Printf("[DEBUG] Added call to init function: %s\n", initFunc.Name())
	} else {
		// Add a debug print to show init function wasn't found
printStrFunc := cg.functions["ballerina_io_println"]
if printStrFunc != nil {
debugStr := cg.CreateString("No init function found to call", entryBlock)
entryBlock.NewCall(printStrFunc, debugStr)
}
fmt.Println("[WARNING] No init function found to call")
}

// Find main function
var userMainFunc *ir.Func
// Look for the function directly by Ballerina name
if f, ok := cg.functions["main"]; ok {
userMainFunc = f
fmt.Println("[DEBUG] Found main function by name: main")
}

// Also try the transformed function name
if userMainFunc == nil {
transformedName := cg.getLLVMFunctionName("main")
if f, ok := cg.functions[transformedName]; ok {
userMainFunc = f
fmt.Printf("[DEBUG] Found main function by transformed name: %s\n", transformedName)
}
}

// Finally check if it's stored in the BIR package
	if userMainFunc == nil && cg.birPackage != nil && cg.birPackage.ModuleStartFunc != nil {
		if f, ok := cg.functions[cg.birPackage.ModuleStartFunc.Name]; ok {
			userMainFunc = f
			fmt.Printf("[DEBUG] Found main function from BIR package: %s\n", cg.birPackage.ModuleStartFunc.Name)
		}
	}

	// Call main function if found
	if userMainFunc != nil {
		entryBlock.NewCall(userMainFunc)
		fmt.Printf("[DEBUG] Added call to main function: %s\n", userMainFunc.Name())
	} else {
		// Add a debug print to show main function wasn't found
printStrFunc := cg.functions["ballerina_io_println"]
if printStrFunc != nil {
debugStr := cg.CreateString("No main function found to call", entryBlock)
entryBlock.NewCall(printStrFunc, debugStr)
}
fmt.Println("[WARNING] No main function found to call")
}

// Add return instruction
entryBlock.NewRet(nil)

// Store in the functions map
cg.functions["ballerina_main"] = mainFunc

fmt.Println("[DEBUG] Generated ballerina_main function successfully")
}
