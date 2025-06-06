// filepath: /Users/rch/src/lightfoot/backend/generate_ballerina_main.go
package backend

import (
	"wx-yz/lightfoot/debug"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/types"
)

// generateBallerinaMain generates the ballerina_main function.
func (cg *CodeGenerator) generateBallerinaMain() {
	debug.PrintBackend("Generating ballerina_main function")

	// First check if ballerina_main already exists - this would happen if
	// we've already transformed the user's main function to "ballerina_main"
	if _, ok := cg.functions["ballerina_main"]; ok {
		debug.PrintBackend("ballerina_main function already exists - no need to create it")

		// The current implementation transforms the user's main function to "ballerina_main"
		// We'll leverage that - the main function already has the right name.
		// We just need to make sure init gets called before the main code.

		// Find init function
		var initFunc *ir.Func
		// Look for the function directly by Ballerina name
		if f, ok := cg.functions["init"]; ok {
			initFunc = f
		}

		// Also try the transformed function name
		if initFunc == nil {
			transformedName := cg.getLLVMFunctionName("init")
			if f, ok := cg.functions[transformedName]; ok {
				initFunc = f
			}
		}

		// Finally check if it's stored in the BIR package
		if initFunc == nil && cg.birPackage != nil && cg.birPackage.ActualInitFunc != nil {
			if f, ok := cg.functions[cg.birPackage.ActualInitFunc.Name]; ok {
				initFunc = f
			}
		}

		// Call init function if found - we need a separate function that calls init first
		if initFunc != nil {
			// Create a new function that will call init, then call main
			wrapperFunc := cg.module.NewFunc("_ballerina_init_then_main", types.Void)
			wrapperBlock := wrapperFunc.NewBlock("entry")

			// Call init
			wrapperBlock.NewCall(initFunc)

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
			cg.functions["_user_main"] = mainFunc // Save the original under a new name
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
	}

	// Also try the transformed function name
	if initFunc == nil {
		transformedName := cg.getLLVMFunctionName("init")
		if f, ok := cg.functions[transformedName]; ok {
			initFunc = f
		}
	}

	// Finally check if it's stored in the BIR package
	if initFunc == nil && cg.birPackage != nil && cg.birPackage.ActualInitFunc != nil {
		if f, ok := cg.functions[cg.birPackage.ActualInitFunc.Name]; ok {
			initFunc = f
		}
	}

	// Call init function if found
	if initFunc != nil {
		entryBlock.NewCall(initFunc)
	}

	// Find main function
	var userMainFunc *ir.Func
	// Look for the function directly by Ballerina name
	if f, ok := cg.functions["main"]; ok {
		userMainFunc = f
	}

	// Also try the transformed function name
	if userMainFunc == nil {
		transformedName := cg.getLLVMFunctionName("main")
		if f, ok := cg.functions[transformedName]; ok {
			userMainFunc = f
		}
	}

	// Finally check if it's stored in the BIR package
	if userMainFunc == nil && cg.birPackage != nil && cg.birPackage.ModuleStartFunc != nil {
		if f, ok := cg.functions[cg.birPackage.ModuleStartFunc.Name]; ok {
			userMainFunc = f
		}
	}

	// Call main function if found
	if userMainFunc != nil {
		entryBlock.NewCall(userMainFunc)
	}

	// Add return instruction
	entryBlock.NewRet(nil)

	// Store in the functions map
	cg.functions["ballerina_main"] = mainFunc
}
