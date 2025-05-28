package backend

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
)

// AddRuntimeTypes adds runtime type declarations to the module
func (cg *CodeGenerator) AddRuntimeTypes() {
	// Define BallerinaString struct
	strFields := []types.Type{
		types.I64,                  // length
		types.NewPointer(types.I8), // data
	}
	strType := types.NewStruct(strFields...)
	cg.structTypes["BallerinaString"] = strType

	// Define BallerinaArray struct
	arrFields := []types.Type{
		types.I64,                  // length
		types.NewPointer(types.I8), // data (void*)
	}
	arrType := types.NewStruct(arrFields...)
	cg.structTypes["BallerinaArray"] = arrType

	// Define HTTP types for service support
	httpReqFields := []types.Type{
		types.NewPointer(strType),  // path
		types.NewPointer(strType),  // method
		types.NewPointer(types.I8), // body (raw data)
		types.I64,                  // bodyLength
	}
	httpReqType := types.NewStruct(httpReqFields...)
	cg.structTypes["BallerinaHTTPRequest"] = httpReqType

	httpRespFields := []types.Type{
		types.I32,                  // statusCode
		types.NewPointer(types.I8), // body (raw data)
		types.I64,                  // bodyLength
	}
	httpRespType := types.NewStruct(httpRespFields...)
	cg.structTypes["BallerinaHTTPResponse"] = httpRespType

	// Define error type
	errorFields := []types.Type{
		types.NewPointer(strType), // message
	}
	errorType := types.NewStruct(errorFields...)
	cg.structTypes["BallerinaError"] = errorType
}

// AddRuntimeFunctions adds declarations for runtime functions to the module
func (cg *CodeGenerator) AddRuntimeFunctions() {
	// Basic runtime functions
	cg.module.NewFunc("print_int", types.Void, ir.NewParam("value", types.I64))
	cg.module.NewFunc("print_string", types.Void, ir.NewParam("str", types.NewPointer(cg.structTypes["BallerinaString"])))

	// String functions
	cg.module.NewFunc("ballerina_string_concat",
		types.NewPointer(cg.structTypes["BallerinaString"]),
		ir.NewParam("str1", types.NewPointer(cg.structTypes["BallerinaString"])),
		ir.NewParam("str2", types.NewPointer(cg.structTypes["BallerinaString"])))

	// HTTP server functions
	httpReqPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPRequest"])
	httpRespPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPResponse"])

	// Function signature for HTTP resource handlers
	resourceHandlerType := types.NewPointer(types.NewFunc(types.Void, httpReqPtrType, httpRespPtrType))

	// HTTP server API
	cg.module.NewFunc("ballerina_http_server_start", types.I32,
		ir.NewParam("port", types.I32),
		ir.NewParam("registerNow", types.I32))

	cg.module.NewFunc("ballerina_http_server_wait", types.Void)

	cg.module.NewFunc("ballerina_http_register_resource", types.I32,
		ir.NewParam("path", types.NewPointer(cg.structTypes["BallerinaString"])),
		ir.NewParam("method", types.NewPointer(cg.structTypes["BallerinaString"])),
		ir.NewParam("handler", resourceHandlerType))

	// Error handling functions
	errorPtrType := types.NewPointer(cg.structTypes["BallerinaError"])
	cg.module.NewFunc("ballerina_create_error", errorPtrType,
		ir.NewParam("message", types.NewPointer(cg.structTypes["BallerinaString"])))

	cg.module.NewFunc("ballerina_print_error", types.Void,
		ir.NewParam("error", errorPtrType))

	// Function to check if init function returned an error and handle it
	cg.module.NewFunc("ballerina_handle_init_error", types.Void,
		ir.NewParam("error", errorPtrType))
}

// AddRuntimeGlobals adds global variables required by the runtime
func (cg *CodeGenerator) AddRuntimeGlobals() {
	// Add global counter for string IDs
	stringCounter := cg.module.NewGlobalDef("stringIdCounter", constant.NewInt(types.I64, 0))
	stringCounter.Linkage = enum.LinkagePrivate

	// Add init completed flag
	initCompletedFlag := cg.module.NewGlobalDef("initCompleted", constant.NewInt(types.I1, 0))
	initCompletedFlag.Linkage = enum.LinkagePrivate
}

// AddRuntimeInit creates the initialization function that sets up the runtime
// and calls the user's init function if defined
func (cg *CodeGenerator) AddRuntimeInit() {
	// Create the Ballerina runtime initialization function
	runtimeInitFunc := cg.module.NewFunc("ballerina_runtime_init", types.Void)
	entryBlock := runtimeInitFunc.NewBlock("entry")

	// Set up required runtime state
	// (This is a placeholder - implement actual runtime initialization as needed)

	// Check if we have a user-defined init function
	if cg.birPackage.ActualInitFunc != nil {
		// Get the LLVM function for the user's init function
		initFuncName := cg.birPackage.ActualInitFunc.Name
		userInitFunc := cg.functions[initFuncName]

		if userInitFunc != nil {
			// Call the user's init function
			initResult := entryBlock.NewCall(userInitFunc)

			// If the init function returns an error, handle it
			if cg.birPackage.ActualInitFunc.ReturnVariable != nil &&
				cg.birPackage.ActualInitFunc.ReturnVariable.Type == "error?" {
				// Call error handling function with the result
				errorHandlerFunc := cg.getLLVMFunction("ballerina_handle_init_error")
				if errorHandlerFunc != nil {
					entryBlock.NewCall(errorHandlerFunc, initResult)
				}
			}
		}
	}

	// Set the init completed flag
	var initCompletedGlobal *ir.Global
	for _, g := range cg.module.Globals {
		if g.Name() == "initCompleted" {
			initCompletedGlobal = g
			break
		}
	}
	if initCompletedGlobal != nil {
		entryBlock.NewStore(constant.NewInt(types.I1, 1), initCompletedGlobal)
	}

	// Return from the initialization function
	entryBlock.NewRet(nil)

	// Ensure this function is called before main
	cg.functions["$runtime_init"] = runtimeInitFunc

	// Now modify the ballerina_main function to call runtime init first
	mainFunc := cg.functions["$ballerina_main_wrapper"]
	if mainFunc != nil && len(mainFunc.Blocks) > 0 {
		// Get the first block
		firstBlock := mainFunc.Blocks[0]

		// Create a new entry block that will call runtime init first
		newEntryBlock := mainFunc.NewBlock("init_entry")

		// Call runtime init
		newEntryBlock.NewCall(runtimeInitFunc)

		// Jump to the original first block
		newEntryBlock.NewBr(firstBlock)

		// Reorder the blocks to make the new entry block first
		mainFunc.Blocks = append([]*ir.Block{newEntryBlock}, mainFunc.Blocks...)
	}
}
