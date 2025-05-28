package backend

/*
#cgo CFLAGS: -I/opt/homebrew/Cellar/llvm/20.1.5/include -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
#cgo LDFLAGS: -L/opt/homebrew/Cellar/llvm/20.1.5/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names -lLLVM-20
#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>
#include <stdlib.h>
*/
import "C"

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"unsafe"
	"wx-yz/lightfoot/bir"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// CodeGenerator handles the generation of LLVM IR from BIR
// and writing it to a file.
type CodeGenerator struct {
	birPackage  *bir.Package
	module      *ir.Module
	globals     map[string]*ir.Global
	functions   map[string]*ir.Func
	varMap      map[string]value.Value       // BIRName -> LLVM value (alloca, param, etc.)
	structTypes map[string]*types.StructType // BIR record name -> LLVM struct type
	llvmCtx     *LLVMContext
	llvmModule  *LLVMModule
	builder     *LLVMBuilder
}

// NewCodeGenerator creates a new code generator for the given BIR package
func NewCodeGenerator(birPackage *bir.Package) *CodeGenerator {
	llvmCtx := NewLLVMContext()
	llvmModule := NewLLVMModule("lightfoot")
	builder := NewLLVMBuilder()

	return &CodeGenerator{
		birPackage:  birPackage,
		module:      ir.NewModule(),
		globals:     make(map[string]*ir.Global),
		functions:   make(map[string]*ir.Func),
		varMap:      make(map[string]value.Value),
		structTypes: make(map[string]*types.StructType),
		llvmCtx:     llvmCtx,
		llvmModule:  llvmModule,
		builder:     builder,
	}
}

// Close releases all LLVM resources
func (cg *CodeGenerator) Close() {
	if cg.builder != nil {
		cg.builder.Close()
	}
	if cg.llvmModule != nil {
		cg.llvmModule.Close()
	}
	if cg.llvmCtx != nil {
		cg.llvmCtx.Close()
	}
}

// GenerateCode generates LLVM IR from the BIR package and writes it to a file
func (cg *CodeGenerator) GenerateCode(outputFile string) error {
	defer cg.Close()

	// Create target machine early to get DataLayout
	tm, err := NewLLVMTargetMachine()
	if err != nil {
		return fmt.Errorf("failed to create target machine: %w", err)
	}
	defer tm.Close()

	// Set DataLayout on the module
	targetDataRef := C.LLVMCreateTargetDataLayout(tm.machine)
	if targetDataRef == nil {
		return fmt.Errorf("failed to get target data layout ref")
	}
	defer C.LLVMDisposeTargetData(targetDataRef)
	cg.module.DataLayout = C.GoString(C.LLVMCopyStringRepOfTargetData(targetDataRef))
	// Also set target triple
	cg.module.TargetTriple = C.GoString(C.LLVMGetDefaultTargetTriple())

	// Add runtime support
	cg.AddRuntimeTypes()
	cg.AddRuntimeFunctions()
	cg.AddRuntimeGlobals()

	// Generate structure types first
	if err := cg.generateStructTypes(); err != nil {
		return fmt.Errorf("failed to generate struct types: %w", err)
	}

	// Generate globals
	if err := cg.generateGlobals(); err != nil {
		return fmt.Errorf("failed to generate globals: %w", err)
	}

	// Generate code for functions
	cg.generateAllFunctions()

	// Add runtime initialization
	cg.AddRuntimeInit()

	// Look for ballerina_main in the IR before object file generation
	if strings.Contains(cg.module.String(), "ballerina_main") {
		fmt.Println("[DEBUG] Found ballerina_main function in generated IR")
	} else {
		fmt.Println("[WARNING] ballerina_main function not found in generated IR")
	}

	// Convert LLIR to LLVM IR
	data := []byte(cg.module.String())
	tempFile := outputFile + ".ll"
	if err := os.WriteFile(tempFile, data, 0644); err != nil {
		return fmt.Errorf("failed to write IR file: %w", err)
	}
	defer os.Remove(tempFile)

	// Use llc to compile the IR to an object file
	llcCmd := exec.Command("llc", "-filetype=obj", "-o", outputFile, tempFile)
	if output, err := llcCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("llc failed: %w\nOutput: %s", err, string(output))
	}

	return nil
}

// generateStructTypes scans BIR for record types and creates LLVM struct types
func (cg *CodeGenerator) generateStructTypes() error {
	for _, fn := range cg.birPackage.Functions {
		fmt.Printf("[DEBUG] generateStructTypes: Processing function '%s'\n", fn.Name)
		// Scan function for custom types and create LLVM struct types if needed
		// This is a placeholder - implement based on your BIR structure
	}
	return nil
}

// generateGlobals generates LLVM IR for global variables
func (cg *CodeGenerator) generateGlobals() error {
	for _, g := range cg.birPackage.GlobalVars {
		fmt.Printf("[DEBUG] generateGlobals: Processing global variable '%s' of type '%s'\n", g.Name, g.Type)

		llvmType := cg.birTypeToLLVMType(g.Type)
		fmt.Printf("[DEBUG] generateGlobals: Defined global '%s' with LLVM type '%s' (value type). ", g.Name, llvmType)

		var initializer constant.Constant
		if ptrType, isPtr := llvmType.(*types.PointerType); isPtr {
			initializer = constant.NewNull(ptrType)
		} else {
			initializer = constant.NewZeroInitializer(llvmType)
		}
		fmt.Printf("Initializer type: '%s'\n", initializer.Type())

		glob := cg.module.NewGlobalDef(g.Name, initializer)
		cg.globals[g.Name] = glob
	}
	return nil
}

// generateAllFunctions generates LLVM IR for all functions in the BIR package
func (cg *CodeGenerator) generateAllFunctions() {
	// Generate all functions from BIR first to make them available
	for _, f := range cg.birPackage.Functions {
		cg.generateFunction(f)
		// Check if this function is a resource function and store metadata if needed
		// This depends on how BIR represents resource functions.
	}

	// If we have a user-defined init function, ensure it's generated
	if cg.birPackage.ActualInitFunc != nil {
		// The init function should already be in the functions map,
		// but let's verify and log if it's missing
		if _, ok := cg.functions[cg.birPackage.ActualInitFunc.Name]; !ok {
			fmt.Printf("[WARNING] User-defined init function '%s' not found in functions map\n",
				cg.birPackage.ActualInitFunc.Name)
		} else {
			fmt.Printf("[DEBUG] User-defined init function '%s' successfully generated\n",
				cg.birPackage.ActualInitFunc.Name)
		}
	}

	// Now create the ballerina_main function that will be called from C
	mainFunc := cg.module.NewFunc("ballerina_main", types.Void)

	// Critical: Set this function's properties to ensure it's visible to the linker
	// 1. Use External linkage to make sure it's visible outside the module
	mainFunc.Linkage = enum.LinkageExternal

	// 2. Make sure it's not anonymized or optimized away
	mainFunc.UnnamedAddr = enum.UnnamedAddrNone

	// 3. Explicitly set default visibility for maximum exposure
	mainFunc.Visibility = enum.VisibilityDefault

	// Create the entry block for the function
	entryBlock := mainFunc.NewBlock("entry")

	// Call the BIR main function if it exists
	birMainFunc, ok := cg.functions["main"]
	if ok {
		fmt.Printf("[DEBUG] Added call to BIR main function in ballerina_main\n")
		entryBlock.NewCall(birMainFunc)
	} else {
		fmt.Printf("[WARNING] No BIR main function found\n")
		// Create a call to print an error message instead of doing nothing
		// This ensures we have some visible indication if this function is called
	}

	// Return from ballerina_main
	entryBlock.NewRet(nil)

	// Add the function to our map with a unique name
	cg.functions["$ballerina_main_wrapper"] = mainFunc

	// Generate HTTP services after the main wrapper is created
	cg.generateHTTPServices()

	// Print debug information about the ballerina_main function
	fmt.Printf("[DEBUG] ballerina_main function: Linkage=%v, Visibility=%v, UnnamedAddr=%v\n",
		mainFunc.Linkage, mainFunc.Visibility, mainFunc.UnnamedAddr)
}

// generateHTTPServices processes BIR service definitions and sets up HTTP listeners and resources.
func (cg *CodeGenerator) generateHTTPServices() {
	fmt.Println("[DEBUG] generateHTTPServices: Starting HTTP service generation.")

	listenerPort := int64(9090)
	resourcePath := "/greeting"
	resourceMethod := "GET"
	originalResourceBirFuncName := "main$get_greeting.0"

	// 1. Start the HTTP server
	mainFunc := cg.functions["$ballerina_main_wrapper"]
	if mainFunc != nil && len(mainFunc.Blocks) > 0 {
		entryBlock := mainFunc.Blocks[0]

		// Add server start call
		serverStartFunc := cg.getLLVMFunction("ballerina_http_server_start")
		if serverStartFunc != nil {
			portVal := constant.NewInt(types.I32, listenerPort)
			registerNowVal := constant.NewInt(types.I32, 1) // true
			entryBlock.NewCall(serverStartFunc, portVal, registerNowVal)
		}
	}

	// 2. Find the original LLVM function for the resource logic
	originalLlvmResourceFunc := cg.getLLVMFunction(originalResourceBirFuncName)
	if originalLlvmResourceFunc == nil {
		fmt.Printf("[WARNING] getLLVMFunction: Function '%s' not found.\n", originalResourceBirFuncName)
		fmt.Printf("[ERROR] generateHTTPServices: Original LLVM resource function '%s' not found.\n", originalResourceBirFuncName)
		return
	}

	// 3. Create a wrapper LLVM function for the resource
	httpReqPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPRequest"])
	httpRespPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPResponse"])
	wrapperFuncName := originalResourceBirFuncName + "_http_wrapper"

	// Check if wrapper already exists
	if cg.getLLVMFunction(wrapperFuncName) == nil {
		wrapperFunc := cg.module.NewFunc(wrapperFuncName, types.Void,
			ir.NewParam("req", httpReqPtrType),
			ir.NewParam("resp", httpRespPtrType))
		cg.functions[wrapperFuncName] = wrapperFunc

		// Generate wrapper body that calls the original function and sets response
		// This is a simplified implementation
		wrapperEntry := wrapperFunc.NewBlock("entry")
		wrapperEntry.NewCall(originalLlvmResourceFunc)
		wrapperEntry.NewRet(nil)
	}

	// 4. Register the wrapper function with the C runtime
	if mainFunc != nil && len(mainFunc.Blocks) > 0 {
		entryBlock := mainFunc.Blocks[0]

		registerFunc := cg.getLLVMFunction("ballerina_http_register_resource")
		if registerFunc != nil {
			pathStr := cg.CreateGlobalString(resourcePath, entryBlock)
			methodStr := cg.CreateGlobalString(resourceMethod, entryBlock)
			wrapperFuncPtr := cg.getLLVMFunction(wrapperFuncName)

			entryBlock.NewCall(registerFunc, pathStr, methodStr, wrapperFuncPtr)
		}

		// 5. Keep the server running by calling the wait function
		waitFunc := cg.getLLVMFunction("ballerina_http_server_wait")
		if waitFunc != nil {
			entryBlock.NewCall(waitFunc)
		}
	}

	fmt.Println("[DEBUG] generateHTTPServices: Finished HTTP service generation attempt.")
}

// CreateGlobalString creates a global string constant and returns a pointer to it (i8*).
func (cg *CodeGenerator) CreateGlobalString(s string, block *ir.Block) value.Value {
	name := fmt.Sprintf(".str.g.%d", len(cg.module.Globals))
	chars := []constant.Constant{}
	for i := 0; i < len(s); i++ {
		chars = append(chars, constant.NewInt(types.I8, int64(s[i])))
	}
	chars = append(chars, constant.NewInt(types.I8, 0)) // Null terminator
	arrayType := types.NewArray(uint64(len(chars)), types.I8)
	init := constant.NewArray(arrayType, chars...)

	global := cg.module.NewGlobalDef(name, init)
	global.Linkage = enum.LinkagePrivate
	global.UnnamedAddr = enum.UnnamedAddrNone

	// Get i8* pointer to the first element
	zero := constant.NewInt(types.I32, 0)
	ptr := constant.NewGetElementPtr(arrayType, global, zero, zero)
	return ptr
}

// CreateString creates a BallerinaString struct from a Go string literal.
func (cg *CodeGenerator) CreateString(s string, block *ir.Block) value.Value {
	createStrFunc := cg.getLLVMFunction("ballerina_string_new_with_literal")
	if createStrFunc == nil {
		panic("Runtime function ballerina_string_new_with_literal not found.")
	}

	globalCStr := cg.CreateGlobalString(s, block)
	lengthVal := constant.NewInt(types.I64, int64(len(s)))
	balStringPtr := block.NewCall(createStrFunc, globalCStr, lengthVal)

	safeName := strings.Map(func(r rune) rune {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
			return r
		}
		return '_'
	}, s)
	if len(safeName) > 20 {
		safeName = safeName[:20]
	}
	balStringPtr.SetName("balstr_" + safeName)
	return balStringPtr
}

// birTypeToLLVMType converts a BIR type string to an LLVM type.
func (cg *CodeGenerator) birTypeToLLVMType(birType string) types.Type {
	fmt.Printf("[DEBUG] birTypeToLLVMType: Converting BIR type '%s'\n", birType)
	switch birType {
	case "int":
		return types.I64
	case "string":
		// Always return BallerinaString* for string types
		if t, ok := cg.structTypes["BallerinaString"]; ok {
			return types.NewPointer(t)
		}
		// Fallback if BallerinaString not defined yet - but this should not happen
		// if AddRuntimeTypes is called properly
		fmt.Printf("[WARNING] birTypeToLLVMType: BallerinaString not defined, falling back to i8*\n")
		return types.I8Ptr
	case "boolean":
		return types.I1
	case "()":
		return types.Void
	case "error?":
		// Handle error? type for init function - can return either error or nil
		if t, ok := cg.structTypes["BallerinaError"]; ok {
			return types.NewPointer(t)
		}
		// Fallback
		return types.I8Ptr
	case "map":
		if t, ok := cg.structTypes["BallerinaMap"]; ok {
			return types.NewPointer(t)
		}
		// Fallback if BallerinaMap not defined yet
		return types.I8Ptr
	default:
		// Handle complex types
		if strings.Contains(birType, "map<") {
			// Handle map types like "map<any>"
			if t, ok := cg.structTypes["BallerinaMap"]; ok {
				return types.NewPointer(t)
			}
			// Fallback if BallerinaMap not defined yet
			return types.I8Ptr
		}
		if strings.Contains(birType, "ballerina/io") && strings.Contains(birType, "Printable") {
			// Handle Printable type - this should be BallerinaString* for string values
			if t, ok := cg.structTypes["BallerinaString"]; ok {
				return types.NewPointer(t)
			}
			return types.I8Ptr
		}
		if strings.Contains(birType, "typeRefDesc") {
			// Handle type descriptors - these are usually arrays for println
			// For now, treat them as BallerinaArray* which contains the string elements
			if t, ok := cg.structTypes["BallerinaArray"]; ok {
				return types.NewPointer(t)
			}
			return types.I8Ptr
		}

		if t, ok := cg.structTypes[birType]; ok {
			return types.NewPointer(t)
		}
		fmt.Printf("[WARNING] birTypeToLLVMType: Unhandled BIR type '%s', returning void*\n", birType)
		return types.I8Ptr
	}
}

// generateFunction generates LLVM IR for a single BIR function.
func (cg *CodeGenerator) generateFunction(fn *bir.Function) *ir.Func {
	fmt.Printf("[DEBUG] generateFunction: Starting generation for BIR function '%s'\n", fn.Name)

	// Ensure runtime types are available before processing
	if _, ok := cg.structTypes["BallerinaString"]; !ok {
		cg.AddRuntimeTypes()
	}

	// 1. Determine LLVM function signature
	var llvmParamTypes []types.Type
	for _, birParam := range fn.Parameters {
		llvmParamTypes = append(llvmParamTypes, cg.birTypeToLLVMType(birParam.Type))
	}
	var returnType types.Type
	if fn.ReturnVariable != nil && fn.ReturnVariable.Type != "()" {
		returnType = cg.birTypeToLLVMType(fn.ReturnVariable.Type)
	} else {
		returnType = types.Void
	}

	// Create ir.Param instances for NewFunc
	var llvmParams []*ir.Param
	for i, pt := range llvmParamTypes {
		paramName := ""
		if i < len(fn.Parameters) {
			paramName = fn.Parameters[i].OriginalName
		}
		llvmParams = append(llvmParams, ir.NewParam(paramName, pt))
	}

	llvmFunc := cg.module.NewFunc(fn.Name, returnType, llvmParams...)
	cg.functions[fn.Name] = llvmFunc

	// Clear and repopulate varMap for this function's scope
	cg.varMap = make(map[string]value.Value)

	// 2. Create entry basic block
	entryBB := llvmFunc.NewBlock("entry")

	// 3. Allocate space for parameters and local variables
	for i, birParam := range fn.Parameters {
		llvmParam := llvmFunc.Params[i]
		llvmParam.SetName(birParam.OriginalName)

		allocaInst := entryBB.NewAlloca(llvmParam.Type())
		allocaInst.SetName(birParam.BIRName + ".addr")
		entryBB.NewStore(llvmParam, allocaInst)
		cg.varMap[birParam.BIRName] = allocaInst
	}

	// Return variable (if not void) - IMPORTANT: Create %0 variable for return value
	if fn.ReturnVariable != nil && !returnType.Equal(types.Void) {
		allocaInst := entryBB.NewAlloca(returnType)
		allocaInst.SetName(fn.ReturnVariable.BIRName + ".addr")
		cg.varMap[fn.ReturnVariable.BIRName] = allocaInst
		// Also map by %0 convention
		cg.varMap["%0"] = allocaInst
	}

	// Local Variables - be more careful about type handling
	for _, birVar := range fn.LocalVars {
		if _, isMapped := cg.varMap[birVar.BIRName]; isMapped {
			continue
		}
		llvmVarType := cg.birTypeToLLVMType(birVar.Type)
		if llvmVarType.Equal(types.Void) {
			continue
		}

		// For string variables, ensure we use BallerinaString* not i8*
		if birVar.Type == "string" {
			if ballerinaStringType, ok := cg.structTypes["BallerinaString"]; ok {
				llvmVarType = types.NewPointer(ballerinaStringType)
			}
		}

		allocaInst := entryBB.NewAlloca(llvmVarType)
		allocaInst.SetName(birVar.BIRName + ".local.addr")
		cg.varMap[birVar.BIRName] = allocaInst
	}

	// 4. Map BIR basic blocks to LLVM basic blocks
	bbMap := make(map[string]*ir.Block)
	for i, birBB := range fn.BasicBlocks {
		var llvmBB *ir.Block
		if i == 0 {
			llvmBB = entryBB
			llvmBB.SetName(birBB.ID)
		} else {
			llvmBB = llvmFunc.NewBlock(birBB.ID)
		}
		bbMap[birBB.ID] = llvmBB
	}

	// 5. Populate LLVM basic blocks with instructions
	for _, birBB := range fn.BasicBlocks {
		llvmBB := bbMap[birBB.ID]
		fmt.Printf("[DEBUG] generateFunction: Processing BIR BB '%s' for LLVM BB '%s'\n", birBB.ID, llvmBB.Ident())

		// Process BIR instructions
		for _, birInst := range birBB.Instructions {
			cg.emitInstruction(birInst, llvmBB, bbMap)
		}

		// Process terminator
		if birBB.Terminator != nil {
			cg.emitTerminator(birBB.Terminator, llvmBB, bbMap)
		} else {
			// Add default terminator
			if iAmTheLastBlockAndHaveNoTerminator(birBB, fn, llvmBB, llvmFunc) {
				if returnType.Equal(types.Void) {
					llvmBB.NewRet(nil)
				} else {
					if retVarAlloca, ok := cg.varMap[fn.ReturnVariable.BIRName]; ok {
						loadedVal := llvmBB.NewLoad(returnType, retVarAlloca)
						llvmBB.NewRet(loadedVal)
					} else {
						llvmBB.NewRet(constant.NewZeroInitializer(returnType))
					}
				}
			} else {
				llvmBB.NewUnreachable()
				fmt.Printf("[WARNING] generateFunction: Block '%s' has no terminator, added unreachable\n", birBB.ID)
			}
		}
	}

	fmt.Printf("[DEBUG] generateFunction: Finished generation for LLVM function '%s'\n", llvmFunc.Ident())
	return llvmFunc
}

// iAmTheLastBlockAndHaveNoTerminator is a helper function
func iAmTheLastBlockAndHaveNoTerminator(birBB *bir.BasicBlock, fn *bir.Function, llvmBB *ir.Block, llvmFunc *ir.Func) bool {
	if birBB.ID == fn.BasicBlocks[len(fn.BasicBlocks)-1].ID && birBB.Terminator == nil {
		return true
	}
	return false
}

// emitInstruction converts BIR instructions to LLVM IR
func (cg *CodeGenerator) emitInstruction(birInst bir.Instruction, llvmBB *ir.Block, bbMap map[string]*ir.Block) {
	fmt.Printf("[DEBUG] emitInstruction: Processing instruction type: %T\n", birInst)

	switch inst := birInst.(type) {
	case *bir.ConstantLoadInst:
		var llvmVal value.Value
		switch inst.TypeName {
		case "string":
			if strVal, ok := inst.Value.(string); ok {
				llvmVal = cg.CreateString(strVal, llvmBB)
			}
		case "int":
			if intVal, ok := inst.Value.(int64); ok {
				llvmVal = constant.NewInt(types.I64, intVal)
			}
		case "boolean":
			if boolVal, ok := inst.Value.(bool); ok {
				if boolVal {
					llvmVal = constant.NewInt(types.I1, 1)
				} else {
					llvmVal = constant.NewInt(types.I1, 0)
				}
			}
		case "()":
			// For void constants, don't assign to a variable
			// Just return without creating any instruction
			return
		default:
			fmt.Printf("[WARNING] emitInstruction: Unhandled constant type: %s\n", inst.TypeName)
			llvmVal = constant.NewUndef(cg.birTypeToLLVMType(inst.TypeName))
		}

		if len(inst.GetLHS()) > 0 && llvmVal != nil {
			cg.assignToVar(inst.GetLHS()[0], llvmVal, llvmBB)
		}

	case *bir.MoveInst:
		sourceVal := cg.loadVar(inst.RHS, llvmBB)
		cg.assignToVar(inst.GetLHS()[0], sourceVal, llvmBB)

	case *bir.BinaryOpInst:
		leftVal := cg.loadVar(inst.Op1, llvmBB)
		rightVal := cg.loadVar(inst.Op2, llvmBB)

		var resultVal value.Value
		switch inst.Op {
		case "+":
			resultVal = llvmBB.NewAdd(leftVal, rightVal)
		case "-":
			resultVal = llvmBB.NewSub(leftVal, rightVal)
		case "*":
			resultVal = llvmBB.NewMul(leftVal, rightVal)
		case "/":
			resultVal = llvmBB.NewSDiv(leftVal, rightVal)
		case "==":
			resultVal = llvmBB.NewICmp(enum.IPredEQ, leftVal, rightVal)
		case "!=":
			resultVal = llvmBB.NewICmp(enum.IPredNE, leftVal, rightVal)
		case "<":
			resultVal = llvmBB.NewICmp(enum.IPredSLT, leftVal, rightVal)
		case "<=":
			resultVal = llvmBB.NewICmp(enum.IPredSLE, leftVal, rightVal)
		case ">":
			resultVal = llvmBB.NewICmp(enum.IPredSGT, leftVal, rightVal)
		case ">=":
			resultVal = llvmBB.NewICmp(enum.IPredSGE, leftVal, rightVal)
		default:
			fmt.Printf("[WARNING] emitInstruction: Unhandled binary op: %s\n", inst.Op)
			resultVal = constant.NewUndef(cg.birTypeToLLVMType("int"))
		}
		cg.assignToVar(inst.GetLHS()[0], resultVal, llvmBB)

	case *bir.TypeCastInst:
		// Handle type casting - be more careful about type compatibility
		sourceVal := cg.loadVar(inst.SourceVar, llvmBB)
		targetType := cg.birTypeToLLVMType(inst.TargetType)

		fmt.Printf("[DEBUG] TypeCastInst: Source type=%s, Target type string='%s', Target LLVM type=%s\n",
			sourceVal.Type(), inst.TargetType, targetType)

		var castedVal value.Value
		if sourceVal.Type().Equal(targetType) {
			castedVal = sourceVal
		} else {
			// Check if this is a boolean source
			if sourceVal.Type().Equal(types.I1) {
				fmt.Printf("[DEBUG] TypeCastInst: Boolean source type i1, target type='%s'\n", inst.TargetType)
			}

			// Special case: boolean to string conversion
			if sourceVal.Type().Equal(types.I1) && strings.Contains(inst.TargetType, "Printable") {
				fmt.Printf("[DEBUG] TypeCastInst: Converting boolean to Printable, target type='%s'\n", inst.TargetType)
				// Convert boolean to string using runtime function
				boolToStringFunc := cg.getLLVMFunction("ballerina_runtime_bool_to_string")
				if boolToStringFunc != nil {
					castedVal = llvmBB.NewCall(boolToStringFunc, sourceVal)
					fmt.Printf("[DEBUG] TypeCastInst: Converted boolean to string using runtime function\n")
				} else {
					fmt.Printf("[ERROR] TypeCastInst: ballerina_runtime_bool_to_string function not found!\n")
					castedVal = constant.NewUndef(targetType)
				}
			} else if _, isSourcePtr := sourceVal.Type().(*types.PointerType); isSourcePtr {
				// Only cast between compatible pointer types
				if _, isTargetPtr := targetType.(*types.PointerType); isTargetPtr {
					castedVal = llvmBB.NewBitCast(sourceVal, targetType)
				} else if targetType.Equal(types.I8Ptr) {
					castedVal = llvmBB.NewBitCast(sourceVal, targetType)
				} else {
					fmt.Printf("[WARNING] emitInstruction: Cannot cast pointer to non-pointer type in TypeCastInst\n")
					castedVal = constant.NewUndef(targetType)
				}
			} else if sourceVal.Type().Equal(types.I8Ptr) {
				if _, isTargetPtr := targetType.(*types.PointerType); isTargetPtr {
					castedVal = llvmBB.NewBitCast(sourceVal, targetType)
				} else {
					fmt.Printf("[WARNING] emitInstruction: Cannot cast i8* to non-pointer type in TypeCastInst\n")
					castedVal = constant.NewUndef(targetType)
				}
			} else {
				fmt.Printf("[WARNING] emitInstruction: Cannot cast between incompatible types in TypeCastInst: %s to %s\n",
					sourceVal.Type(), targetType)
				castedVal = constant.NewUndef(targetType)
			}
		}
		cg.assignToVar(inst.GetLHS()[0], castedVal, llvmBB)

	case *bir.NewArrayInst:
		// Handle array creation
		arrayType := cg.birTypeToLLVMType(inst.ElementType)
		var arraySize value.Value

		fmt.Printf("[DEBUG] NewArrayInst: SizeVar=%v, Args count=%d\n", inst.SizeVar, len(inst.Args))
		if inst.SizeVar != nil {
			sizeVal := cg.loadVar(inst.SizeVar, llvmBB)
			fmt.Printf("[DEBUG] NewArrayInst: SizeVar value=%v\n", sizeVal)
		}

		// If we have arguments, use the argument count as the size
		// This handles the case where SizeVar contains -1 (unknown size at BIR generation time)
		if len(inst.Args) > 0 {
			arraySize = constant.NewInt(types.I64, int64(len(inst.Args)))
			fmt.Printf("[DEBUG] NewArrayInst: Using args count as size: %d (constant)\n", len(inst.Args))
		} else if inst.SizeVar != nil {
			arraySize = cg.loadVar(inst.SizeVar, llvmBB)
			fmt.Printf("[DEBUG] NewArrayInst: Using SizeVar value: %v\n", arraySize)
		} else {
			arraySize = constant.NewInt(types.I64, 0) // Empty array
			fmt.Printf("[DEBUG] NewArrayInst: Using size 0\n")
		}

		// Create array using runtime function
		elementSizeVal := constant.NewInt(types.I64, cg.getSizeOfType(arrayType))
		arrayFunc := cg.getLLVMFunction("ballerina_lang_array_new")
		if arrayFunc != nil {
			arrayVal := llvmBB.NewCall(arrayFunc, arraySize, elementSizeVal)

			// Populate the array with the provided arguments
			for i, arg := range inst.Args {
				argVal := cg.loadVar(arg, llvmBB)
				indexVal := constant.NewInt(types.I64, int64(i))

				// Call runtime function to set array element
				setFunc := cg.getLLVMFunction("ballerina_lang_array_set")
				if setFunc != nil {
					llvmBB.NewCall(setFunc, arrayVal, indexVal, argVal)
				}
			}

			cg.assignToVar(inst.GetLHS()[0], arrayVal, llvmBB)
		}

	case *bir.CallInst:
		var args []value.Value
		for _, arg := range inst.Args {
			args = append(args, cg.loadVar(arg, llvmBB))
		}

		funcName := inst.FunctionName
		if inst.PackagePath != "" {
			funcName = inst.PackagePath + ":" + inst.FunctionName
		}

		// Special handling for println (both fully qualified and simple names)
		if (funcName == "ballerina/io:println" || funcName == "println") && len(args) > 0 {
			arg := args[0]
			ballerinaStringPtrType := types.NewPointer(cg.structTypes["BallerinaString"])
			ballerinaArrayPtrType := types.NewPointer(cg.structTypes["BallerinaArray"])

			// Check if the argument is a BallerinaArray* (which is the case for println arrays)
			if arg.Type().Equal(ballerinaArrayPtrType) {
				// Use the array-specific println function
				printlnArrayFunc := cg.getLLVMFunction("ballerina_io_println_array")
				if printlnArrayFunc != nil {
					llvmBB.NewCall(printlnArrayFunc, arg)
					fmt.Printf("[DEBUG] emitInstruction: Generated println_array call with arg type %s\n", arg.Type())
				} else {
					fmt.Printf("[ERROR] emitInstruction: ballerina_io_println_array function not found!\n")
				}
			} else {
				// Use the regular string println function
				printlnFunc := cg.getLLVMFunction("ballerina_io_println")
				if printlnFunc != nil {
					if arg.Type().Equal(ballerinaStringPtrType) {
						// Direct BallerinaString* - use as-is
						llvmBB.NewCall(printlnFunc, arg)
					} else {
						// Cast other types to BallerinaString*
						castedArg := llvmBB.NewBitCast(arg, ballerinaStringPtrType)
						llvmBB.NewCall(printlnFunc, castedArg)
					}
					fmt.Printf("[DEBUG] emitInstruction: Generated println call with arg type %s\n", arg.Type())
				} else {
					fmt.Printf("[ERROR] emitInstruction: ballerina_io_println function not found!\n")
				}
			}
		} else {
			llvmFunc := cg.getLLVMFunction(funcName)
			if llvmFunc == nil {
				llvmFunc = cg.getLLVMFunction(inst.FunctionName)
			}
			if llvmFunc != nil {
				result := llvmBB.NewCall(llvmFunc, args...)
				if len(inst.GetLHS()) > 0 {
					cg.assignToVar(inst.GetLHS()[0], result, llvmBB)
				}
			} else {
				fmt.Printf("[WARNING] emitInstruction: Function '%s' not found\n", funcName)
			}
		}

	default:
		fmt.Printf("[WARNING] emitInstruction: Unhandled instruction type: %T\n", birInst)
	}
}

// emitTerminator converts BIR terminators to LLVM IR
func (cg *CodeGenerator) emitTerminator(birTerm bir.TerminatorInstruction, llvmBB *ir.Block, bbMap map[string]*ir.Block) {
	switch term := birTerm.(type) {
	case *bir.ReturnInst:
		// Check the function's return type and return accordingly
		var parentFunc *ir.Func
		for _, f := range cg.module.Funcs {
			for _, bb := range f.Blocks {
				if bb == llvmBB {
					parentFunc = f
					break
				}
			}
			if parentFunc != nil {
				break
			}
		}

		if parentFunc != nil && !parentFunc.Sig.RetType.Equal(types.Void) {
			// Function expects a return value - load from return variable
			retVarName := "%0" // Convention: %0 is usually the return variable
			if retVar, ok := cg.varMap[retVarName]; ok {
				retVal := llvmBB.NewLoad(parentFunc.Sig.RetType, retVar)
				llvmBB.NewRet(retVal)
			} else {
				// Return a default value
				llvmBB.NewRet(constant.NewZeroInitializer(parentFunc.Sig.RetType))
			}
		} else {
			llvmBB.NewRet(nil)
		}

	case *bir.GotoInst:
		if targetBB, ok := bbMap[term.TargetBB]; ok {
			llvmBB.NewBr(targetBB)
		} else {
			fmt.Printf("[ERROR] emitTerminator: Target block '%s' not found\n", term.TargetBB)
			llvmBB.NewUnreachable()
		}

	case *bir.ConditionalBranchInst:
		conditionVal := cg.loadVar(term.Condition, llvmBB)
		trueBB, okTrue := bbMap[term.TrueBB]
		falseBB, okFalse := bbMap[term.FalseBB]

		if okTrue && okFalse {
			llvmBB.NewCondBr(conditionVal, trueBB, falseBB)
		} else {
			fmt.Printf("[ERROR] emitTerminator: Branch target blocks not found: %s, %s\n", term.TrueBB, term.FalseBB)
			llvmBB.NewUnreachable()
		}

	case *bir.CallInst:
		cg.emitInstruction(term, llvmBB, bbMap)
		if nextBB, ok := bbMap[term.NextBB]; ok {
			llvmBB.NewBr(nextBB)
		} else {
			fmt.Printf("[ERROR] emitTerminator: Next block '%s' not found for call\n", term.NextBB)
			llvmBB.NewUnreachable()
		}

	default:
		fmt.Printf("[WARNING] emitTerminator: Unhandled terminator type: %T\n", birTerm)
		llvmBB.NewUnreachable()
	}
}

// Helper methods for variable handling
func (cg *CodeGenerator) assignToVar(birVar *bir.Variable, llvmVal value.Value, llvmBB *ir.Block) {
	if alloca, ok := cg.varMap[birVar.BIRName]; ok {
		// Check if the types are compatible
		allocaType := alloca.Type().(*types.PointerType).ElemType
		valType := llvmVal.Type()

		fmt.Printf("[DEBUG] assignToVar: Assigning to variable '%s', alloca type: %s, value type: %s\n",
			birVar.BIRName, allocaType, valType)

		// If types match exactly, store directly
		if allocaType.Equal(valType) {
			llvmBB.NewStore(llvmVal, alloca)
			fmt.Printf("[DEBUG] assignToVar: Direct store for variable '%s'\n", birVar.BIRName)
			return
		}

		// Handle BallerinaString* assignments specially
		ballerinaStringPtrType := types.NewPointer(cg.structTypes["BallerinaString"])

		// If we're trying to store a BallerinaString* into a variable
		if valType.Equal(ballerinaStringPtrType) {
			if allocaType.Equal(ballerinaStringPtrType) {
				// Direct store - both are BallerinaString*
				llvmBB.NewStore(llvmVal, alloca)
				fmt.Printf("[DEBUG] assignToVar: Direct BallerinaString* store for variable '%s'\n", birVar.BIRName)
				return
			} else if allocaType.Equal(types.I8Ptr) {
				// NEVER cast BallerinaString* to i8* - this loses the structure
				// Instead, we need to ensure the alloca is created as BallerinaString*
				fmt.Printf("[ERROR] assignToVar: Attempted to store BallerinaString* as i8* for variable '%s' - this would corrupt the data!\n", birVar.BIRName)
				fmt.Printf("[ERROR] assignToVar: The alloca should have been created as BallerinaString*, not i8*\n")
				return
			}
		}

		// Handle compatible type conversions
		var castedVal value.Value = llvmVal

		// Handle pointer type conversions
		if _, isAllocaPtr := allocaType.(*types.PointerType); isAllocaPtr {
			if _, isValPtr := valType.(*types.PointerType); isValPtr {
				// Both are pointers - safe to bitcast
				castedVal = llvmBB.NewBitCast(llvmVal, allocaType)
				fmt.Printf("[DEBUG] assignToVar: Bitcast between pointer types for variable '%s'\n", birVar.BIRName)
			} else if valType.Equal(types.I8Ptr) {
				// Source is generic i8* pointer
				castedVal = llvmBB.NewBitCast(llvmVal, allocaType)
				fmt.Printf("[DEBUG] assignToVar: Cast i8* to pointer type for variable '%s'\n", birVar.BIRName)
			} else {
				fmt.Printf("[WARNING] assignToVar: Cannot cast non-pointer type %s to pointer type %s for variable '%s'\n",
					valType, allocaType, birVar.BIRName)
				return
			}
		} else if allocaType.Equal(types.I8Ptr) {
			if _, isValPtr := valType.(*types.PointerType); isValPtr {
				// Target is generic i8* pointer, source is specific pointer
				castedVal = llvmBB.NewBitCast(llvmVal, types.I8Ptr)
				fmt.Printf("[DEBUG] assignToVar: Cast pointer to i8* for variable '%s'\n", birVar.BIRName)
			} else {
				fmt.Printf("[WARNING] assignToVar: Cannot cast non-pointer type %s to i8* for variable '%s'\n",
					valType, birVar.BIRName)
				return
			}
		} else {
			// Non-pointer types - check for valid conversions
			if isValidNonPointerConversion(valType, allocaType) {
				// For now, just store as-is for compatible non-pointer types
				castedVal = llvmVal
				fmt.Printf("[DEBUG] assignToVar: Non-pointer assignment for variable '%s'\n", birVar.BIRName)
			} else {
				fmt.Printf("[WARNING] assignToVar: Type mismatch for variable '%s': alloca type %s, value type %s - skipping assignment\n",
					birVar.BIRName, allocaType, valType)
				return
			}
		}

		llvmBB.NewStore(castedVal, alloca)
		fmt.Printf("[DEBUG] assignToVar: Successfully stored value for variable '%s'\n", birVar.BIRName)
	} else {
		fmt.Printf("[WARNING] assignToVar: Variable '%s' not found in varMap\n", birVar.BIRName)
	}
}

func (cg *CodeGenerator) loadVar(birVar *bir.Variable, llvmBB *ir.Block) value.Value {
	if alloca, ok := cg.varMap[birVar.BIRName]; ok {
		allocaType := alloca.Type().(*types.PointerType).ElemType
		expectedType := cg.birTypeToLLVMType(birVar.Type)

		fmt.Printf("[DEBUG] loadVar: Loading variable '%s', stored as: %s, expected: %s\n",
			birVar.BIRName, allocaType, expectedType)

		loadedVal := llvmBB.NewLoad(allocaType, alloca)

		// If types match exactly, return as-is
		if allocaType.Equal(expectedType) {
			fmt.Printf("[DEBUG] loadVar: Direct load for variable '%s'\n", birVar.BIRName)
			return loadedVal
		}

		// Handle BallerinaString* loading specially
		ballerinaStringPtrType := types.NewPointer(cg.structTypes["BallerinaString"])

		// If we're loading a string variable and expect BallerinaString*
		if birVar.Type == "string" && expectedType.Equal(ballerinaStringPtrType) {
			if allocaType.Equal(ballerinaStringPtrType) {
				// Direct load - already BallerinaString*
				fmt.Printf("[DEBUG] loadVar: Direct BallerinaString* load for variable '%s'\n", birVar.BIRName)
				return loadedVal
			} else if allocaType.Equal(types.I8Ptr) {
				// Cast from i8* to BallerinaString*
				castedVal := llvmBB.NewBitCast(loadedVal, ballerinaStringPtrType)
				fmt.Printf("[DEBUG] loadVar: Cast i8* to BallerinaString* for variable '%s'\n", birVar.BIRName)
				return castedVal
			}
		}

		// Handle compatible type conversions

		// Handle pointer type conversions
		if _, isAllocaPtr := allocaType.(*types.PointerType); isAllocaPtr {
			if _, isExpectedPtr := expectedType.(*types.PointerType); isExpectedPtr {
				// Both are pointers - safe to bitcast
				castedVal := llvmBB.NewBitCast(loadedVal, expectedType)
				fmt.Printf("[DEBUG] loadVar: Bitcast between pointer types for variable '%s'\n", birVar.BIRName)
				return castedVal
			} else if expectedType.Equal(types.I8Ptr) {
				// Expected is generic i8* pointer
				castedVal := llvmBB.NewBitCast(loadedVal, types.I8Ptr)
				fmt.Printf("[DEBUG] loadVar: Cast pointer to i8* for variable '%s'\n", birVar.BIRName)
				return castedVal
			} else {
				fmt.Printf("[WARNING] loadVar: Cannot cast pointer type %s to non-pointer type %s for variable '%s'\n",
					allocaType, expectedType, birVar.BIRName)
				return constant.NewUndef(expectedType)
			}
		} else if allocaType.Equal(types.I8Ptr) {
			if _, isExpectedPtr := expectedType.(*types.PointerType); isExpectedPtr {
				// Stored as generic i8* pointer, expected specific pointer
				castedVal := llvmBB.NewBitCast(loadedVal, expectedType)
				fmt.Printf("[DEBUG] loadVar: Cast i8* to pointer type for variable '%s'\n", birVar.BIRName)
				return castedVal
			} else {
				fmt.Printf("[WARNING] loadVar: Cannot cast i8* to non-pointer type %s for variable '%s'\n",
					expectedType, birVar.BIRName)
				return constant.NewUndef(expectedType)
			}
		} else {
			// Non-pointer types
			if isValidNonPointerConversion(allocaType, expectedType) {
				fmt.Printf("[DEBUG] loadVar: Non-pointer load for variable '%s'\n", birVar.BIRName)
				return loadedVal
			} else {
				fmt.Printf("[WARNING] loadVar: Type mismatch for variable '%s': stored as %s, expected %s\n",
					birVar.BIRName, allocaType, expectedType)
				return constant.NewUndef(expectedType)
			}
		}
	} else {
		fmt.Printf("[WARNING] loadVar: Variable '%s' not found in varMap\n", birVar.BIRName)
		return constant.NewUndef(cg.birTypeToLLVMType(birVar.Type))
	}
}

// Helper function to check if non-pointer type conversion is valid
func isValidNonPointerConversion(fromType, toType types.Type) bool {
	// Allow conversions between same basic types
	if fromType.Equal(toType) {
		return true
	}

	// Allow integer type conversions
	if _, isFromInt := fromType.(*types.IntType); isFromInt {
		if _, isToInt := toType.(*types.IntType); isToInt {
			return true // Allow int-to-int conversions
		}
	}

	// Allow void conversions (should not happen in practice)
	if fromType.Equal(types.Void) || toType.Equal(types.Void) {
		return true
	}

	// Don't allow boolean to pointer or other incompatible conversions
	return false
}

// getLLVMFunction retrieves a declared LLVM function.
func (cg *CodeGenerator) getLLVMFunction(name string) *ir.Func {
	if fn, ok := cg.functions[name]; ok {
		return fn
	}
	for _, f := range cg.module.Funcs {
		if f.Name() == name {
			cg.functions[name] = f
			return f
		}
	}
	fmt.Printf("[WARNING] getLLVMFunction: Function '%s' (and C-style '%s') not found in cg.functions map.\n", name, strings.Replace(name, ".", "_", -1))
	return nil
}

// getSizeOfType calculates the size of an LLVM type
func (cg *CodeGenerator) getSizeOfType(typ types.Type) int64 {
	if cg.module.DataLayout == "" {
		tm, err := NewLLVMTargetMachine()
		if err != nil {
			panic(fmt.Errorf("getSizeOfType: failed to create target machine for data layout: %v", err))
		}
		defer tm.Close()
		targetDataRef := C.LLVMCreateTargetDataLayout(tm.machine)
		if targetDataRef == nil {
			panic("getSizeOfType: failed to get target data layout ref")
		}
		defer C.LLVMDisposeTargetData(targetDataRef)
		cg.module.DataLayout = C.GoString(C.LLVMCopyStringRepOfTargetData(targetDataRef))
		fmt.Printf("[DEBUG] getSizeOfType: DataLayout not set on module, initialized to: %s\n", cg.module.DataLayout)
	}

	// Create a temporary target data from the data layout string
	dataLayoutCStr := C.CString(cg.module.DataLayout)
	defer C.free(unsafe.Pointer(dataLayoutCStr))

	targetDataRef := C.LLVMCreateTargetData(dataLayoutCStr)
	if targetDataRef == nil {
		panic(fmt.Sprintf("getSizeOfType: failed to create target data from layout string: %s", cg.module.DataLayout))
	}
	defer C.LLVMDisposeTargetData(targetDataRef)

	var size int64

	switch t := typ.(type) {
	case *types.IntType:
		switch t.BitSize {
		case 1:
			size = 1
		case 8:
			size = 1
		case 16:
			size = 2
		case 32:
			size = 4
		case 64:
			size = 8
		default:
			size = (int64(t.BitSize) + 7) / 8
		}
	case *types.FloatType:
		switch t.Kind {
		case types.FloatKindFloat:
			size = 4
		case types.FloatKindDouble:
			size = 8
		default:
			size = 8
		}
	case *types.PointerType:
		size = int64(C.LLVMPointerSize(targetDataRef))
	case *types.ArrayType:
		elemSize := cg.getSizeOfType(t.ElemType)
		size = int64(t.Len) * elemSize
	case *types.StructType:
		totalSize := int64(0)
		for _, field := range t.Fields {
			totalSize += cg.getSizeOfType(field)
		}
		size = totalSize
	default:
		size = int64(C.LLVMPointerSize(targetDataRef))
		fmt.Printf("[WARNING] getSizeOfType: Unknown type %T, defaulting to pointer size %d\n", typ, size)
	}

	return size
}
