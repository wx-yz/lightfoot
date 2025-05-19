package backend

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"wx-yz/lightfoot/bir"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// #cgo CFLAGS: -I/opt/homebrew/Cellar/llvm/20.1.5/include  -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
// #cgo LDFLAGS: -L/opt/homebrew/Cellar/llvm/20.1.5/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names -lLLVM-20

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

	// Add runtime support
	cg.AddRuntimeTypes()
	cg.AddRuntimeFunctions()
	cg.AddRuntimeGlobals()

	// Generate structure types first
	if err := cg.generateStructTypes(); err != nil {
		return fmt.Errorf("error generating struct types: %w", err)
	}

	// Generate globals
	if err := cg.generateGlobals(); err != nil {
		return fmt.Errorf("error generating globals: %w", err)
	}

	// Generate code for functions
	cg.generateAllFunctions()

	// Add runtime initialization
	cg.AddRuntimeInit()

	// Print the LLVM IR for debugging
	fmt.Println("Generated LLVM IR:")
	fmt.Println(cg.module.String())

	// Look for ballerina_main in the IR before object file generation
	if strings.Contains(cg.module.String(), "ballerina_main") {
		fmt.Println("[DEBUG] ballerina_main found in IR")
	} else {
		fmt.Println("[ERROR] ballerina_main NOT found in IR!")
	}

	// Create target machine
	tm, err := NewLLVMTargetMachine()
	if err != nil {
		return fmt.Errorf("failed to create target machine: %w", err)
	}
	defer tm.Close()

	// Convert LLIR to LLVM IR
	data := []byte(cg.module.String())
	tempFile := outputFile + ".ll"
	if err := os.WriteFile(tempFile, data, 0644); err != nil {
		return fmt.Errorf("failed to write LLVM IR to file: %w", err)
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
		for _, v := range fn.LocalVars {
			if strings.HasPrefix(v.Type, "record:") {
				recName := v.Type
				if _, ok := cg.structTypes[recName]; !ok {
					// For now, create a struct with two i64 fields
					// In a real implementation, you'd scan the record definition
					cg.structTypes[recName] = types.NewStruct(types.I64, types.I64)
				}
			}
		}
	}
	return nil
}

// generateGlobals generates LLVM IR for global variables
func (cg *CodeGenerator) generateGlobals() error {
	for _, g := range cg.birPackage.GlobalVars {
		// llvmType will be the type of the value the global variable holds (e.g., BallerinaMap*)
		llvmType := cg.birTypeToLLVMType(g.Type)

		// The global variable itself will be of this type.
		// NewGlobal creates a global of type `llvmType`.
		// The returned `glob` is a value.Value representing this global,
		// and its type when used as an address (e.g. in a store) is `llvmType*`.
		glob := cg.module.NewGlobal(g.Name, llvmType)

		// Initialize pointer types to null, other types to zero.
		if ptrType, isPtr := llvmType.(*types.PointerType); isPtr {
			glob.Init = constant.NewNull(ptrType)
		} else {
			glob.Init = constant.NewZeroInitializer(llvmType)
		}

		fmt.Printf("[DEBUG] generateGlobals: Defined global '%s' with LLVM type '%s' (value type). Initializer type: '%s'\n",
			g.Name, glob.Typ.LLString(), glob.Init.Type().LLString())

		cg.globals[g.Name] = glob // cg.globals stores *ir.Global
	}
	return nil
}

// generateAllFunctions generates LLVM IR for all functions in the BIR package
func (cg *CodeGenerator) generateAllFunctions() {
	// Generate all functions from BIR first to make them available
	for _, f := range cg.birPackage.Functions {
		llvmFunc := cg.generateFunction(f)
		cg.functions[f.Name] = llvmFunc
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
		// Found the Ballerina main function - call it
		entryBlock.NewCall(birMainFunc)
		fmt.Printf("[DEBUG] Added call to BIR main function in ballerina_main\n")
	} else {
		// No Ballerina main function found - emit an error message
		fmt.Printf("[WARNING] No 'main' function found in BIR, ballerina_main will be empty\n")

		// Create a call to print an error message instead of doing nothing
		// This ensures we have some visible indication if this function is called
		strPtr := cg.CreateGlobalString("Error: No main function was found in the Ballerina program")
		printlnFunc := cg.functions["ballerina_io_println"]
		if printlnFunc != nil {
			entryBlock.NewCall(printlnFunc, strPtr)
		}
	}

	// Return from ballerina_main
	entryBlock.NewRet(nil)

	// Add the function to our map with a unique name
	cg.functions["$ballerina_main_wrapper"] = mainFunc

	// Print debug information about the ballerina_main function
	fmt.Printf("[DEBUG] ballerina_main function: Linkage=%v, Visibility=%v, UnnamedAddr=%v\n",
		mainFunc.Linkage, mainFunc.Visibility, mainFunc.UnnamedAddr)
}

// generateFunction generates LLVM IR for a single function
func (cg *CodeGenerator) generateFunction(fn *bir.Function) *ir.Func {
	// Get the proper name for the function
	funcName := fn.Name

	// Prepare argument types
	paramTypes := []types.Type{}
	for _, param := range fn.Parameters {
		paramTypes = append(paramTypes, cg.birTypeToLLVMType(param.Type))
	}
	retType := cg.birTypeToLLVMType(fn.ReturnVariable.Type)
	irParams := make([]*ir.Param, len(paramTypes))
	for i, t := range paramTypes {
		irParams[i] = ir.NewParam(fmt.Sprintf("arg%d", i), t)
	}

	// Create the LLVM function with the original function name
	llvmFunc := cg.module.NewFunc(funcName, retType, irParams...)

	cg.functions[fn.Name] = llvmFunc // Index by original name in BIR

	// Create entry block
	entryBlock := llvmFunc.NewBlock("entry")

	// IMPORTANT: Create allocas for all variables at the start of the entry block
	// This ensures all variables have memory allocated before use
	for birName, birVar := range fn.LocalVars {
		// Skip parameters for now (handled separately)
		if birVar.IsFunctionArg {
			continue
		}

		// Get the LLVM type for this BIR variable
		llvmType := cg.birTypeToLLVMType(birVar.Type)

		// Create an alloca instruction in the entry block
		alloca := entryBlock.NewAlloca(llvmType)

		// Store the alloca in the variable map for later use
		cg.varMap[birName] = alloca

		// For debugging
		fmt.Printf("[DEBUG] generateFunction '%s': Allocating var BIRName='%s', OriginalName='%s', BIRType='%s', Kind='%s' with LLVM type '%s'\n",
			funcName, birVar.BIRName, birVar.OriginalName, birVar.Type, birVar.Kind, llvmType.LLString())

		// Initialize the variable with a zero value
		if birVar.Kind == bir.VarKindReturn {
			zeroInit := constant.NewZeroInitializer(llvmType)
			entryBlock.NewStore(zeroInit, alloca)
			fmt.Printf("[DEBUG] generateFunction '%s': Storing zero initializer '%s' (type '%s') into return var alloca '%s' for BIR var '%s'\n",
				funcName, zeroInit.String(), zeroInit.Type().LLString(), alloca.String(), birVar.BIRName)
		}
	}

	// Add special handling for the return variable if it's not in LocalVars
	if fn.ReturnVariable != nil && fn.ReturnVariable.BIRName != "" {
		// Check if we already created an alloca for the return variable
		if _, exists := cg.varMap[fn.ReturnVariable.BIRName]; !exists {
			// Get the LLVM type for the return variable
			retVarLLVMType := cg.birTypeToLLVMType(fn.ReturnVariable.Type)

			// Create an alloca for the return variable
			retVarAlloca := entryBlock.NewAlloca(retVarLLVMType)

			// Store the alloca in the variable map
			cg.varMap[fn.ReturnVariable.BIRName] = retVarAlloca

			// Initialize with zero value
			zeroInit := constant.NewZeroInitializer(retVarLLVMType)
			entryBlock.NewStore(zeroInit, retVarAlloca)

			fmt.Printf("[DEBUG] generateFunction '%s': Allocating RETURN var BIRName='%s', BIRType='%s', Kind='RETURN' with LLVM type '%s'\n",
				funcName, fn.ReturnVariable.BIRName, fn.ReturnVariable.Type, retVarLLVMType.LLString())

			fmt.Printf("[DEBUG] generateFunction '%s': Storing zero initializer '%s' (type '%s') into return var alloca '%s' for BIR var '%s'\n",
				funcName, zeroInit.String(), zeroInit.Type().LLString(), retVarAlloca.String(), fn.ReturnVariable.BIRName)
		}
	}

	// Map BIR basic blocks to LLVM basic blocks
	blockMap := make(map[string]*ir.Block)
	blockMap[fn.BasicBlocks[0].ID] = entryBlock // Map the first BIR block to the entry block

	// Create blocks for all other BIR basic blocks
	for i := 1; i < len(fn.BasicBlocks); i++ {
		birBB := fn.BasicBlocks[i]
		blockMap[birBB.ID] = llvmFunc.NewBlock(birBB.ID)
	}

	// Process function body
	fmt.Printf("[DEBUG] generateFunction: Processing function '%s'\n", funcName)

	// Process each BIR basic block
	for _, birBB := range fn.BasicBlocks {
		llvmBB := blockMap[birBB.ID]

		// Emit instructions for this basic block
		for _, inst := range birBB.Instructions {
			cg.emitInstruction(inst, llvmBB)
		}

		// Emit terminator for this basic block
		if birBB.Terminator != nil {
			cg.emitTerminator(birBB.Terminator, llvmBB, blockMap, fn)
		} else {
			// If no terminator is present, add a default return
			// This ensures every block has a terminator
			llvmFuncForDefaultRet, ok := cg.functions[fn.Name] // Get the LLVM function from the map
			if !ok || llvmFuncForDefaultRet == nil {
				panic(fmt.Sprintf("generateFunction default ret: LLVM function %s not found in cg.functions", fn.Name))
			}

			if llvmFuncForDefaultRet.Sig.RetType.Equal(types.Void) {
				llvmBB.NewRet(nil) // LLVM void return
			} else {
				if fn.ReturnVariable == nil || fn.ReturnVariable.BIRName == "" {
					panic(fmt.Sprintf("generateFunction default ret: LLVM func %s returns %s but BIR func has no ReturnVariable", fn.Name, llvmFuncForDefaultRet.Sig.RetType.LLString()))
				}
				retVal := cg.getVarValue(fn.ReturnVariable, llvmBB)
				if !retVal.Type().Equal(llvmFuncForDefaultRet.Sig.RetType) {
					fmt.Printf("[WARNING] generateFunction default ret: Type mismatch for function %s. Expected LLVM RetType: %s, Actual retVal type: %s. Value: %s. Attempting bitcast.\n",
						fn.Name, llvmFuncForDefaultRet.Sig.RetType.LLString(), retVal.Type().LLString(), retVal.Ident())
					// Attempt a bitcast if types are pointer-compatible or otherwise logically equivalent
					// This is a fallback and might indicate a deeper type system inconsistency.
					if types.IsPointer(retVal.Type()) && types.IsPointer(llvmFuncForDefaultRet.Sig.RetType) {
						retVal = llvmBB.NewBitCast(retVal, llvmFuncForDefaultRet.Sig.RetType)
					} else {
						// If not pointer-compatible, this is a more serious error.
						panic(fmt.Sprintf("generateFunction default ret: Cannot reconcile return type %s with expected %s for function %s", retVal.Type().LLString(), llvmFuncForDefaultRet.Sig.RetType.LLString(), fn.Name))
					}
				}
				llvmBB.NewRet(retVal)
			}
			fmt.Printf("[WARNING] Added default return for block %s in function %s because BIR terminator was nil\n", birBB.ID, funcName)
		}
	}

	return llvmFunc
}

// emitActualCall generates the LLVM IR for a function call.
// This is a helper called by both emitInstruction and emitTerminator for CallInst.
func (cg *CodeGenerator) emitActualCall(i *bir.CallInst, block *ir.Block) value.Value {
	args := []value.Value{}
	for _, arg := range i.Args {
		argVal := cg.getVarValue(arg, block)
		args = append(args, argVal)
	}

	callee := cg.getLLVMFunction(i.FunctionName)
	if callee == nil {
		panic(fmt.Sprintf("emitActualCall: LLVM function not found for BIR function '%s'", i.FunctionName))
	}

	var callInst value.Value

	// Special handling for println
	if i.FunctionName == "println" || i.FunctionName == "ballerina.io.println" {
		fmt.Printf("[DEBUG] emitActualCall: Special handling for println with %d args\n", len(args))
		if len(args) > 0 {
			// Argument for println is expected to be BallerinaArray* containing the actual value(s)
			// The actual value to print is the first element of this array.
			expectedArrayPtrType := types.NewPointer(cg.structTypes["BallerinaArray"])
			actualArgType := args[0].Type()

			if !actualArgType.Equal(expectedArrayPtrType) {
				// Fallback check for structural equality if direct equality fails
				// This might happen if types are equivalent but not the same instance.
				isEquivalent := false
				if ptrType, ok := actualArgType.(*types.PointerType); ok {
					if structType, okElem := ptrType.ElemType.(*types.StructType); okElem {
						if structType.Equal(cg.structTypes["BallerinaArray"]) {
							isEquivalent = true
						}
					}
				}
				if !isEquivalent {
					panic(fmt.Sprintf("println argument is not a BallerinaArray* (expected LLVM type %s), got LLVM type %s",
						expectedArrayPtrType.LLString(), actualArgType.LLString()))
				}
				fmt.Printf("[WARNING] emitActualCall for println: Actual arg type %s is structurally equivalent to BallerinaArray* %s but not strictly Equal(). Proceeding.\n",
					actualArgType.LLString(), expectedArrayPtrType.LLString())
			}
			arrayValue := args[0] // This is the BallerinaArray*

			arrayStructType := cg.structTypes["BallerinaArray"]

			// Get pointer to the data field of the array struct
			arrayDataFieldPtr := block.NewGetElementPtr(arrayStructType, arrayValue,
				constant.NewInt(types.I32, 0), // Struct index
				constant.NewInt(types.I32, 1)) // Data field (i8*)

			// Load the data buffer pointer (e.g., i8* pointing to the actual array elements)
			arrayDataBuffer := block.NewLoad(types.I8Ptr, arrayDataFieldPtr)

			// The buffer contains i8* elements. We want the first one (index 0).
			firstElemAddr := block.NewGetElementPtr(types.I8Ptr, arrayDataBuffer, constant.NewInt(types.I64, 0))
			stringPtrAsI8Star := block.NewLoad(types.I8Ptr, firstElemAddr)

			// Cast this i8* back to BallerinaString*
			stringPtrType := types.NewPointer(cg.structTypes["BallerinaString"])
			stringPtr := block.NewBitCast(stringPtrAsI8Star, stringPtrType)

			fmt.Printf("[DEBUG] emitActualCall: Passing extracted string %s of type %s to println\n", stringPtr.Ident(), stringPtr.Type().LLString())
			callInst = block.NewCall(callee, stringPtr)
		} else {
			fmt.Printf("[WARNING] emitActualCall: println called with no arguments. Calling with null BallerinaString*.\n")
			nullStringPtr := constant.NewNull(types.NewPointer(cg.structTypes["BallerinaString"]))
			callInst = block.NewCall(callee, nullStringPtr)
		}
	} else {
		// Normal call handling for other functions
		callInst = block.NewCall(callee, args...)
	}

	// Assign to LHS if it exists
	if len(i.GetLHS()) > 0 && i.GetLHS()[0] != nil {
		// Ensure LHS is assigned only if the function doesn't return void
		// or if the callInst itself is not void
		if returnVal, ok := callInst.(value.Value); ok && !returnVal.Type().Equal(types.Void) {
			cg.assignToVar(i.GetLHS()[0], returnVal, block)
		} else if !callee.Sig.RetType.Equal(types.Void) {
			// This case might be redundant if callInst always reflects the callee's return type correctly.
			cg.assignToVar(i.GetLHS()[0], callInst, block)
		} else if lhsVar := i.GetLHS()[0]; lhsVar.Type != "()" {
			// If callee is void but LHS expects a value (and BIR type is not '()'), this is a mismatch.
			fmt.Printf("[WARNING] emitActualCall: Assigning to LHS %s (BIRType: %s) from void function call %s.\n", lhsVar.BIRName, lhsVar.Type, i.FunctionName)
		}
	}
	return callInst // Return the call instruction itself (e.g., *ir.InstCall)
}

// emitInstruction generates LLVM IR for a single instruction
func (cg *CodeGenerator) emitInstruction(inst bir.Instruction, block *ir.Block) {
	switch i := inst.(type) {
	case *bir.ConstantLoadInst:
		var llvmVal value.Value
		if i.TypeName == "string" {
			// For string literals, call a runtime function to create BallerinaString struct
			strVal, ok := i.Value.(string)
			if !ok {
				panic(fmt.Sprintf("ConstantLoadInst: expected string value for type string, got %T", i.Value))
			}
			llvmVal = cg.CreateString(strVal, block) // Returns BallerinaString*
		} else {
			// For other constants, directly create the LLVM constant value
			llvmVal = cg.emitConstant(i)
		}
		cg.assignToVar(i.GetLHS()[0], llvmVal, block)
		return

	case *bir.MoveInst:
		rhsVal := cg.getVarValue(i.RHS, block)
		cg.assignToVar(i.GetLHS()[0], rhsVal, block)
		return

	case *bir.BinaryOpInst:
		op1 := cg.getVarValue(i.Op1, block)
		op2 := cg.getVarValue(i.Op2, block)
		var result value.Value
		switch i.Op {
		case "+":
			result = block.NewAdd(op1, op2)
		case "-":
			result = block.NewSub(op1, op2)
		case "*":
			result = block.NewMul(op1, op2)
		case "/":
			result = block.NewSDiv(op1, op2)
		default:
			panic(fmt.Sprintf("unsupported binary operator: %s", i.Op))
		}
		cg.assignToVar(i.GetLHS()[0], result, block)
		return

	case *bir.CallInst:
		// This path handles CallInst when it appears in the middle of a block (not as a terminator).
		if i.IsTerminator() {
			// This should ideally not happen if BIR is structured correctly.
			// Terminators are handled by emitTerminator.
			fmt.Printf("[WARNING] emitInstruction: Encountered a CallInst that IsTerminator() in the middle of a block: %s. This is unusual and might be handled by emitTerminator instead.\n", i.String())
			// Depending on strictness, one might panic here or proceed.
			// If we proceed, it means this call might be emitted twice if also a terminator.
			// For now, let's assume BIR is correct and this is for non-terminators.
		}
		cg.emitActualCall(i, block)
		return

	case *bir.TypeCastInst:
		src := cg.getVarValue(i.SourceVar, block)
		targetLLVMType := cg.birTypeToLLVMType(i.TargetType)

		if i.TargetType == "ballerina/io:1.8.0:Printable" {
			if src.Type().Equal(types.NewPointer(cg.structTypes["BallerinaString"])) {
				cg.assignToVar(i.GetLHS()[0], src, block)
				return
			}
			cast := block.NewBitCast(src, types.NewPointer(cg.structTypes["BallerinaString"]))
			cg.assignToVar(i.GetLHS()[0], cast, block)
			return
		}

		var cast value.Value
		if types.IsPointer(src.Type()) && types.IsPointer(targetLLVMType) {
			cast = block.NewBitCast(src, targetLLVMType)
		} else if types.IsInt(src.Type()) && types.IsPointer(targetLLVMType) {
			cast = block.NewIntToPtr(src, targetLLVMType)
		} else if types.IsPointer(src.Type()) && types.IsInt(targetLLVMType) {
			cast = block.NewPtrToInt(src, targetLLVMType)
		} else if types.IsInt(src.Type()) && types.IsInt(targetLLVMType) {
			srcIntType := src.Type().(*types.IntType)
			tgtIntType := targetLLVMType.(*types.IntType)
			if srcIntType.BitSize < tgtIntType.BitSize {
				cast = block.NewSExt(src, targetLLVMType)
			} else if srcIntType.BitSize > tgtIntType.BitSize {
				cast = block.NewTrunc(src, targetLLVMType)
			} else {
				cast = src
			}
		} else {
			fmt.Printf("[WARNING] TypeCastInst: Performing potentially unsafe BitCast from %s to %s\n", src.Type().LLString(), targetLLVMType.LLString())
			cast = block.NewBitCast(src, targetLLVMType)
		}
		cg.assignToVar(i.GetLHS()[0], cast, block)
		return

	case *bir.NewArrayInst:
		var sizeVal int64
		if i.SizeVar != nil {
			sizeOperand := cg.getVarValue(i.SizeVar, block)
			if constSize, ok := sizeOperand.(*constant.Int); ok {
				sizeVal = constSize.X.Int64()
			} else {
				fmt.Printf("[WARNING] NewArrayInst: SizeVar %s is not a constant. Defaulting based on Args count if SizeVar is -1.\n", i.SizeVar.BIRName)
				sizeVal = -1
			}
		} else {
			sizeVal = int64(len(i.Args))
		}

		if sizeVal == -1 && len(i.Args) > 0 {
			fmt.Printf("[DEBUG] NewArrayInst: SizeVar resulted in -1, but Args are present. Using len(Args)=%d as size.\n", len(i.Args))
			sizeVal = int64(len(i.Args))
		} else if sizeVal < 0 {
			fmt.Printf("[ERROR] NewArrayInst: Invalid array size %d. Defaulting to 0 for safety.\n", sizeVal)
			sizeVal = 0
		}

		llvmElementType := types.I8Ptr
		array := cg.CreateArray(sizeVal, llvmElementType, block)

		if len(i.Args) > 0 && sizeVal > 0 {
			arrayStructType := cg.structTypes["BallerinaArray"]
			arrayDataFieldPtr := block.NewGetElementPtr(arrayStructType, array,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1))

			arrayDataBuffer := block.NewLoad(types.I8Ptr, arrayDataFieldPtr)

			for idx, argVar := range i.Args {
				if int64(idx) >= sizeVal {
					fmt.Printf("[ERROR] NewArrayInst: Attempting to write past allocated array bounds. Index: %d, Size: %d for arg %s\n", idx, sizeVal, argVar.BIRName)
					break
				}
				elemValue := cg.getVarValue(argVar, block)
				fmt.Printf("[DEBUG] NewArrayInst: Element value for arg %s at index %d is: %s of type %s\n",
					argVar.BIRName, idx, elemValue.Ident(), elemValue.Type().LLString())

				elemAsI8Ptr := block.NewBitCast(elemValue, types.I8Ptr)

				ptrToElemSlot := block.NewGetElementPtr(llvmElementType, arrayDataBuffer, constant.NewInt(types.I64, int64(idx)))

				fmt.Printf("[DEBUG] NewArrayInst: Storing %s (type %s) at index %d into slot %s (type %s)\n",
					elemAsI8Ptr.Ident(), elemAsI8Ptr.Type().LLString(),
					idx, ptrToElemSlot.Ident(), ptrToElemSlot.Type().LLString())

				block.NewStore(elemAsI8Ptr, ptrToElemSlot)
			}
		}
		cg.assignToVar(i.GetLHS()[0], array, block)
		fmt.Printf("[DEBUG] NewArrayInst: Created array %s with %d slots, assigned to %s\n", array.Ident(), sizeVal, i.GetLHS()[0].BIRName)
		return

	case *bir.NewTypeInst:
		fmt.Printf("[WARNING] NewTypeInst: Not fully implemented. Desc: %s\n", i.Desc)
		if len(i.GetLHS()) > 0 && i.GetLHS()[0] != nil {
			lhsVar := i.GetLHS()[0]
			llvmType := cg.birTypeToLLVMType(lhsVar.Type)
			var placeholder value.Value
			if ptrType, ok := llvmType.(*types.PointerType); ok {
				placeholder = constant.NewNull(ptrType)
			} else {
				placeholder = constant.NewZeroInitializer(llvmType)
			}
			cg.assignToVar(lhsVar, placeholder, block)
		}
		return

	case *bir.NewMapInst:
		fmt.Printf("[DEBUG] emitInstruction NewMapInst: LHS Var BIRName='%s', BIRType='%s'\n", i.GetLHS()[0].BIRName, i.GetLHS()[0].Type)
		mapVal := cg.CreateMap(block)
		cg.assignToVar(i.GetLHS()[0], mapVal, block)
		return

	default:
		panic(fmt.Sprintf("unsupported BIR instruction: %T", inst))
	}
}

// emitTerminator generates LLVM IR for a BIR terminator instruction.
func (cg *CodeGenerator) emitTerminator(term bir.TerminatorInstruction, currentBlock *ir.Block, blockMap map[string]*ir.Block, birFunc *bir.Function) {
	fmt.Printf("[DEBUG] emitTerminator: Processing terminator %T in block %s for function %s\n", term, currentBlock.LocalIdent.Ident(), birFunc.Name)

	llvmFunc, ok := cg.functions[birFunc.Name] // Get the LLVM function from the map
	if !ok || llvmFunc == nil {
		panic(fmt.Sprintf("emitTerminator: LLVM function %s not found in cg.functions", birFunc.Name))
	}

	switch t := term.(type) {
	case *bir.ReturnInst:
		if llvmFunc.Sig.RetType.Equal(types.Void) {
			currentBlock.NewRet(nil) // LLVM void return
		} else {
			// LLVM function returns a non-void type
			if birFunc.ReturnVariable == nil || birFunc.ReturnVariable.BIRName == "" {
				panic(fmt.Sprintf("emitTerminator: LLVM func %s returns %s but BIR func has no ReturnVariable", birFunc.Name, llvmFunc.Sig.RetType.LLString()))
			}
			retVal := cg.getVarValue(birFunc.ReturnVariable, currentBlock)
			if !retVal.Type().Equal(llvmFunc.Sig.RetType) {
				fmt.Printf("[WARNING] emitTerminator: ReturnInst for %s. LLVM RetType: %s, Actual retVal type: %s. Value: %s. Attempting bitcast.\n",
					birFunc.Name, llvmFunc.Sig.RetType.LLString(), retVal.Type().LLString(), retVal.Ident())
				// Attempt a bitcast if types are pointer-compatible or otherwise logically equivalent
				if types.IsPointer(retVal.Type()) && types.IsPointer(llvmFunc.Sig.RetType) {
					retVal = currentBlock.NewBitCast(retVal, llvmFunc.Sig.RetType)
				} else {
					panic(fmt.Sprintf("emitTerminator: ReturnInst: Cannot reconcile return type %s with expected %s for function %s", retVal.Type().LLString(), llvmFunc.Sig.RetType.LLString(), birFunc.Name))
				}
			}
			currentBlock.NewRet(retVal)
		}
	case *bir.GotoInst:
		targetBB, ok := blockMap[t.TargetBB]
		if !ok {
			panic(fmt.Sprintf("emitTerminator: target basic block %s not found for GOTO in function %s", t.TargetBB, birFunc.Name))
		}
		currentBlock.NewBr(targetBB)
	case *bir.ConditionalBranchInst:
		condVal := cg.getVarValue(t.Condition, currentBlock)
		// Ensure condVal is i1. If not, it needs to be compared (e.g., != 0 for integers)
		if !condVal.Type().Equal(types.I1) {
			// Assuming boolean is represented as i8, compare with 0
			if intType, ok := condVal.Type().(*types.IntType); ok && intType.BitSize > 1 {
				condVal = currentBlock.NewICmp(enum.IPredNE, condVal, constant.NewInt(intType, 0))
			} else {
				// If it's not an integer type we can easily compare to zero, this might be an issue.
				// For now, we'll proceed, but LLVM will error if condVal is not i1.
				fmt.Printf("[WARNING] emitTerminator: ConditionalBranch condition in %s for func %s is type %s, expected i1. Attempting to use directly.\n", currentBlock.LocalIdent.Ident(), birFunc.Name, condVal.Type().LLString())
			}
		}
		trueBB, okTrue := blockMap[t.TrueBB]
		falseBB, okFalse := blockMap[t.FalseBB]
		if !okTrue {
			panic(fmt.Sprintf("emitTerminator: target basic block TrueBB %s not found for ConditionalBranch in function %s", t.TrueBB, birFunc.Name))
		}
		if !okFalse {
			panic(fmt.Sprintf("emitTerminator: target basic block FalseBB %s not found for ConditionalBranch in function %s", t.FalseBB, birFunc.Name))
		}
		currentBlock.NewCondBr(condVal, trueBB, falseBB)
	case *bir.CallInst: // If a CallInst can be a terminator
		if t.IsTerminator() {
			cg.emitActualCall(t, currentBlock) // Generate the actual call

			// After the call, branch to the NextBB
			// A CallInst terminator must define where control flows next.
			if t.NextBB == "" {
				// This is problematic for a CallInst that IS a terminator but doesn't specify NextBB
				// unless it's a known 'noreturn' call (like panic).
				// If it's the last call in a function, the block should still be terminated by a 'ret'.
				// This situation suggests the BIR might be incomplete or this block needs a subsequent 'ret'.
				// For now, we rely on the outer loop in `generateFunction` to add a default terminator if this block isn't properly closed.
				fmt.Printf("[WARNING] emitTerminator: CallInst in %s for func %s is a terminator but has no NextBB. Block termination relies on subsequent logic or default terminator.\n", currentBlock.LocalIdent.Ident(), birFunc.Name)
				// If the block is still not terminated (e.g. call was not noreturn),
				// and this is the *only* terminator, it's an issue.
				// However, if `currentBlock.Term` is still nil here, the default return in `generateFunction` will kick in.
			} else {
				targetBB, ok := blockMap[t.NextBB]
				if !ok {
					panic(fmt.Sprintf("emitTerminator: target basic block %s not found for CallInst terminator in function %s", t.NextBB, birFunc.Name))
				}
				// Check if the block isn't already terminated (e.g., by a noreturn call like panic from emitActualCall)
				if currentBlock.Term == nil {
					currentBlock.NewBr(targetBB)
				}
			}
		} else {
			// This means a CallInst was passed as a terminator, but its IsTerminator() is false.
			// This is a BIR structural error.
			panic(fmt.Sprintf("emitTerminator: CallInst in %s for func %s was processed as terminator but IsTerminator() is false. Block will likely be ill-formed.", currentBlock.LocalIdent.Ident(), birFunc.Name))
		}
	default:
		// Ensure the block is terminated if we fall through here without a specific case
		if currentBlock.Term == nil {
			panic(fmt.Sprintf("emitTerminator: unhandled BIR terminator type %T in block %s for function %s, and block was not otherwise terminated.", term, currentBlock.LocalIdent.Ident(), birFunc.Name))
		}
	}
}

// getLLVMFunction retrieves the *ir.Func for a given BIR function name,
// handling mapping for known runtime functions.
func (cg *CodeGenerator) getLLVMFunction(birFuncName string) *ir.Func {
	var llvmMappedName string

	// Map BIR function names to C-style names used in runtime.c
	switch birFuncName {
	case "println", "ballerina.io.println":
		llvmMappedName = "ballerina_io_println"
	case "ballerina.lang.array.new":
		llvmMappedName = "ballerina_lang_array_new"
	case "ballerina.lang.string.concat":
		llvmMappedName = "ballerina_lang_string_concat"
	case "ballerina.lang.map.new":
		llvmMappedName = "ballerina_lang_map_new"
	case "ballerina_string_new_with_literal": // Already C-style
		llvmMappedName = birFuncName
	default:
		llvmMappedName = birFuncName
	}

	fn, ok := cg.functions[llvmMappedName]
	if !ok {
		fn, ok = cg.functions[birFuncName] // Fallback to original name
		if !ok {
			panic(fmt.Sprintf("getLLVMFunction: LLVM function not found for BIR function '%s' (tried mapping to '%s')", birFuncName, llvmMappedName))
		}
	}
	fmt.Printf("[DEBUG] getLLVMFunction: Mapped BIR func '%s' to LLVM func '%s' (Ident: %s)\n", birFuncName, llvmMappedName, fn.Ident())
	return fn
}

// CreateGlobalString creates a global string constant
func (cg *CodeGenerator) CreateGlobalString(s string) value.Value {
	name := fmt.Sprintf(".str.%d", len(cg.module.Globals))
	chars := []constant.Constant{}
	for i := 0; i < len(s); i++ {
		chars = append(chars, constant.NewInt(types.I8, int64(s[i])))
	}
	chars = append(chars, constant.NewInt(types.I8, 0))
	arrayType := types.NewArray(uint64(len(s)+1), types.I8)
	init := constant.NewArray(arrayType, chars...)
	global := cg.module.NewGlobalDef(name, init)
	zero := constant.NewInt(types.I32, 0)
	indices := []constant.Constant{zero, zero}
	return constant.NewGetElementPtr(arrayType, global, indices...)
}

// assignToVar assigns a value to a variable in the LLVM IR
func (cg *CodeGenerator) assignToVar(v *bir.Variable, val value.Value, block *ir.Block) {
	fmt.Printf("[DEBUG] assignToVar: START for variable Name='%s', BIRName='%s', BIRType='%s', Kind='%s'\n", v.Name, v.BIRName, v.Type, v.Kind)

	// Check if it's a global variable
	if globDef, isGlobal := cg.globals[v.BIRName]; isGlobal {
		fmt.Printf("[DEBUG] assignToVar: Assigning to GLOBAL variable '%s'. GlobalDef Type: %s, Value to store Type: %s\n",
			v.BIRName, globDef.Typ.LLString(), val.Type().LLString())

		if !val.Type().Equal(globDef.Typ) {
			errorMsg := fmt.Sprintf("assignToVar: PANICKING! Type mismatch for GLOBAL. Cannot store value of LLVM type '%s' into global variable '%s' which has LLVM type '%s'.",
				val.Type().LLString(), v.BIRName, globDef.Typ.LLString())
			fmt.Printf("[DEBUG] %s\n", errorMsg)
			panic(errorMsg)
		}

		block.NewStore(val, globDef)
		fmt.Printf("[DEBUG] assignToVar: END for global variable '%s'\n", v.BIRName)
		return
	}

	// If not global, assume it's a local variable (alloca)
	alloca, ok := cg.varMap[v.BIRName]
	if !ok {
		panicMsg := fmt.Sprintf("assignToVar: missing alloca for local variable %s (BIRName: %s)", v.Name, v.BIRName)
		fmt.Printf("[DEBUG] assignToVar: %s\n", panicMsg)
		panic(panicMsg)
	}

	fmt.Printf("[DEBUG] assignToVar: Alloca for '%s' is: %s\n", v.BIRName, alloca.String())
	fmt.Printf("[DEBUG] assignToVar: Alloca's LLVM type for '%s' is: %s\n", v.BIRName, alloca.Type().LLString())

	allocaElemType := alloca.Type().(*types.PointerType).ElemType
	fmt.Printf("[DEBUG] assignToVar: Alloca Elem Type (variable's expected LLVM type) for '%s' (BIRType: '%s') is: %s\n", v.BIRName, v.Type, allocaElemType.LLString())

	valType := val.Type()
	fmt.Printf("[DEBUG] assignToVar: Value to store is: %s\n", val.String())
	fmt.Printf("[DEBUG] assignToVar: Value's LLVM type is: %s\n", valType.LLString())

	if !valType.Equal(allocaElemType) {
		errorMsg := fmt.Sprintf("assignToVar: PANICKING! Type mismatch for LOCAL. Cannot store value of LLVM type '%s' into variable '%s' (BIR type: '%s') which has LLVM type '%s'. Value: '%s', Alloca: '%s'",
			valType.LLString(), v.BIRName, v.Type, allocaElemType.LLString(), val.String(), alloca.String())
		fmt.Printf("[DEBUG] %s\n", errorMsg)
		panic(errorMsg)
	}

	fmt.Printf("[DEBUG] assignToVar: Types match for LOCAL. Storing value into alloca for '%s'.\n", v.BIRName)
	block.NewStore(val, alloca)
	fmt.Printf("[DEBUG] assignToVar: END for local variable '%s'\n", v.BIRName)
}

// getSizeOfType returns the size of a type in bytes
func (cg *CodeGenerator) getSizeOfType(t types.Type) int64 {
	switch t := t.(type) {
	case *types.IntType:
		return int64(t.BitSize) / 8
	case *types.FloatType:
		if t == types.Float {
			return 4
		}
		return 8 // double
	case *types.StructType:
		// Rough estimate, this should be improved for proper struct layout
		var size int64
		for _, field := range t.Fields {
			size += cg.getSizeOfType(field)
		}
		return size
	case *types.ArrayType:
		return int64(t.Len) * cg.getSizeOfType(t.ElemType)
	case *types.PointerType:
		return 8 // Pointer size, typically 8 bytes on 64-bit systems
	default:
		return 8 // Default to 8 bytes
	}
}

// getVarValue retrieves a value from a variable in the LLVM IR
func (cg *CodeGenerator) getVarValue(v *bir.Variable, block *ir.Block) value.Value {
	fmt.Printf("[DEBUG] getVarValue: START for variable Name='%s', BIRName='%s', BIRType='%s', Kind='%s'\n", v.Name, v.BIRName, v.Type, v.Kind)

	llvmVal, ok := cg.varMap[v.BIRName]
	if !ok {
		if globDef, isGlobal := cg.globals[v.BIRName]; isGlobal {
			loadedGlobal := block.NewLoad(globDef.Typ, globDef)
			fmt.Printf("[DEBUG] getVarValue: Variable '%s' is GLOBAL. Loading from global. Global: %s, Loaded: %s, LLVMType: %s\n",
				v.BIRName, globDef.Ident(), loadedGlobal.Ident(), loadedGlobal.Type().LLString())
			return loadedGlobal
		}
		panic(fmt.Sprintf("getVarValue: LLVM value not found in varMap or globals for BIR variable '%s' (Name: '%s')", v.BIRName, v.Name))
	}

	if anAlloca, isAlloca := llvmVal.(*ir.InstAlloca); isAlloca {
		elemType := anAlloca.ElemType
		loadedVal := block.NewLoad(elemType, anAlloca)
		fmt.Printf("[DEBUG] getVarValue: Variable '%s' is LOCAL (alloca). Loading from alloca. Alloca: %s, Loaded: %s, LLVMType: %s\n",
			v.BIRName, anAlloca.Ident(), loadedVal.Ident(), loadedVal.Type().LLString())
		return loadedVal
	}

	fmt.Printf("[DEBUG] getVarValue: Variable '%s' is PARAMETER or other direct value. Using directly. Value: %s, LLVMType: %s\n",
		v.BIRName, llvmVal.Ident(), llvmVal.Type().LLString())
	return llvmVal
}

// emitConstant generates LLVM IR for a constant
func (cg *CodeGenerator) emitConstant(inst *bir.ConstantLoadInst) value.Value {
	typ := cg.birTypeToLLVMType(inst.TypeName)
	switch v := inst.Value.(type) {
	case int:
		return constant.NewInt(typ.(*types.IntType), int64(v))
	case int64:
		return constant.NewInt(typ.(*types.IntType), v)
	case float64:
		return constant.NewFloat(typ.(*types.FloatType), v)
	case string:
		if v == "nil" {
			if ptrType, ok := typ.(*types.PointerType); ok {
				return constant.NewNull(ptrType)
			}
			panic(fmt.Sprintf("emitConstant: 'nil' assignment to non-pointer LLVM type %s for BIR type %s", typ.LLString(), inst.TypeName))
		}
		panic(fmt.Sprintf("emitConstant for non-nil string literals ('%s') is not supported as it requires a runtime call. Use ConstantLoadInst in emitInstruction.", v))
	case bool:
		if v {
			return constant.NewInt(typ.(*types.IntType), 1)
		}
		return constant.NewInt(typ.(*types.IntType), 0)
	default:
		panic(fmt.Sprintf("unsupported constant type in emitConstant: %T", v))
	}
}

// getOrCreateMemcpyIntrinsic ensures the memcpy intrinsic is defined in the module
func (cg *CodeGenerator) getOrCreateMemcpyIntrinsic() *ir.Func {
	memcpyName := "llvm.memcpy.p0i8.p0i8.i64"
	if memcpyFunc, ok := cg.functions[memcpyName]; ok {
		return memcpyFunc
	}

	memcpyFunc := cg.module.NewFunc(
		memcpyName,
		types.Void,
		ir.NewParam("dest", types.I8Ptr),
		ir.NewParam("src", types.I8Ptr),
		ir.NewParam("size", types.I64),
		ir.NewParam("isvolatile", types.I1),
		ir.NewParam("align", types.I32),
	)
	cg.functions[memcpyName] = memcpyFunc

	return memcpyFunc
}

// birTypeToLLVMType converts a BIR type to an LLVM type
func (cg *CodeGenerator) birTypeToLLVMType(birType string) types.Type {
	fmt.Printf("[DEBUG] birTypeToLLVMType: Called with birType='%s'\n", birType)

	if strings.HasPrefix(birType, "map<") {
		if _, ok := cg.structTypes["BallerinaMap"]; !ok {
			cg.structTypes["BallerinaMap"] = types.NewStruct(
				types.I64,
				types.I64,
				types.I8Ptr,
			)
		}

		mapType := cg.structTypes["BallerinaMap"]
		fmt.Printf("[DEBUG] birTypeToLLVMType: birType='%s' (map) using BallerinaMap. Returning LLVM type: %s\n",
			birType, types.NewPointer(mapType).LLString())
		return types.NewPointer(mapType)
	}

	if strings.HasSuffix(birType, "[]") || strings.Contains(birType, "<>[]") {
		if _, ok := cg.structTypes["BallerinaArray"]; !ok {
			cg.structTypes["BallerinaArray"] = types.NewStruct(
				types.I64,
				types.I8Ptr,
			)
		}

		arrayType := cg.structTypes["BallerinaArray"]
		fmt.Printf("[DEBUG] birTypeToLLVMType: birType='%s' (array) using BallerinaArray. Returning LLVM type: %s\n",
			birType, types.NewPointer(arrayType).LLString())
		return types.NewPointer(arrayType)
	}

	if st, ok := cg.structTypes[birType]; ok {
		return types.NewPointer(st)
	}

	var retType types.Type
	switch birType {
	case "int":
		retType = types.I64
	case "boolean":
		retType = types.I1
	case "float":
		retType = types.Double
	case "string":
		if ballerinaStringType, ok := cg.structTypes["BallerinaString"]; ok {
			retType = types.NewPointer(ballerinaStringType)
		} else {
			ballerinaStringType = types.NewStruct(
				types.I64,
				types.I8Ptr,
			)
			cg.structTypes["BallerinaString"] = ballerinaStringType
			retType = types.NewPointer(ballerinaStringType)
		}
	case "()":
		retType = types.NewPointer(types.I64)
	default:
		if strings.Contains(birType, ":") {
			if strings.Contains(birType, "Printable") {
				if ballerinaStringType, ok := cg.structTypes["BallerinaString"]; ok {
					retType = types.NewPointer(ballerinaStringType)
				} else {
					ballerinaStringType = types.NewStruct(
						types.I64,
						types.I8Ptr,
					)
					cg.structTypes["BallerinaString"] = ballerinaStringType
					retType = types.NewPointer(ballerinaStringType)
				}
			} else {
				fmt.Printf("[DEBUG] birTypeToLLVMType: birType='%s' (FQN %s). Returning LLVM type: %s\n",
					birType, birType[strings.LastIndex(birType, ":")+1:], types.I8Ptr.LLString())
				retType = types.I8Ptr
			}
		} else {
			fmt.Printf("[WARNING] birTypeToLLVMType: Unknown type '%s', defaulting to i8*\n", birType)
			retType = types.I8Ptr
		}
	}

	fmt.Printf("[DEBUG] birTypeToLLVMType: birType='%s' (switch case). Returning LLVM type: %s\n", birType, retType.LLString())
	return retType
}

// AddRuntimeFunctions is defined in runtime.go
