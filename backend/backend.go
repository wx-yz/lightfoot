package backend

/*
#cgo CFLAGS: -I/opt/homebrew/Cellar/llvm/20.1.5/include -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
#cgo LDFLAGS: -L/opt/homebrew/Cellar/llvm/20.1.5/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names -lLLVM-20
#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Analysis.h>
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

// Generate converts the BIR package to LLVM IR code.
func (cg *CodeGenerator) Generate() (string, error) {
	fmt.Println("[INFO] backend.CodeGenerator.Generate called")

	// 1. Initialize LLVM types and runtime functions
	// Add runtime types and functions first so they're available for user code
	cg.AddRuntimeTypes()
	cg.AddRuntimeFunctions()

	// 2. Generate functions from BIR
	if cg.birPackage != nil {
		// Debug print for BIR package
		fmt.Printf("[DEBUG] BIR Package has %d functions\n", len(cg.birPackage.Functions))

		// Process each function in the BIR package
		for _, birFunc := range cg.birPackage.Functions {
			fmt.Printf("[DEBUG] Processing BIR function: %s\n", birFunc.Name)

			// Create LLVM function from BIR function
			llvmFunc := cg.generateFunction(birFunc)
			if llvmFunc != nil {
				// Store in function map for later reference
				cg.functions[birFunc.Name] = llvmFunc
				fmt.Printf("[DEBUG] Added function %s to cg.functions map\n", birFunc.Name)
			} else {
				fmt.Printf("[WARNING] Failed to generate LLVM function for %s\n", birFunc.Name)
			}
		}
	} else {
		fmt.Println("[WARNING] BIR Package is nil, no functions to generate")
	}

	// 3. Generate the ballerina_main function that will call init and main
	cg.generateBallerinaMain()

	// 4. Make sure ballerina_main exists in the module
	cg.EnsureBallerinaMainFunction()

	// 5. Verify and dump the LLVM module using the C API
	if cg.llvmModule != nil && cg.llvmModule.module != nil {
		// Verify the module to catch any errors
		var errorMessage *C.char

		// Verify the module for correctness
		errorResult := C.LLVMVerifyModule(cg.llvmModule.module, C.LLVMReturnStatusAction, &errorMessage)
		if errorResult != 0 {
			errorStr := C.GoString(errorMessage)
			C.LLVMDisposeMessage(errorMessage)
			fmt.Printf("[WARNING] Module verification warnings: %s\n", errorStr)
		}

		// Convert to string representation
		llvmIR := cg.llvmModule.String()
		if llvmIR == "" {
			return "", fmt.Errorf("failed to generate LLVM IR: module.String() returned empty string")
		}
		fmt.Println("[DEBUG] Successfully generated LLVM IR")
		return llvmIR, nil
	}

	return "", fmt.Errorf("failed to generate LLVM IR: module is nil")
}

// GetLLVMModule returns the underlying C API LLVMModule wrapper.
func (cg *CodeGenerator) GetLLVMModule() *LLVMModule {
	return cg.llvmModule
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
	for _, fn := range cg.birPackage.Functions {
		cg.generateFunction(fn)
		fmt.Printf("[DEBUG] Generated BIR function: '%s'\n", fn.Name)
	}

	// Handle module-level init functions (these are different from user init functions)
	if cg.birPackage.ModuleInitFunc != nil && cg.birPackage.ModuleInitFunc.Name != "init" {
		cg.generateFunction(cg.birPackage.ModuleInitFunc)
		fmt.Printf("[DEBUG] Module init function '%s' generated\n", cg.birPackage.ModuleInitFunc.Name)
	}

	if cg.birPackage.ModuleStartFunc != nil {
		cg.generateFunction(cg.birPackage.ModuleStartFunc)
		fmt.Printf("[DEBUG] Module start function '%s' generated\n", cg.birPackage.ModuleStartFunc.Name)
	}

	if cg.birPackage.ModuleStopFunc != nil {
		cg.generateFunction(cg.birPackage.ModuleStopFunc)
		fmt.Printf("[DEBUG] Module stop function '%s' generated\n", cg.birPackage.ModuleStopFunc.Name)
	}

	// If we have a user-defined init function, ensure it's generated
	if cg.birPackage.ActualInitFunc != nil {
		cg.generateFunction(cg.birPackage.ActualInitFunc)
		fmt.Printf("[DEBUG] User-defined init function '%s' successfully generated\n", cg.birPackage.ActualInitFunc.Name)
	}

	// Debug: Print all functions that were generated
	fmt.Printf("[DEBUG] All generated functions: ")
	for name := range cg.functions {
		fmt.Printf("'%s' ", name)
	}
	fmt.Printf("\n")

	// Now create the ballerina_main function that will be called from C
	cg.generateBallerinaMain()
}

// generateHTTPServices processes BIR service definitions and sets up HTTP listeners and resources.
func (cg *CodeGenerator) generateHTTPServices() {
	fmt.Println("[DEBUG] generateHTTPServices: Starting HTTP service generation.")

	// Skip HTTP service generation for basic programs that don't have services
	// This prevents errors when trying to access undefined HTTP structures
	if len(cg.birPackage.Functions) == 0 {
		fmt.Println("[DEBUG] generateHTTPServices: No functions found, skipping HTTP service generation.")
		return
	}

	// Check if we have HTTP-related structures defined
	if _, hasHTTPRequest := cg.structTypes["BallerinaHTTPRequest"]; !hasHTTPRequest {
		fmt.Println("[DEBUG] generateHTTPServices: No HTTP structures defined, skipping HTTP service generation.")
		return
	}

	// Only proceed if we actually have HTTP service definitions
	// For now, just log that we're skipping this
	fmt.Println("[DEBUG] generateHTTPServices: HTTP service generation skipped for basic programs.")
}

// CreateGlobalString creates a global string constant and returns a pointer to it (i8*).
func (cg *CodeGenerator) CreateGlobalString(s string, block *ir.Block, uniqueSuffix ...string) value.Value {
	name := ".str.g."
	if len(uniqueSuffix) > 0 {
		name += uniqueSuffix[0]
	} else {
		name += fmt.Sprintf("%d", len(cg.module.Globals))
	}
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
	// Create a global string constant with null terminator
	strConstant := constant.NewCharArrayFromString(s + "\x00")
	strGlobal := cg.module.NewGlobalDef(fmt.Sprintf(".str.%d", len(cg.globals)), strConstant)
	cg.globals[s] = strGlobal

	// Get i8* pointer to the first element
	zero := constant.NewInt(types.I32, 0)
	strPtr := block.NewGetElementPtr(strGlobal.ContentType, strGlobal, zero, zero)

	// Call ballerina_string_new_with_literal to create a BallerinaString
	newStringLiteralFunc := cg.functions["ballerina_string_new_with_literal"]
	if newStringLiteralFunc == nil {
		fmt.Println("[ERROR] ballerina_string_new_with_literal function not found")
		return strPtr // Return just the raw string pointer as fallback
	}

	// Create a constant integer for the string length
	strLen := constant.NewInt(types.I64, int64(len(s)))

	// Call the function to create a new BallerinaString
	return block.NewCall(newStringLiteralFunc, strPtr, strLen)
}

// birTypeToLLVMType converts a BIR type string to an LLVM type.
func (cg *CodeGenerator) birTypeToLLVMType(birType string) types.Type {
	// Handle basic types
	switch birType {
	case "int":
		return types.I64
	case "float":
		return types.Double
	case "boolean":
		return types.I1
	case "string":
		// String is represented as a pointer to a BallerinaString struct
		if stringType, ok := cg.structTypes["BallerinaString"]; ok {
			return types.NewPointer(stringType)
		}
		// Fallback to i8* if BallerinaString type not defined
		return types.I8Ptr
	case "", "nil", "void", "()":
		return types.Void
	case "error":
		// For now, treat error as a string pointer (simplified)
		// In a full implementation, error should be a proper struct
		return types.I8Ptr
	case "error?": // Nilable error
		// Same as error for now
		return types.I8Ptr
	default:
		// For complex types (arrays, maps, records, etc.)
		if strings.HasSuffix(birType, "[]") {
			// Array type - use a pointer to BallerinaArray
			if arrayType, ok := cg.structTypes["BallerinaArray"]; ok {
				return types.NewPointer(arrayType)
			}
			return types.I8Ptr // Fallback
		}

		// For map types
		if strings.HasPrefix(birType, "map<") {
			// Map type - use a pointer to BallerinaMap
			if mapType, ok := cg.structTypes["BallerinaMap"]; ok {
				return types.NewPointer(mapType)
			}
			return types.I8Ptr // Fallback
		}

		// For unknown types, use a generic pointer
		fmt.Printf("[WARNING] Unknown BIR type '%s', defaulting to i8*\n", birType)
		return types.I8Ptr
	}
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
