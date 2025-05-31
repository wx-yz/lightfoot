// filepath: /Users/rch/src/lightfoot/backend/llvm_wrappers.go
package backend

/*
#cgo CFLAGS: -I/opt/homebrew/Cellar/llvm/20.1.5/include -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
#cgo LDFLAGS: -L/opt/homebrew/Cellar/llvm/20.1.5/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names -lLLVM-20
#include <stdlib.h> // For C.free
#include <llvm-c/Types.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Analysis.h> // For LLVMVerifyModule
#include <llvm-c/BitWriter.h> // For LLVMWriteBitcodeToFile
#include <llvm-c/Linker.h> // For LLVMLinkModules2
// Get function by name
LLVMValueRef LLVMGetNamedFunction(LLVMModuleRef M, const char *Name) {
    return LLVMGetNamedFunction(M, Name);
}
// Add any other specific LLVM-C headers you might need directly
*/
import "C"
import (
	"fmt"
	"log"
	"unsafe"
)

// LLVMContext wraps LLVM's LLVMContextRef
type LLVMContext struct {
	context C.LLVMContextRef
}

// NewLLVMContext creates a new LLVM context
func NewLLVMContext() *LLVMContext {
	return &LLVMContext{
		context: C.LLVMContextCreate(),
	}
}

// Close releases the LLVM context
func (ctx *LLVMContext) Close() {
	if ctx.context != nil {
		C.LLVMContextDispose(ctx.context)
		ctx.context = nil
	}
}

// CharPtrType returns the LLVM type for char* (i8*).
func (ctx *LLVMContext) CharPtrType() C.LLVMTypeRef {
	if ctx.context == nil {
		log.Fatal("[ERROR] LLVMContext not initialized when calling CharPtrType")
		return nil
	}
	int8Type := C.LLVMInt8TypeInContext(ctx.context)
	return C.LLVMPointerType(int8Type, 0) // Address space 0
}

// LLVMModule wraps LLVM's LLVMModuleRef
type LLVMModule struct {
	module C.LLVMModuleRef
	name   string
}

// NewLLVMModule creates a new LLVM module with the given name
func NewLLVMModule(name string) *LLVMModule {
	cName := C.CString(name)
	defer C.free(unsafe.Pointer(cName))
	return &LLVMModule{
		module: C.LLVMModuleCreateWithName(cName),
		name:   name,
	}
}

// Close releases the LLVM module
func (mod *LLVMModule) Close() {
	if mod.module != nil {
		C.LLVMDisposeModule(mod.module)
		mod.module = nil
	}
}

// String returns the LLVM IR string representation of the module.
func (mod *LLVMModule) String() string {
	if mod.module == nil {
		log.Println("[WARNING] LLVMModule.String() called on a nil module.")
		return ""
	}
	cStr := C.LLVMPrintModuleToString(mod.module)
	if cStr == nil {
		log.Println("[WARNING] LLVMPrintModuleToString returned nil.")
		return ""
	}
	goStr := C.GoString(cStr)
	C.LLVMDisposeMessage(cStr) // Dispose of the C string allocated by LLVM
	return goStr
}

// AddFunction adds a new function to the module
func (mod *LLVMModule) AddFunction(name string, funcType C.LLVMTypeRef) C.LLVMValueRef {
	cName := C.CString(name)
	defer C.free(unsafe.Pointer(cName))
	return C.LLVMAddFunction(mod.module, cName, funcType)
}

// EnsureBallerinaMainExists ensures that the module has a ballerina_main function
// that serves as the entry point. This is what our runtime will call.
func (mod *LLVMModule) EnsureBallerinaMainExists(context *LLVMContext, builder *LLVMBuilder) error {
	// Check if the function already exists
	cName := C.CString("ballerina_main")
	defer C.free(unsafe.Pointer(cName))

	existingFunc := C.LLVMGetNamedFunction(mod.module, cName)
	if existingFunc != nil {
		fmt.Println("[DEBUG] Found existing ballerina_main function in module")
		return nil
	}

	fmt.Println("[DEBUG] Creating new ballerina_main function in module")

	// Create function type: void ()
	voidType := C.LLVMVoidTypeInContext(context.context)
	funcType := C.LLVMFunctionType(voidType, nil, 0, 0)

	// Add function to module
	mainFunc := C.LLVMAddFunction(mod.module, cName, funcType)

	// Create entry block
	entryBlockName := C.CString("entry")
	defer C.free(unsafe.Pointer(entryBlockName))
	entryBlock := C.LLVMAppendBasicBlockInContext(context.context, mainFunc, entryBlockName)

	// Position builder at end of entry block
	builder.SetBlock(entryBlock)

	// Add return void instruction
	builder.NewRetVoid()

	fmt.Println("[DEBUG] Successfully created ballerina_main function")
	return nil
}

// LLVMBuilder wraps LLVM's LLVMBuilderRef
type LLVMBuilder struct {
	builder C.LLVMBuilderRef
}

// NewLLVMBuilder creates a new LLVM IR builder
func NewLLVMBuilder() *LLVMBuilder {
	return &LLVMBuilder{
		builder: C.LLVMCreateBuilder(),
	}
}

// Close releases the LLVM builder
func (b *LLVMBuilder) Close() {
	if b.builder != nil {
		C.LLVMDisposeBuilder(b.builder)
		b.builder = nil
	}
}

// PositionBuilderAtEnd positions the builder at the end of the given basic block.
func (b *LLVMBuilder) PositionBuilderAtEnd(block C.LLVMBasicBlockRef) {
	C.LLVMPositionBuilderAtEnd(b.builder, block)
}

// SetBlock is an alias for PositionBuilderAtEnd for convenience.
func (b *LLVMBuilder) SetBlock(block C.LLVMBasicBlockRef) {
	b.PositionBuilderAtEnd(block)
}

// NewCall creates a new call instruction.
func (b *LLVMBuilder) NewCall(fn C.LLVMValueRef, args []C.LLVMValueRef, name string) C.LLVMValueRef {
	cName := C.CString(name)
	defer C.free(unsafe.Pointer(cName))

	var cArgs *C.LLVMValueRef
	if len(args) > 0 {
		cArgs = &args[0]
	}
	fnPtrType := C.LLVMTypeOf(fn)
	actualFnType := C.LLVMGetElementType(fnPtrType)
	return C.LLVMBuildCall2(b.builder, actualFnType, fn, cArgs, C.unsigned(len(args)), cName)
}

// NewRetVoid creates a new void return instruction.
func (b *LLVMBuilder) NewRetVoid() C.LLVMValueRef {
	return C.LLVMBuildRetVoid(b.builder)
}

// NewLoad creates a new load instruction.
func (b *LLVMBuilder) NewLoad(ptr C.LLVMValueRef, name string) C.LLVMValueRef {
	cName := C.CString(name)
	defer C.free(unsafe.Pointer(cName))
	ptrType := C.LLVMTypeOf(ptr)
	valType := C.LLVMGetElementType(ptrType)
	return C.LLVMBuildLoad2(b.builder, valType, ptr, cName)
}

// NewStore creates a new store instruction.
func (b *LLVMBuilder) NewStore(val C.LLVMValueRef, ptr C.LLVMValueRef) C.LLVMValueRef {
	return C.LLVMBuildStore(b.builder, val, ptr)
}

// LLVMTargetMachine wraps LLVM's LLVMTargetMachineRef
type LLVMTargetMachine struct {
	machine C.LLVMTargetMachineRef
}

// NewLLVMTargetMachine creates a new LLVM target machine for the host
func NewLLVMTargetMachine() (*LLVMTargetMachine, error) {
	// Initialize targets (important for TargetFromTriple to work)
	C.LLVMInitializeAllTargetInfos()
	C.LLVMInitializeAllTargets()
	C.LLVMInitializeAllTargetMCs()
	C.LLVMInitializeAllAsmParsers()
	C.LLVMInitializeAllAsmPrinters()

	triple := C.LLVMGetDefaultTargetTriple()
	defer C.LLVMDisposeMessage(triple)

	var target C.LLVMTargetRef
	var errStr *C.char
	if C.LLVMGetTargetFromTriple(triple, &target, &errStr) != 0 {
		goErrStr := C.GoString(errStr)
		C.LLVMDisposeMessage(errStr)
		return nil, fmt.Errorf("failed to get target from triple: %s", goErrStr)
	}

	cpu := C.CString("generic")
	defer C.free(unsafe.Pointer(cpu))
	features := C.CString("")
	defer C.free(unsafe.Pointer(features))

	machine := C.LLVMCreateTargetMachine(
		target,
		triple,
		cpu,
		features,
		C.LLVMCodeGenLevelDefault,
		C.LLVMRelocDefault,
		C.LLVMCodeModelDefault,
	)
	if machine == nil {
		return nil, fmt.Errorf("failed to create target machine")
	}
	return &LLVMTargetMachine{machine: machine}, nil
}

// Close releases the LLVM target machine
func (tm *LLVMTargetMachine) Close() {
	if tm.machine != nil {
		C.LLVMDisposeTargetMachine(tm.machine)
		tm.machine = nil
	}
}

// LLVMLinker might be a struct to manage linking LLVM modules.
// This is a placeholder structure.
type LLVMLinker struct {
	module *LLVMModule // Example: the primary module to link or operate on
	// Add other necessary fields, e.g., C.LLVMLinkerRef if using a C API for linking
}

// NewLLVMLinker creates a new LLVM linker instance.
// The 'module' argument could be the main module you are working with.
func NewLLVMLinker(module *LLVMModule) *LLVMLinker {
	// TODO: Implement actual LLVM linker initialization if available/needed.
	// This might involve setting up for LLVM's LTO or standard linking.
	fmt.Printf("Placeholder: NewLLVMLinker created for module %s\n", module.name)
	return &LLVMLinker{
		module: module,
	}
}

// Link performs the linking operation.
func (l *LLVMLinker) Link(outputFile string) error {
	// TODO: Implement the actual linking logic.
	// This could involve writing the linked module to 'outputFile'.
	fmt.Printf("Placeholder: LLVMLinker.Link called for %s\n", outputFile)
	return nil
}
