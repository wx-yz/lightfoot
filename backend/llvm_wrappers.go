// filepath: /Users/rch/src/lightfoot/backend/llvm_wrappers.go
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

// LLVMTargetMachine wraps LLVM's LLVMTargetMachineRef
type LLVMTargetMachine struct {
	machine C.LLVMTargetMachineRef
}

// NewLLVMTargetMachine creates a new LLVM target machine for the host
func NewLLVMTargetMachine() (*LLVMTargetMachine, error) {
	// Initialize all targets
	C.LLVMInitializeAllTargetInfos()
	C.LLVMInitializeAllTargets()
	C.LLVMInitializeAllTargetMCs()
	C.LLVMInitializeAllAsmParsers()
	C.LLVMInitializeAllAsmPrinters()

	// Get the host triple
	triple := C.LLVMGetDefaultTargetTriple()
	defer C.LLVMDisposeMessage(triple)

	var target C.LLVMTargetRef
	var err *C.char
	if C.LLVMGetTargetFromTriple(triple, &target, &err) != 0 {
		errMsg := C.GoString(err)
		C.LLVMDisposeMessage(err)
		return nil, fmt.Errorf("failed to get target from triple: %s", errMsg)
	}

	// Create target machine
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
