package backend

import (
	"unsafe"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// Runtime type IDs
const (
	TypeIDString = iota // "BallerinaString"
	TypeIDArray         // "BallerinaArray"
	TypeIDMap           // "BallerinaMap"
)

// Type name mapping
var typeIDToName = map[int]string{
	TypeIDString: "BallerinaString",
	TypeIDArray:  "BallerinaArray",
	TypeIDMap:    "BallerinaMap",
}

// Runtime functions for Ballerina operations

// BallerinaString represents a Ballerina string in memory
type BallerinaString struct {
	Length int64
	Data   *byte
}

// BallerinaArray represents a Ballerina array in memory
type BallerinaArray struct {
	Length int64
	Data   unsafe.Pointer
}

// BallerinaMap represents a Ballerina map in memory
type BallerinaMap struct {
	Size     int64
	Capacity int64
	Data     unsafe.Pointer
}

// Runtime function declarations
var runtimeFunctions = map[string]string{
	"ballerina.io.println": `
		declare void @ballerina.io.println(i8* %value) nounwind
	`,
	"ballerina.lang.string.concat": `
		declare i8* @ballerina.lang.string.concat(i8* %str1, i8* %str2) nounwind
	`,
	"ballerina.lang.array.new": `
		declare %BallerinaArray* @ballerina.lang.array.new(i64 %size, i64 %elementSize) nounwind
	`,
	"ballerina.lang.map.new": `
		declare %BallerinaMap* @ballerina.lang.map.new() nounwind
	`,
}

// AddRuntimeFunctions adds runtime function declarations to the module
func (cg *CodeGenerator) AddRuntimeFunctions() {
	// Ensure BallerinaString type is defined before using it here
	if _, ok := cg.structTypes["BallerinaString"]; !ok {
		// This is a safeguard; AddRuntimeTypes should be called first.
		// Define BallerinaString struct type if not already defined
		cg.structTypes["BallerinaString"] = types.NewStruct(
			types.I64,   // Length
			types.I8Ptr, // Data
		)
		// Also ensure BallerinaArray and BallerinaMap are defined if used before AddRuntimeTypes
		if _, okArr := cg.structTypes["BallerinaArray"]; !okArr {
			cg.structTypes["BallerinaArray"] = types.NewStruct(types.I64, types.I8Ptr)
		}
		if _, okMap := cg.structTypes["BallerinaMap"]; !okMap {
			cg.structTypes["BallerinaMap"] = types.NewStruct(types.I64, types.I64, types.I8Ptr)
		}
	}
	ballerinaStringTypePtr := types.NewPointer(cg.structTypes["BallerinaString"])

	// Create function declarations with corrected names matching the runtime.c implementation
	// Change the parameter type of ballerina_io_println to BallerinaString*
	printlnFunc := cg.module.NewFunc("ballerina_io_println", types.Void, ir.NewParam("value", ballerinaStringTypePtr))

	ballerinaArrayTypePtr := types.NewPointer(cg.structTypes["BallerinaArray"])
	arrayNewFunc := cg.module.NewFunc("ballerina_lang_array_new",
		ballerinaArrayTypePtr,
		ir.NewParam("size", types.I64), ir.NewParam("elementSize", types.I64))

	// For string concat, parameters should also be BallerinaString*
	// The C function ballerina_lang_string_concat takes BallerinaString*
	concatFunc := cg.module.NewFunc("ballerina_lang_string_concat", ballerinaStringTypePtr, /* return type */
		ir.NewParam("str1", ballerinaStringTypePtr), ir.NewParam("str2", ballerinaStringTypePtr))

	ballerinaMapTypePtr := types.NewPointer(cg.structTypes["BallerinaMap"])
	mapNewFunc := cg.module.NewFunc("ballerina_lang_map_new",
		ballerinaMapTypePtr)

	fnNameNewStringLiteral := "ballerina_string_new_with_literal"
	// ballerinaStringTypePtr already defined
	newStringLiteralFunc := cg.module.NewFunc(fnNameNewStringLiteral,
		ballerinaStringTypePtr,
		ir.NewParam("data", types.I8Ptr),
		ir.NewParam("len", types.I64),
	)

	// Add this to the AddRuntimeFunctions method
	mallocFunc := cg.module.NewFunc("malloc", types.I8Ptr,
		ir.NewParam("size", types.I64))
	cg.functions["malloc"] = mallocFunc

	// Store functions in the module's function map - IMPORTANT: use both original and C-style names
	cg.functions["ballerina.io.println"] = printlnFunc // Original BIR name
	cg.functions["ballerina_io_println"] = printlnFunc // C-style name

	cg.functions["ballerina.lang.array.new"] = arrayNewFunc // Original BIR name
	cg.functions["ballerina_lang_array_new"] = arrayNewFunc // C-style name

	cg.functions["ballerina.lang.string.concat"] = concatFunc // Original BIR name
	cg.functions["ballerina_lang_string_concat"] = concatFunc // C-style name

	cg.functions["ballerina.lang.map.new"] = mapNewFunc // Original BIR name
	cg.functions["ballerina_lang_map_new"] = mapNewFunc // C-style name

	cg.functions[fnNameNewStringLiteral] = newStringLiteralFunc
}

// AddRuntimeTypes adds runtime type declarations to the module
func (cg *CodeGenerator) AddRuntimeTypes() {
	// Create BallerinaString type
	stringType := types.NewStruct(
		types.I64,   // length
		types.I8Ptr, // data
	)
	cg.structTypes["BallerinaString"] = stringType

	// Create BallerinaArray type
	arrayType := types.NewStruct(
		types.I64,   // length
		types.I8Ptr, // data (array of pointers)
	)
	cg.structTypes["BallerinaArray"] = arrayType

	// Create BallerinaMap type
	mapType := types.NewStruct(
		types.I64,   // size
		types.I64,   // capacity
		types.I8Ptr, // data
	)
	cg.structTypes["BallerinaMap"] = mapType
}

// AddRuntimeGlobals adds runtime global variables to the module
func (cg *CodeGenerator) AddRuntimeGlobals() {
	// Add any necessary runtime global variables here
}

// AddRuntimeInit adds runtime initialization code
func (cg *CodeGenerator) AddRuntimeInit() {
	// Add any necessary runtime initialization code here
}

// Helper functions for runtime operations

// CreateString creates a new Ballerina string
func (cg *CodeGenerator) CreateString(str string, block *ir.Block) value.Value {
	// Create a global string constant with null terminator
	strConst := cg.module.NewGlobalDef("", constant.NewCharArrayFromString(str+"\x00"))

	// Call runtime function to create the string
	strLen := constant.NewInt(types.I64, int64(len(str)))
	newStringFunc := cg.functions["ballerina_string_new_with_literal"]
	if newStringFunc == nil {
		panic("ballerina_string_new_with_literal function not found")
	}

	// Get pointer to first character of the string constant
	zero := constant.NewInt(types.I32, 0)
	indices := []constant.Constant{zero, zero}
	charPtr := constant.NewGetElementPtr(strConst.Type().(*types.PointerType).ElemType, strConst, indices...)

	// Call the runtime function to create a proper BallerinaString
	return block.NewCall(newStringFunc, charPtr, strLen)
}

// CreateArray creates a new Ballerina array
func (cg *CodeGenerator) CreateArray(size int64, elementType types.Type, block *ir.Block) value.Value {
	// Call runtime function to create array
	arrayFunc := cg.functions["ballerina.lang.array.new"]
	if arrayFunc == nil {
		panic("array.new function not found")
	}
	sizeVal := constant.NewInt(types.I64, size)
	elementSize := constant.NewInt(types.I64, cg.getSizeOfType(elementType))

	return block.NewCall(arrayFunc, sizeVal, elementSize)
}

// CreateMap creates a new Ballerina map
func (cg *CodeGenerator) CreateMap(block *ir.Block) value.Value {
	// Call runtime function to create map
	mapFunc := cg.functions["ballerina.lang.map.new"]
	if mapFunc == nil {
		panic("map.new function not found")
	}
	return block.NewCall(mapFunc)
}

// PrintString prints a Ballerina string
func (cg *CodeGenerator) PrintString(str value.Value, block *ir.Block) {
	// Call println function with the BallerinaString pointer
	printlnFunc := cg.functions["ballerina_io_println"]
	if printlnFunc == nil {
		panic("println function not found")
	}
	block.NewCall(printlnFunc, str)
}

// ConcatStrings concatenates two Ballerina strings
func (cg *CodeGenerator) ConcatStrings(str1, str2 value.Value, block *ir.Block) value.Value {
	// Get string data pointers
	strType := cg.structTypes["BallerinaString"]
	data1Ptr := block.NewGetElementPtr(strType, str1, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	data2Ptr := block.NewGetElementPtr(strType, str2, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	data1 := block.NewLoad(types.I8Ptr, data1Ptr)
	data2 := block.NewLoad(types.I8Ptr, data2Ptr)

	// Call concat function
	concatFunc := cg.functions["ballerina.lang.string.concat"]
	if concatFunc == nil {
		panic("string.concat function not found")
	}
	return block.NewCall(concatFunc, data1, data2)
}
