package backend

import (
	"fmt"
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
	// Add the array println function declaration
	printlnArrayFunc := cg.module.NewFunc("ballerina_io_println_array", types.Void, ir.NewParam("array", ballerinaArrayTypePtr))
	arrayNewFunc := cg.module.NewFunc("ballerina_lang_array_new",
		ballerinaArrayTypePtr,
		ir.NewParam("size", types.I64), ir.NewParam("elementSize", types.I64))

	// Array set function declaration
	arraySetFunc := cg.module.NewFunc("ballerina_lang_array_set", types.Void,
		ir.NewParam("arr", ballerinaArrayTypePtr),
		ir.NewParam("index", types.I64),
		ir.NewParam("value", types.I8Ptr)) // void* becomes i8*

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

	// Add the array println function to the functions map
	cg.functions["ballerina_io_println_array"] = printlnArrayFunc

	cg.functions["ballerina.lang.array.new"] = arrayNewFunc // Original BIR name
	cg.functions["ballerina_lang_array_new"] = arrayNewFunc // C-style name

	cg.functions["ballerina.lang.array.set"] = arraySetFunc // Original BIR name
	cg.functions["ballerina_lang_array_set"] = arraySetFunc // C-style name

	cg.functions["ballerina.lang.string.concat"] = concatFunc // Original BIR name
	cg.functions["ballerina_lang_string_concat"] = concatFunc // C-style name

	cg.functions["ballerina.lang.map.new"] = mapNewFunc // Original BIR name
	cg.functions["ballerina_lang_map_new"] = mapNewFunc // C-style name

	cg.functions[fnNameNewStringLiteral] = newStringLiteralFunc

	// Add ballerina_runtime_int_to_string
	intToStringFunc := cg.module.NewFunc("ballerina_runtime_int_to_string",
		ballerinaStringTypePtr, // Returns BallerinaString*
		ir.NewParam("val", types.I64),
	)
	cg.functions["ballerina_runtime_int_to_string"] = intToStringFunc

	// Add ballerina_runtime_bool_to_string
	boolToStringFunc := cg.module.NewFunc("ballerina_runtime_bool_to_string",
		ballerinaStringTypePtr, // Returns BallerinaString*
		ir.NewParam("val", types.I1),
	)
	cg.functions["ballerina_runtime_bool_to_string"] = boolToStringFunc

	// HTTP Server Functions
	if _, ok := cg.functions["ballerina_http_server_start"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_server_start", types.I32, ir.NewParam("port", types.I32), ir.NewParam("register_service_handlers_now", types.I32))
		cg.functions["ballerina_http_server_start"] = fn
	}

	httpReqPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPRequest"])
	httpRespPtrType := types.NewPointer(cg.structTypes["BallerinaHTTPResponse"])

	// ballerina_resource_func_ptr is void (*)(BallerinaHTTPRequest*, BallerinaHTTPResponse*)
	// In LLVM, this is a pointer to a function: void (BallerinaHTTPRequest*, BallerinaHTTPResponse*)*
	resourceFuncSig := types.NewFunc(types.Void, httpReqPtrType, httpRespPtrType)
	resourceFuncPtrType := types.NewPointer(resourceFuncSig)

	if _, ok := cg.functions["ballerina_http_register_resource"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_register_resource", types.Void,
			ir.NewParam("path", types.I8Ptr),
			ir.NewParam("method", types.I8Ptr),
			ir.NewParam("handler", resourceFuncPtrType),
		)
		cg.functions["ballerina_http_register_resource"] = fn
	}

	// HTTP Server Wait Function
	if _, ok := cg.functions["ballerina_http_server_wait"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_server_wait", types.Void)
		cg.functions["ballerina_http_server_wait"] = fn
		fmt.Printf("[DEBUG] AddRuntimeFunctions: Added ballerina_http_server_wait function\n")
	}

	// HTTP Response utility functions
	if _, ok := cg.functions["ballerina_http_response_set_string_body"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_response_set_string_body", types.Void,
			ir.NewParam("resp", httpRespPtrType),
			ir.NewParam("body_str", types.NewPointer(cg.structTypes["BallerinaString"])),
		)
		cg.functions["ballerina_http_response_set_string_body"] = fn
	}
	if _, ok := cg.functions["ballerina_http_response_set_status_code"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_response_set_status_code", types.Void,
			ir.NewParam("resp", httpRespPtrType),
			ir.NewParam("status_code", types.I32),
		)
		cg.functions["ballerina_http_response_set_status_code"] = fn
	}
	if _, ok := cg.functions["ballerina_http_request_get_placeholder_body"]; !ok {
		fn := cg.module.NewFunc("ballerina_http_request_get_placeholder_body", types.NewPointer(cg.structTypes["BallerinaString"]),
			ir.NewParam("req", httpReqPtrType),
		)
		cg.functions["ballerina_http_request_get_placeholder_body"] = fn
	}

	// Add error handling functions
	cg.addErrorFunctions()

	// Add float and boolean print functions
	cg.AddFloatAndBoolPrintFunctions()
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

	// HTTP Request Object (simplified)
	if _, ok := cg.structTypes["BallerinaHTTPRequest"]; !ok {
		ballerinaHTTPRequestType := types.NewStruct(
			types.I8Ptr, // path
			types.I8Ptr, // method
			types.NewPointer(cg.structTypes["BallerinaString"]), // placeholder_body
			// Add more fields corresponding to BallerinaHTTPRequest in C
		)
		cg.module.NewTypeDef("BallerinaHTTPRequest", ballerinaHTTPRequestType)
		cg.structTypes["BallerinaHTTPRequest"] = ballerinaHTTPRequestType
	}

	// HTTP Response Object (simplified)
	if _, ok := cg.structTypes["BallerinaHTTPResponse"]; !ok {
		ballerinaHTTPResponseType := types.NewStruct(
			types.I32, // status_code
			types.NewPointer(cg.structTypes["BallerinaString"]), // body
			// Add more fields corresponding to BallerinaHTTPResponse in C
		)
		cg.module.NewTypeDef("BallerinaHTTPResponse", ballerinaHTTPResponseType)
		cg.structTypes["BallerinaHTTPResponse"] = ballerinaHTTPResponseType
	}
}

// AddRuntimeGlobals adds runtime global variables to the module
func (cg *CodeGenerator) AddRuntimeGlobals() {
	// Add any necessary runtime global variables here
}

// AddRuntimeInit adds runtime initialization code
func (cg *CodeGenerator) AddRuntimeInit() {
	// Create a simple runtime initialization that doesn't print debug messages
	// Remove any startup messages to prevent unwanted output
}

// Helper functions for runtime operations

// CreateArray creates a new Ballerina array
func (cg *CodeGenerator) CreateArray(size int64, elementType types.Type, block *ir.Block) value.Value {
	// Call runtime function to create array
	arrayFunc := cg.functions["ballerina.lang.array.new"]
	if arrayFunc == nil {
		panic("array.new function not found")
	}
	sizeVal := constant.NewInt(types.I64, size)
	elementSizeVal := constant.NewInt(types.I64, cg.getSizeOfType(elementType)) // Use the new method

	return block.NewCall(arrayFunc, sizeVal, elementSizeVal)
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
	// Call the correct runtime function with BallerinaString* arguments
	concatFunc := cg.functions["ballerina_lang_string_concat"]
	if concatFunc == nil {
		concatFunc = cg.functions["ballerina.lang.string.concat"]
	}
	if concatFunc == nil {
		panic("string.concat function not found")
	}
	return block.NewCall(concatFunc, str1, str2)
}
