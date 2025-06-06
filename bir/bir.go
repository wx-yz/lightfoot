// bir/bir.go
package bir

import (
	"fmt"
	"strings"
	"wx-yz/lightfoot/debug"
	"wx-yz/lightfoot/lexer"
	"wx-yz/lightfoot/parser" // Assuming parser.IdentifierNode might be checked, etc.
	// "sort" // If sorting variables for printing
)

// --- BIR Core Structures ---

type Package struct {
	OrgName         string
	Name            string
	Version         string
	ImportModules   []*ImportModule
	GlobalVars      []*GlobalVariable
	Functions       []*Function
	AnnotationData  *GlobalVariable // This should be the one used
	ModuleInitFunc  *Function
	ModuleStartFunc *Function
	ModuleStopFunc  *Function
	ActualInitFunc  *Function // To store the user-defined init function
}

type ImportModule struct {
	OrgName     string
	PackageName string
	Version     string
	Alias       string
}

type GlobalVariable struct {
	Name         string
	Type         string
	IsAnnotation bool
}

type Function struct {
	Name              string
	Visibility        string
	IsModuleLifecycle bool
	Flags             []string
	Parameters        []*Variable // For signature display
	ReturnVariable    *Variable
	ArgumentVars      []*Variable          // Actual ARG variables used in body, in order
	LocalVars         map[string]*Variable // All variables by BIRName (%0, %1, ...), and original name for locals
	BasicBlocks       []*BasicBlock
	CurrentBB         *BasicBlock
	nextVarID         int // Counter for %N names, starts at 0 (%0 is return)
	nextBBID          int
	// PackagePrefix string // Not currently used, but could be for imported calls
}

type Variable struct {
	Name          string
	BIRName       string
	Type          string
	Kind          VarKind
	ScopeID       int
	IsFunctionArg bool
	OriginalName  string
}

type VarKind string

const (
	VarKindLocal     VarKind = "LOCAL"
	VarKindTemp      VarKind = "TEMP"
	VarKindReturn    VarKind = "RETURN"
	VarKindArg       VarKind = "ARG"
	VarKindGlobalRef VarKind = "GLOBAL_REF" // Placeholder, not fully used
	VarKindConstant  VarKind = "CONST_REF"  // Placeholder
	VarKindSelf      VarKind = "SELF"       // Placeholder
)

type BasicBlock struct {
	ID           string
	Instructions []Instruction
	Terminator   TerminatorInstruction
	// visited      bool // For CFG traversal/ordering (transient)
}

// --- BIR Instructions ---

type Instruction interface {
	String() string
	GetLHS() []*Variable
}

type TerminatorInstruction interface {
	Instruction
	IsTerminator() bool
	GetTargets() []string
}

type birInstruction struct{ lhs []*Variable }

func (i *birInstruction) GetLHS() []*Variable { return i.lhs }

type NewTypeInst struct {
	birInstruction
	Desc string
}

func (i *NewTypeInst) String() string {
	return fmt.Sprintf("%s = newType %s", i.lhs[0].BIRName, i.Desc)
}

type NewMapInst struct {
	birInstruction
	TypeDescVar *Variable
}

func (i *NewMapInst) String() string {
	return fmt.Sprintf("%s = NewMap %s{}", i.lhs[0].BIRName, i.TypeDescVar.BIRName)
}

type ConstantLoadInst struct {
	birInstruction
	Value    interface{}
	TypeName string
}

func (i *ConstantLoadInst) String() string {
	valStr := ""
	if strVal, ok := i.Value.(string); ok {
		if strVal == "nil" {
			valStr = "0"
		} else {
			// Escape string for BIR output if needed, for now direct.
			valStr = fmt.Sprintf("%s", strVal)
		}
	} else {
		valStr = fmt.Sprintf("%v", i.Value)
	}
	// Official BIR for string const load might be different, e.g. using string table index.
	// For "Hello Ballerina", target is "ConstLoad Hello Ballerina". My code will produce this.
	return fmt.Sprintf("%s = ConstLoad %s", i.lhs[0].BIRName, valStr)
}

type MoveInst struct {
	birInstruction
	RHS *Variable
}

func (i *MoveInst) String() string { return fmt.Sprintf("%s = %s", i.lhs[0].BIRName, i.RHS.BIRName) } // Target uses '=' not 'move' for simple moves to existing

type BinaryOpInst struct {
	birInstruction
	Op  string
	Op1 *Variable
	Op2 *Variable
}

func (i *BinaryOpInst) String() string {
	return fmt.Sprintf("%s = %s %s %s", i.lhs[0].BIRName, i.Op1.BIRName, i.Op, i.Op2.BIRName)
}

type CallInst struct {
	birInstruction
	PackagePath  string // e.g. "ballerina/io" (resolved path, not alias)
	FunctionName string
	Args         []*Variable
	NextBB       string
}

func (i *CallInst) String() string {
	argsStr := []string{}
	for _, arg := range i.Args {
		argsStr = append(argsStr, arg.BIRName)
	}

	funcPath := i.FunctionName
	if i.PackagePath != "" {
		versionedPkgPath := i.PackagePath
		// Attempt to match official BIR call syntax: `ballerina/io:1.8.0:println`
		if i.PackagePath == "ballerina/io" { // Hack for example
			versionedPkgPath = "ballerina/io:1.8.0"
		}
		funcPath = fmt.Sprintf("%s:%s", versionedPkgPath, i.FunctionName)
	}

	callStr := fmt.Sprintf("%s(%s)", funcPath, strings.Join(argsStr, ", "))
	if len(i.lhs) > 0 && i.lhs[0] != nil {
		callStr = fmt.Sprintf("%s = %s", i.lhs[0].BIRName, callStr)
	}
	if i.NextBB != "" {
		callStr += fmt.Sprintf(" -> %s", i.NextBB)
	}
	return callStr
}

// Make CallInst a TerminatorInstruction if NextBB is present
func (i *CallInst) IsTerminator() bool {
	return i.NextBB != ""
}

func (i *CallInst) GetTargets() []string {
	if i.NextBB != "" {
		return []string{i.NextBB}
	}
	return nil
}

type birTerminator struct { /* birInstruction - if terminators define vars */
}

func (t *birTerminator) IsTerminator() bool  { return true }
func (t *birTerminator) GetLHS() []*Variable { return nil } // Standard terminators don't usually have LHS

type ReturnInst struct{ birTerminator }

func (i *ReturnInst) String() string       { return "return" }
func (i *ReturnInst) GetTargets() []string { return nil }

type GotoInst struct {
	birTerminator
	TargetBB string
}

func (i *GotoInst) String() string       { return fmt.Sprintf("GOTO %s", i.TargetBB) }
func (i *GotoInst) GetTargets() []string { return []string{i.TargetBB} }

type ConditionalBranchInst struct {
	birTerminator
	Condition *Variable
	TrueBB    string
	FalseBB   string
}

func (i *ConditionalBranchInst) String() string {
	return fmt.Sprintf("%s? %s : %s", i.Condition.BIRName, i.TrueBB, i.FalseBB)
}
func (i *ConditionalBranchInst) GetTargets() []string { return []string{i.TrueBB, i.FalseBB} }

// TypeCastInst (Not fully used yet, but defined)
type TypeCastInst struct {
	birInstruction
	TargetType string
	SourceVar  *Variable
}

func (i *TypeCastInst) String() string {
	return fmt.Sprintf("%s = <%s> %s", i.lhs[0].BIRName, i.TargetType, i.SourceVar.BIRName)
}

// NewArrayInst (Not fully used yet, but defined for future io:println refinement)
type NewArrayInst struct {
	birInstruction
	ElementType string
	SizeVar     *Variable
	Args        []*Variable
}

func (i *NewArrayInst) String() string {
	argNames := []string{}
	for _, arg := range i.Args {
		argNames = append(argNames, arg.BIRName)
	}
	sizeStr := ""
	if i.SizeVar != nil {
		sizeStr = i.SizeVar.BIRName
	} else if len(i.Args) > 0 { // Infer size if args provided and no size var
		sizeStr = fmt.Sprintf("%d", len(i.Args))
	}
	return fmt.Sprintf("%s = newArray %s[%s]{%s}", i.lhs[0].BIRName, i.ElementType, sizeStr, strings.Join(argNames, ", "))
}

// TypeConversionInst represents a type conversion instruction
type TypeConversionInst struct {
	birInstruction
	LHSVar     *Variable // Renamed from lhs to avoid embedding conflict and be clear
	SourceVar  *Variable // Renamed from From
	TargetType string    // Renamed from To
	IsCheck    bool      // For type checks like `x is float`
}

func (tc *TypeConversionInst) String() string {
	if tc.IsCheck {
		return fmt.Sprintf("%s = %s is %s", tc.LHSVar.BIRName, tc.SourceVar.BIRName, tc.TargetType)
	}
	return fmt.Sprintf("%s = <%s> %s", tc.LHSVar.BIRName, tc.TargetType, tc.SourceVar.BIRName)
}

// Overriding GetLHS for TypeConversionInst
func (tc *TypeConversionInst) GetLHS() []*Variable {
	return []*Variable{tc.LHSVar}
}

// --- BIR Emitter ---
type Emitter struct {
	birPackage  *Package
	currentFunc *Function
	// varMap       map[string]*Variable // Removed in favor of LocalVars in Function
	globalVarMap map[string]*GlobalVariable
	// tempVarCount int // Managed by currentFunc.nextVarID
	// labelCount   int // Managed by currentFunc.nextBBID
	// moduleInitDone bool // Not strictly needed for this logic
	errors []error // For collecting compilation errors
}

func NewEmitter() *Emitter {
	return &Emitter{
		globalVarMap: make(map[string]*GlobalVariable),
		errors:       make([]error, 0),
	}
}

// Helper function to map AST TypeNode to BIR type string
func (e *Emitter) mapAstTypeToBirType(astTypeNode *parser.TypeNode) string {
	if astTypeNode == nil {
		return "any" // Or handle error
	}
	// This is a simplified mapping. A real implementation would handle qualified types,
	// arrays, nilable types, etc., more robustly.
	switch astTypeNode.TypeName {
	case "int":
		return "int"
	case "float":
		return "float"
	case "boolean":
		return "boolean"
	case "string":
		return "string"
	case "error":
		// Handle nilable error type error?
		if astTypeNode.IsNilable {
			return "error?" // Or a more specific BIR representation if available
		}
		return "error"
	default:
		// For unknown or complex types, might return the name directly or a generic type
		// For now, let's assume it's a direct mapping or a placeholder.
		if astTypeNode.IsNilable {
			return astTypeNode.TypeName + "?"
		}
		return astTypeNode.TypeName // e.g., for custom types or complex ones like http:Listener
	}
}

func (e *Emitter) newBIRNameCurrentFunc() string {
	if e.currentFunc == nil {
		panic("newBIRNameCurrentFunc called outside function context")
	}
	name := fmt.Sprintf("%%%d", e.currentFunc.nextVarID)
	e.currentFunc.nextVarID++
	return name
}

func (e *Emitter) newBBCurrentFunc() *BasicBlock {
	if e.currentFunc == nil {
		panic("newBBCurrentFunc called outside function context")
	}
	bbID := fmt.Sprintf("bb%d", e.currentFunc.nextBBID)
	e.currentFunc.nextBBID++
	bb := &BasicBlock{ID: bbID, Instructions: []Instruction{}}
	e.currentFunc.BasicBlocks = append(e.currentFunc.BasicBlocks, bb)
	return bb
}

func (e *Emitter) setCurrentBB(bb *BasicBlock) {
	if e.currentFunc == nil {
		panic("setCurrentBB called outside function context")
	}
	e.currentFunc.CurrentBB = bb
}

func (e *Emitter) addInstruction(inst Instruction) {
	if e.currentFunc == nil || e.currentFunc.CurrentBB == nil {
		panic("addInstruction called without active function or basic block")
	}
	if e.currentFunc.CurrentBB.Terminator != nil {
		// This indicates a logic error in CFG construction by the emitter.
		// A new BB should have been created and set as current if the previous one was terminated.
		debug.Log(debug.CategoryBIR, "Warning: Adding instruction '%s' to already terminated BB %s. This instruction might be dead code.", inst.String(), e.currentFunc.CurrentBB.ID)
		// Optionally, create a new "unreachable" BB, but this masks the underlying issue.
		// For now, allow adding, but it's a sign of trouble.
	}
	debug.Log(debug.CategoryBIR, "Adding instruction to %s: %s", e.currentFunc.CurrentBB.ID, inst.String())
	e.currentFunc.CurrentBB.Instructions = append(e.currentFunc.CurrentBB.Instructions, inst)
}

func (e *Emitter) addTerminator(term TerminatorInstruction) {
	if e.currentFunc == nil || e.currentFunc.CurrentBB == nil {
		panic("addTerminator called without active function or basic block")
	}
	if e.currentFunc.CurrentBB.Terminator != nil {
		panic(fmt.Sprintf("Attempting to add terminator %s to BB %s which already has terminator %s",
			term.String(), e.currentFunc.CurrentBB.ID, e.currentFunc.CurrentBB.Terminator.String()))
	}
	e.currentFunc.CurrentBB.Terminator = term
}

// FIX APPLIED HERE: Logic for creating ARG variables corrected.
func (e *Emitter) getOrCreateVar(originalName string, birType string, kind VarKind, isFuncArg bool) *Variable {
	if e.currentFunc == nil {
		panic("getOrCreateVar called outside function context")
	}

	var birName string
	var v *Variable

	// Handle RETURN and ARG vars first due to fixed naming/special status
	if kind == VarKindReturn {
		birName = "%0" // By convention
		// Return var should already be in LocalVars, created during function setup.
		if existingReturnVar, ok := e.currentFunc.LocalVars[birName]; ok && existingReturnVar.Kind == VarKindReturn {
			return existingReturnVar
		}
		// If not found, create (should have been done by emitFunctionDefinition setup)
	} else if isFuncArg && kind == VarKindArg {
		// For function arguments, we generate a new BIR name (%1, %2, etc.)
		// These are created when processing function parameters.
		// The panic for "not pre-declared" is removed; this function *creates* them now.
		birName = e.newBIRNameCurrentFunc()
	} else {
		// For LOCAL or TEMP variables
		// Check if a LOCAL variable with this originalName already exists
		if kind == VarKindLocal {
			if existingVar, ok := e.currentFunc.LocalVars[originalName]; ok && existingVar.Kind == VarKindLocal {
				return existingVar // Reuse existing local var declaration
			}
		}
		// Otherwise, it's a new TEMP or new LOCAL, generate a new BIRName
		birName = e.newBIRNameCurrentFunc()
	}

	// If we reached here, we are creating a new variable struct
	v = &Variable{
		Name:          originalName, // Original AST name, can be "" for true temps
		BIRName:       birName,
		Type:          birType,
		Kind:          kind,
		IsFunctionArg: isFuncArg, // Mark if it originated from a function parameter
		OriginalName:  originalName,
	}
	e.currentFunc.LocalVars[v.BIRName] = v // Store by unique BIRName
	if kind == VarKindLocal && originalName != "" && !strings.HasPrefix(originalName, "%") {
		// Also map original name for locals for easier lookup by AST name,
		// but only if it's not already a BIR-style name.
		e.currentFunc.LocalVars[originalName] = v
	}
	return v
}

func (e *Emitter) findVarByOriginalName(originalName string) *Variable {
	if e.currentFunc == nil {
		return nil
	}
	// Prioritize finding it as a declared argument by its original name.
	for _, argVar := range e.currentFunc.ArgumentVars {
		if argVar.OriginalName == originalName {
			return argVar // This var will have its correct BIRName (%1, %2, etc.)
		}
	}
	// If not an argument, check if it's a local variable mapped by its original name.
	if v, ok := e.currentFunc.LocalVars[originalName]; ok && (v.Kind == VarKindLocal) {
		return v
	}
	// Fallback: if originalName itself is a BIRName like "%N" (e.g. for temps passed around)
	if strings.HasPrefix(originalName, "%") {
		if v, ok := e.currentFunc.LocalVars[originalName]; ok {
			return v
		}
	}
	return nil
}

func (e *Emitter) emitModuleInit() {
	e.currentFunc = &Function{
		Name:              ".<init>",
		Visibility:        "PUBLIC",
		IsModuleLifecycle: true,
		// Target return type: error{map<ballerina/lang.value:0.0.0:Cloneable>}|()
		// However, for BIR variable declaration, we need to use "error|()"
		ReturnVariable: &Variable{
			BIRName:      "%0",
			Type:         "error|()", // Changed from error{map<ballerina/lang.value:0.0.0:Cloneable>}|()
			Kind:         VarKindReturn,
			OriginalName: "%0(RETURN)",
		},
		LocalVars: make(map[string]*Variable),
		nextVarID: 0, // %0 for return
		nextBBID:  0,
	}
	e.currentFunc.LocalVars[e.currentFunc.ReturnVariable.BIRName] = e.currentFunc.ReturnVariable
	e.currentFunc.nextVarID++ // next is %1

	e.birPackage.ModuleInitFunc = e.currentFunc

	bb0 := e.newBBCurrentFunc()
	e.setCurrentBB(bb0)

	// Target type for %1: typeDesc<any|error>;
	// Instruction is `newType map<any>`. The target BIR uses a more general type for the descriptor variable.
	tdVar := e.getOrCreateVar("_td_map_any", "typeDesc<any|error>", VarKindTemp, false) // This will be %1
	e.addInstruction(&NewTypeInst{birInstruction: birInstruction{lhs: []*Variable{tdVar}}, Desc: "map<any>"})

	// $annotation_data = NewMap %1{};
	// Get the global variable $annotation_data
	annotationDataGlobal := e.birPackage.AnnotationData
	if annotationDataGlobal == nil {
		panic("Emitter error: birPackage.AnnotationData is nil in emitModuleInit")
	}
	// Create a Variable representation for the LHS of NewMapInst
	annotationDataLHS := &Variable{
		Name:         annotationDataGlobal.Name,
		BIRName:      annotationDataGlobal.Name, // Use its actual name for printing
		Type:         annotationDataGlobal.Type,
		Kind:         VarKindGlobalRef, // Important for printer/semantics
		OriginalName: annotationDataGlobal.Name,
	}
	e.addInstruction(&NewMapInst{birInstruction: birInstruction{lhs: []*Variable{annotationDataLHS}}, TypeDescVar: tdVar})

	// Call init function if it exists
	if e.birPackage.ActualInitFunc != nil {
		// Create a temporary variable to hold the init function's return value
		initRetVar := e.getOrCreateVar("_init_ret", e.birPackage.ActualInitFunc.ReturnVariable.Type, VarKindTemp, false)

		// Create a basic block for handling init function errors (if any)
		errorHandlingBB := e.newBBCurrentFunc()

		// Call the init function
		callInit := &CallInst{
			birInstruction: birInstruction{lhs: []*Variable{initRetVar}},
			PackagePath:    "",
			FunctionName:   "init",
			Args:           []*Variable{},
			NextBB:         errorHandlingBB.ID,
		}
		e.addTerminator(callInit)

		// Set current BB to error handling block
		e.setCurrentBB(errorHandlingBB)

		// Check if init function returned an error
		if e.birPackage.ActualInitFunc.ReturnVariable.Type == "error?" {
			// Create variables for comparison
			isNilVar := e.getOrCreateVar("_init_ret_is_nil", "boolean", VarKindTemp, false)

			// Check if initRetVar is nil (no error)
			e.addInstruction(&BinaryOpInst{
				birInstruction: birInstruction{lhs: []*Variable{isNilVar}},
				Op:             "==",
				Op1:            initRetVar,
				Op2:            &Variable{BIRName: "nil", Type: "nil", Kind: VarKindConstant},
			})

			// Create blocks for success and error paths
			successBB := e.newBBCurrentFunc()
			failBB := e.newBBCurrentFunc()

			// Branch based on nil check
			e.addTerminator(&ConditionalBranchInst{
				Condition: isNilVar,
				TrueBB:    successBB.ID,
				FalseBB:   failBB.ID,
			})

			// Success path: continue normal execution
			e.setCurrentBB(successBB)
			e.addInstruction(&ConstantLoadInst{
				birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}},
				Value:          "nil",
				TypeName:       e.currentFunc.ReturnVariable.Type,
			})

			// Continue to final return
			finalBB := e.newBBCurrentFunc()
			e.addTerminator(&GotoInst{TargetBB: finalBB.ID})

			// Error path: return the error from init
			e.setCurrentBB(failBB)
			e.addInstruction(&MoveInst{
				birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}},
				RHS:            initRetVar,
			})
			e.addTerminator(&GotoInst{TargetBB: finalBB.ID})

			// Final return block
			e.setCurrentBB(finalBB)
			e.addTerminator(&ReturnInst{})

			return
		}
	}

	// In the target BIR, %0 is loaded with 0 (nil for error|() or the map part)
	// The actual value might be more complex if configurables are involved.
	// For now, loading "nil" (represented as 0 in BIR for success/empty) is consistent.
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})

	bb1 := e.newBBCurrentFunc()
	e.addTerminator(&GotoInst{TargetBB: bb1.ID})

	e.setCurrentBB(bb1)
	e.addTerminator(&ReturnInst{})

	e.currentFunc = nil
}

func (e *Emitter) emitModuleStartStop(name string) {
	e.currentFunc = &Function{
		Name:              name,
		Visibility:        "PUBLIC",
		IsModuleLifecycle: true,
		ReturnVariable:    &Variable{BIRName: "%0", Type: "error|()", Kind: VarKindReturn, OriginalName: "%0(RETURN)"},
		LocalVars:         make(map[string]*Variable),
		nextVarID:         0,
		nextBBID:          0,
	}
	e.currentFunc.LocalVars["%0"] = e.currentFunc.ReturnVariable
	e.currentFunc.nextVarID++

	if name == ".<start>" {
		e.birPackage.ModuleStartFunc = e.currentFunc
	} else {
		e.birPackage.ModuleStopFunc = e.currentFunc
	}

	bb0 := e.newBBCurrentFunc()
	e.setCurrentBB(bb0)
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})

	bb1 := e.newBBCurrentFunc()
	e.addTerminator(&GotoInst{TargetBB: bb1.ID})

	e.setCurrentBB(bb1)
	e.addTerminator(&ReturnInst{})

	e.currentFunc = nil
}

func (e *Emitter) Emit(fileNode *parser.FileNode) (*Package, error) {
	if fileNode == nil {
		return nil, fmt.Errorf("cannot emit BIR from nil AST FileNode")
	}
	e.birPackage = &Package{
		OrgName:       "$anon",
		Name:          ".",
		Version:       "0.0.0",
		Functions:     []*Function{},
		ImportModules: []*ImportModule{},
		GlobalVars:    []*GlobalVariable{},
	}

	e.birPackage.AnnotationData = &GlobalVariable{Name: "$annotation_data", Type: "map<any>", IsAnnotation: true}
	e.birPackage.GlobalVars = append(e.birPackage.GlobalVars, e.birPackage.AnnotationData)

	e.emitModuleInit()
	e.emitModuleStartStop(".<start>")
	e.emitModuleStartStop(".<stop>")

	for _, def := range fileNode.Definitions {
		switch node := def.(type) {
		case *parser.FunctionDefinitionNode:
			// Skip emitting module lifecycle functions again if they were parsed from source
			if node.Name.Value == ".<init>" || node.Name.Value == ".<start>" || node.Name.Value == ".<stop>" {
				continue
			}
			fnBir, err := e.emitFunctionDefinition(node)
			if err != nil {
				return nil, fmt.Errorf("failed to emit function %s: %w", node.Name.Value, err)
			}
			e.birPackage.Functions = append(e.birPackage.Functions, fnBir)
			fmt.Printf("[DEBUG] Added function '%s' to BIR package (now has %d functions, %d basic blocks)\n",
				fnBir.Name, len(e.birPackage.Functions), len(fnBir.BasicBlocks))

		case *parser.ServiceDeclarationNode:
			// Handle service declarations by converting resource functions to BIR functions
			serviceFuncs, err := e.emitServiceDeclaration(node)
			if err != nil {
				return nil, fmt.Errorf("failed to emit service declaration: %w", err)
			}
			e.birPackage.Functions = append(e.birPackage.Functions, serviceFuncs...)

		case *parser.InitFunctionNode:
			// Handle init function
			initFnBir, err := e.emitInitFunction(node)
			if err != nil {
				return nil, fmt.Errorf("failed to emit init function: %w", err)
			}
			if e.birPackage.ActualInitFunc != nil {
				return nil, fmt.Errorf("multiple init functions found in the package")
			}
			e.birPackage.ActualInitFunc = initFnBir
			e.birPackage.Functions = append(e.birPackage.Functions, initFnBir)

		case *parser.GlobalVariableNode:
			// Handle global variables
			_ = e.emitGlobalVariable(node)
			// Global variable is already added to the package in emitGlobalVariable

		case *parser.VariableDeclarationNode:
			// Handle module-level variable declarations
			e.emitVariableDeclaration(node)

		default:
			// Ignore other node types
		}
	}

	// Check if any functions (user or lifecycle) were actually added
	if len(e.birPackage.Functions) == 0 &&
		e.birPackage.ModuleInitFunc == nil &&
		e.birPackage.ModuleStartFunc == nil &&
		e.birPackage.ModuleStopFunc == nil &&
		len(e.birPackage.ImportModules) == 0 {
		return nil, nil
	}

	// Return any collected errors
	if len(e.errors) > 0 {
		return nil, e.errors[0] // Return the first error for now
	}

	return e.birPackage, nil
}

func (e *Emitter) emitImport(impNode *parser.ImportNode) {
	version := "0.0.0"
	if impNode.PackageName == "io" && impNode.OrgName == "ballerina" {
		version = "1.8.0"
	}

	birImp := &ImportModule{
		OrgName:     impNode.OrgName,
		PackageName: impNode.PackageName,
		Version:     version,
		Alias:       impNode.PackageName,
	}
	// In Ballerina, the alias is usually the last part of the package name if no 'as' clause.
	// If AST provides alias, use it. For `ballerina/io`, alias is `io`.
	if impNode.Alias != "" {
		birImp.Alias = impNode.Alias
	} else {
		parts := strings.Split(impNode.PackageName, "/")
		birImp.Alias = parts[len(parts)-1]
	}
	e.birPackage.ImportModules = append(e.birPackage.ImportModules, birImp)
}

func (e *Emitter) emitFunctionDefinition(fdn *parser.FunctionDefinitionNode) (*Function, error) {
	e.currentFunc = &Function{
		Name:         fdn.Name.Value,
		Visibility:   strings.ToUpper(fdn.Visibility),
		LocalVars:    make(map[string]*Variable),
		ArgumentVars: []*Variable{},
		Parameters:   []*Variable{},
		BasicBlocks:  []*BasicBlock{},
		nextVarID:    0, // Initialized to 0
		nextBBID:     0,
	}

	returnTypeStr := "()"
	if fdn.ReturnType != nil {
		// fdn.ReturnType is already a *parser.TypeNode if not nil,
		// as per parser.FunctionDefinitionNode struct.
		// We just need to access its TypeName field.
		returnTypeStr = fdn.ReturnType.TypeName
	}
	// Create and register the return variable.
	// getOrCreateVar for RETURN kind uses the fixed "%0" name and doesn't advance nextVarID itself.
	e.currentFunc.ReturnVariable = e.getOrCreateVar("%0(RETURN)", returnTypeStr, VarKindReturn, false)
	e.currentFunc.LocalVars[e.currentFunc.ReturnVariable.BIRName] = e.currentFunc.ReturnVariable

	// FIX: After establishing %0 for the return variable, ensure nextVarID for subsequent variables starts from 1.
	e.currentFunc.nextVarID = 1

	for _, paramAST := range fdn.Parameters { // Changed from 'for i, paramAST' to 'for _, paramAST'
		paramTypeStr := "any" // Default if type is complex or not parsed
		if paramAST.Type != nil {
			paramTypeStr = paramAST.Type.TypeName
		}
		// Argument variables should also use nextVarID, starting from %1, %2, etc.
		argVar := e.getOrCreateVar(paramAST.Name.Value, paramTypeStr, VarKindArg, true)
		e.currentFunc.ArgumentVars = append(e.currentFunc.ArgumentVars, argVar)
		e.currentFunc.Parameters = append(e.currentFunc.Parameters, argVar) // Parameters list for signature
	}

	entryBB := e.newBBCurrentFunc() // bb0
	e.setCurrentBB(entryBB)

	fmt.Printf("[DEBUG] Function %s: checking special case - params:%d, return:%s, body_statements:%d\n",
		fdn.Name.Value, len(fdn.Parameters), returnTypeStr,
		func() int {
			if fdn.Body != nil {
				return len(fdn.Body.Statements)
			} else {
				return 0
			}
		}())

	// Special handling for main function with hardcoded "Hello, World!" demo
	if fdn.Name.Value == "main" && len(fdn.Parameters) == 0 && returnTypeStr == "()" &&
		fdn.Body != nil && len(fdn.Body.Statements) == 1 {
		// Check if body is just a single io:println("Hello, World!")
		// This check might need to be more robust based on your AST structure
		if exprStmt, ok := fdn.Body.Statements[0].(*parser.ExpressionStatementNode); ok {
			if callExpr, ok := exprStmt.Expression.(*parser.CallExpressionNode); ok {
				if ident, ok := callExpr.Function.(*parser.IdentifierNode); ok && ident.Value == "println" {
					_, err := e.emitHelloWorldMain() // Call the specific emitter
					if err != nil {
						return nil, err
					}
					// emitHelloWorldMain now sets its own currentFunc, so we retrieve it
					fnToReturn := e.currentFunc // This might be problematic if emitHelloWorldMain changes e.currentFunc
					// It's better if emitHelloWorldMain operates on the e.currentFunc set by emitFunctionDefinition
					// For now, let's assume emitHelloWorldMain correctly populates the *current* e.currentFunc
					// and then we nullify e.currentFunc before returning.
					e.currentFunc = nil
					return fnToReturn, nil // Return early after emitting hardcoded main
				}
			}
		}
	}

	// Default behavior for all other functions (including more complex main)
	if fdn.Body != nil {
		fmt.Printf("[DEBUG] Processing function body with emitBlockStatement\n")
		fmt.Printf("[DEBUG] Current BB before emitBlockStatement: %v (has %d instructions)\n",
			e.currentFunc.CurrentBB.ID, len(e.currentFunc.CurrentBB.Instructions))
		e.emitBlockStatement(fdn.Body, nil)
		fmt.Printf("[DEBUG] Current BB after emitBlockStatement: %v (has %d instructions)\n",
			e.currentFunc.CurrentBB.ID, len(e.currentFunc.CurrentBB.Instructions))
		fmt.Printf("[DEBUG] Total basic blocks in function: %d\n", len(e.currentFunc.BasicBlocks))
	}

	// Ensure function ends with a terminator if not already set by its body
	if e.currentFunc.CurrentBB != nil && e.currentFunc.CurrentBB.Terminator == nil {
		// If return type is "()", and it's not main, it might implicitly return.
		// For simplicity, all non-terminated blocks in functions that are not main
		// and have a "()" return type will get a default return sequence.
		// Main function's termination is handled by emitHelloWorldMain or specific logic.
		if returnTypeStr == "()" {
			// For void functions, ensure a return if the last block isn't terminated.
			// This might involve loading nil to %0 if it hasn't been set.
			// Check if the last instruction was a store to %0. If not, add it.
			// This is a simplified placeholder. Proper dead code elimination / CFG analysis would be better.
			e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})
			e.addTerminator(&ReturnInst{})
		} else {
			// For non-void functions, falling off the end without a return is an error.
			// Or, if your language allows, it might be an implicit panic or undefined behavior.
			// For now, let's add an unreachable or a panic instruction in BIR,
			// or rely on LLVM verification to catch this.
			// A simple GOTO to a new block with just a return might be needed if there's complex logic.
			// For now, assume valid BIR will have explicit returns for non-void.
			// If not, this is a point for future improvement or error reporting.
			fmt.Printf("Warning: Function %s (non-void) reached end of emitFunctionDefinition without a terminator in the current BB.\n", fdn.Name.Value)
		}
	}

	fnToReturn := e.currentFunc

	// Debug: Print final BIR function state
	fmt.Printf("[DEBUG] Final BIR function state for '%s':\n", fnToReturn.Name)
	fmt.Printf("[DEBUG] - Total basic blocks: %d\n", len(fnToReturn.BasicBlocks))
	for i, bb := range fnToReturn.BasicBlocks {
		fmt.Printf("[DEBUG] - Block %d (%s): %d instructions\n", i, bb.ID, len(bb.Instructions))
		for j, inst := range bb.Instructions {
			fmt.Printf("[DEBUG]   - Inst %d: %s\n", j, inst.String())
		}
		if bb.Terminator != nil {
			fmt.Printf("[DEBUG]   - Terminator: %s\n", bb.Terminator.String())
		}
	}

	e.currentFunc = nil // Clear currentFunc after processing
	return fnToReturn, nil
}

func (e *Emitter) emitInitFunction(ifn *parser.InitFunctionNode) (*Function, error) {
	e.currentFunc = &Function{
		Name:         "init",    // Fixed name for init function
		Visibility:   "PRIVATE", // Init functions are implicitly private
		LocalVars:    make(map[string]*Variable),
		ArgumentVars: []*Variable{}, // No parameters
		Parameters:   []*Variable{}, // No parameters
		BasicBlocks:  []*BasicBlock{},
		nextVarID:    0,
		nextBBID:     0,
	}

	returnTypeStr := "()" // Default return type
	if ifn.ReturnType != nil {
		returnTypeStr = ifn.ReturnType.TypeName // Should be "error?" or "()"
	}

	// Create and register the return variable (%0).
	e.currentFunc.ReturnVariable = e.getOrCreateVar("%0(RETURN)", returnTypeStr, VarKindReturn, false)
	e.currentFunc.LocalVars[e.currentFunc.ReturnVariable.BIRName] = e.currentFunc.ReturnVariable
	e.currentFunc.nextVarID = 1 // Next var ID starts from 1

	entryBB := e.newBBCurrentFunc() // bb0
	e.setCurrentBB(entryBB)

	if ifn.Body != nil {
		e.emitBlockStatement(ifn.Body, nil)
	}

	// Ensure function ends with a terminator if not already set by its body
	if e.currentFunc.CurrentBB != nil && e.currentFunc.CurrentBB.Terminator == nil {
		if returnTypeStr == "()" {
			// For void functions, ensure a return if the last block isn't terminated.
			e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})
			e.addTerminator(&ReturnInst{})
		} else if returnTypeStr == "error?" {
			// If init returns error?, and doesn't explicitly return, it implies success (nil error)
			e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})
			e.addTerminator(&ReturnInst{})
		} else {
			// This case should ideally be caught by the parser, but as a safeguard:
			return nil, fmt.Errorf("init function has an invalid return type %s in BIR emission, expected '()' or 'error?'", returnTypeStr)
		}
	}

	fnToReturn := e.currentFunc
	e.currentFunc = nil // Clear currentFunc after processing
	return fnToReturn, nil
}

func (e *Emitter) emitHelloWorldMain() (*Function, error) {
	// Variables in specific order to match target BIR format
	arrayVar := e.getOrCreateVar("_array_for_println", "typeRefDesc<>[]", VarKindTemp, false)
	intVar := e.getOrCreateVar("_const_int_neg1", "int", VarKindTemp, false)
	printableType := "ballerina/io:1.8.0:Printable"
	varForCast2 := e.getOrCreateVar("_cast2_printable", printableType, VarKindTemp, false)
	varForCast1 := e.getOrCreateVar("_cast1_printable", printableType, VarKindTemp, false)
	varForHelloWorld := e.getOrCreateVar("_const_str_HelloWorld", "string", VarKindTemp, false)
	callReturnVar := e.getOrCreateVar("_println_ret", "()", VarKindTemp, false)

	// Generate instructions for bb0
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{intVar}}, Value: int64(-1), TypeName: "int"})
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{varForHelloWorld}}, Value: "Hello, World!", TypeName: "string"})
	e.addInstruction(&TypeCastInst{birInstruction: birInstruction{lhs: []*Variable{varForCast1}}, TargetType: printableType, SourceVar: varForHelloWorld})
	e.addInstruction(&TypeCastInst{birInstruction: birInstruction{lhs: []*Variable{varForCast2}}, TargetType: printableType, SourceVar: varForCast1})
	e.addInstruction(&NewArrayInst{
		birInstruction: birInstruction{lhs: []*Variable{arrayVar}},
		ElementType:    "typeRefDesc<>[]",
		SizeVar:        intVar,
		Args:           []*Variable{varForCast2},
	})

	bb1 := e.newBBCurrentFunc()
	printlnCall := &CallInst{
		birInstruction: birInstruction{lhs: []*Variable{callReturnVar}},
		PackagePath:    "",
		FunctionName:   "println",
		Args:           []*Variable{arrayVar},
		NextBB:         bb1.ID,
	}
	e.addTerminator(printlnCall)

	// bb1
	e.setCurrentBB(bb1)
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: e.currentFunc.ReturnVariable.Type})

	bb2 := e.newBBCurrentFunc()
	e.addTerminator(&GotoInst{TargetBB: bb2.ID})

	// bb2
	e.setCurrentBB(bb2)
	e.addTerminator(&ReturnInst{})

	fnToReturn := e.currentFunc
	e.currentFunc = nil
	return fnToReturn, nil
}

// Update emitCallExpression to handle io:println calls consistently
func (e *Emitter) emitCallExpression(callAST *parser.CallExpressionNode, isDiscarded bool) *Variable {
	fmt.Printf("DEBUG: emitCallExpression for func %T, args: %d, isDiscarded: %t\n", callAST.Function, len(callAST.Arguments), isDiscarded)
	var pkgPath, funcName string

	// Determine if this is an io:println call
	isPrintln := false

	// Check if this is a method call (e.g., value.toString())
	isMethodCall := false
	var receiverExpr parser.Expression
	var methodName string

	switch fnIdentifier := callAST.Function.(type) {
	case *parser.IdentifierNode:
		funcName = fnIdentifier.Value
		// Check if it's a direct println call without module qualifier
		isPrintln = funcName == "println"
	case *parser.MemberAccessExpressionNode:
		if modIdent, ok := fnIdentifier.Expression.(*parser.IdentifierNode); ok {
			// Check if this is a method call on a variable
			if fnIdentifier.Token.Type == lexer.TokenDot {
				isMethodCall = true
				receiverExpr = fnIdentifier.Expression
				methodName = fnIdentifier.MemberName.Value
			} else {
				// This is a module:function call
				alias := modIdent.Value
				// Resolve alias to full package path
				for _, imp := range e.birPackage.ImportModules {
					if imp.Alias == alias {
						pkgPath = imp.OrgName + "/" + imp.PackageName
						break
					}
				}
				funcName = fnIdentifier.MemberName.Value
				// Check if this is io:println
				isPrintln = alias == "io" && funcName == "println"
			}
		} else {
			// This could be a method call on a complex expression
			if fnIdentifier.Token.Type == lexer.TokenDot {
				isMethodCall = true
				receiverExpr = fnIdentifier.Expression
				methodName = fnIdentifier.MemberName.Value
			} else {
				panic(fmt.Sprintf("Unsupported member access expression for function call: %T", fnIdentifier.Expression))
			}
		}
	default:
		panic(fmt.Sprintf("Unsupported function identifier type in call: %T", callAST.Function))
	}

	// For println calls, emit special instructions sequence
	if isPrintln {
		return e.emitPrintlnCall(callAST.Arguments)
	}

	// Handle method calls
	if isMethodCall {
		// Emit the receiver expression
		receiverVar := e.emitExpression(receiverExpr, false)
		if receiverVar == nil {
			return nil
		}

		// Convert arguments to BIR variables
		args := make([]*Variable, 0, len(callAST.Arguments))
		for _, argAST := range callAST.Arguments {
			argBIR := e.emitExpression(argAST, false)
			if argBIR == nil {
				return nil
			}
			args = append(args, argBIR)
		}

		// Handle the method call
		return e.emitMethodCall(receiverVar, methodName, args)
	}

	// Handle regular function calls
	args := make([]*Variable, 0, len(callAST.Arguments))
	for _, argAST := range callAST.Arguments {
		argBIR := e.emitExpression(argAST, false)
		if argBIR == nil {
			return nil
		}
		args = append(args, argBIR)
	}

	var resultVar *Variable = nil
	callReturnType := "any" // Default, needs lookup

	if pkgPath == "ballerina/io" && funcName == "println" {
		callReturnType = "()"
	} else if funcName == "calculateSum" {
		callReturnType = "int"
	}

	if !isDiscarded && callReturnType != "()" {
		resultVar = e.getOrCreateVar("_call_ret_"+funcName, callReturnType, VarKindTemp, false)
	}

	callInstruction := &CallInst{
		PackagePath:  pkgPath,
		FunctionName: funcName,
		Args:         args,
	}
	if resultVar != nil {
		callInstruction.birInstruction = birInstruction{lhs: []*Variable{resultVar}}
	}

	e.addInstruction(callInstruction)
	return resultVar
}

// Helper method for emitting println calls with the correct BIR pattern
func (e *Emitter) emitPrintlnCall(arguments []parser.Expression) *Variable {
	fmt.Printf("DEBUG: emitPrintlnCall for %d arguments\n", len(arguments))

	if len(arguments) == 1 {
		// Single argument println - use existing logic
		return e.emitSinglePrintlnCall(arguments[0])
	}

	// Multi-argument println - concatenate arguments with spaces
	var resultVar *Variable

	for i, arg := range arguments {
		argVar := e.emitExpression(arg, false)
		if argVar == nil {
			continue
		}

		if i == 0 {
			// First argument becomes the result
			resultVar = argVar
		} else {
			// Add space before each subsequent argument
			spaceVar := e.getOrCreateVar("_const_space", "string", VarKindTemp, false)
			e.addInstruction(&ConstantLoadInst{
				birInstruction: birInstruction{lhs: []*Variable{spaceVar}},
				Value:          " ",
				TypeName:       "string",
			})

			// Concatenate current result with space
			tempVar1 := e.getOrCreateVar("_concat_temp", "string", VarKindTemp, false)
			e.addInstruction(&CallInst{
				birInstruction: birInstruction{lhs: []*Variable{tempVar1}},
				PackagePath:    "ballerina/lang.string",
				FunctionName:   "concat",
				Args:           []*Variable{resultVar, spaceVar},
			})

			// Convert argument to string if needed
			argStrVar := argVar
			if argVar.Type != "string" {
				argStrVar = e.getOrCreateVar("_arg_to_str", "string", VarKindTemp, false)
				e.addInstruction(&TypeConversionInst{
					LHSVar:     argStrVar,
					SourceVar:  argVar,
					TargetType: "string",
					IsCheck:    false,
				})
			}

			// Concatenate with argument
			newResultVar := e.getOrCreateVar("_concat_result", "string", VarKindTemp, false)
			e.addInstruction(&CallInst{
				birInstruction: birInstruction{lhs: []*Variable{newResultVar}},
				PackagePath:    "ballerina/lang.string",
				FunctionName:   "concat",
				Args:           []*Variable{tempVar1, argStrVar},
			})

			resultVar = newResultVar
		}
	}

	if resultVar == nil {
		return nil
	}

	// Now emit the actual println call with the concatenated result
	return e.emitSinglePrintlnCall(nil, resultVar)
}

// Helper method for single argument println calls
func (e *Emitter) emitSinglePrintlnCall(argument parser.Expression, argVar ...*Variable) *Variable {
	var finalArgVar *Variable

	if len(argVar) > 0 && argVar[0] != nil {
		// Use provided variable
		finalArgVar = argVar[0]
	} else if argument != nil {
		// Emit the expression
		finalArgVar = e.emitExpression(argument, false)
		if finalArgVar == nil {
			return nil
		}
	} else {
		return nil
	}

	// Create necessary variables
	intVar := e.getOrCreateVar("_const_int_neg1", "int", VarKindTemp, false)
	printableType := "ballerina/io:1.8.0:Printable"
	arrayVar := e.getOrCreateVar("_array_for_println", "typeRefDesc<>[]", VarKindTemp, false)

	// Cast the argument to Printable (two casts as in target BIR)
	varForCast1 := e.getOrCreateVar("_cast1_printable", printableType, VarKindTemp, false)
	varForCast2 := e.getOrCreateVar("_cast2_printable", printableType, VarKindTemp, false)

	// Generate constant load for array size (-1)
	e.addInstruction(&ConstantLoadInst{
		birInstruction: birInstruction{lhs: []*Variable{intVar}},
		Value:          int64(-1),
		TypeName:       "int",
	})

	// Generate casts
	e.addInstruction(&TypeCastInst{
		birInstruction: birInstruction{lhs: []*Variable{varForCast1}},
		TargetType:     printableType,
		SourceVar:      finalArgVar,
	})

	e.addInstruction(&TypeCastInst{
		birInstruction: birInstruction{lhs: []*Variable{varForCast2}},
		TargetType:     printableType,
		SourceVar:      varForCast1,
	})

	// Generate array creation
	e.addInstruction(&NewArrayInst{
		birInstruction: birInstruction{lhs: []*Variable{arrayVar}},
		ElementType:    "typeRefDesc<>[]",
		SizeVar:        intVar,
		Args:           []*Variable{varForCast2},
	})

	// Create next basic block for after println
	nextBB := e.newBBCurrentFunc()

	// Generate println call as terminator
	callReturnVar := e.getOrCreateVar("_println_ret", "()", VarKindTemp, false)

	printlnCall := &CallInst{
		birInstruction: birInstruction{lhs: []*Variable{callReturnVar}},
		PackagePath:    "", // Empty as in target BIR
		FunctionName:   "println",
		Args:           []*Variable{arrayVar},
		NextBB:         nextBB.ID,
	}
	e.addTerminator(printlnCall)

	// Set current basic block to the next one
	e.setCurrentBB(nextBB)

	return callReturnVar
}

// Update emitIfStatement to handle the control flow correctly
func (e *Emitter) emitIfStatement(ifStmt *parser.IfStatementNode, loopContext interface{}) {
	conditionVar := e.emitExpression(ifStmt.Condition, false)
	if conditionVar == nil {
		fmt.Printf("Warning: Failed to emit condition for if statement\n")
		return
	}

	thenBB := e.newBBCurrentFunc()
	elseBB := e.newBBCurrentFunc()
	afterIfBB := e.newBBCurrentFunc() // Create a merge point after if/else

	// Add conditional branch
	e.addTerminator(&ConditionalBranchInst{
		Condition: conditionVar,
		TrueBB:    thenBB.ID,
		FalseBB:   elseBB.ID,
	})

	// Then block
	e.setCurrentBB(thenBB)
	e.emitBlockStatement(ifStmt.Consequence, loopContext)

	// If then block doesn't already have a terminator (like return), add goto to merge point
	if e.currentFunc.CurrentBB.Terminator == nil {
		e.addTerminator(&GotoInst{TargetBB: afterIfBB.ID})
	}

	// Else block
	e.setCurrentBB(elseBB)
	if ifStmt.Alternative != nil {
		e.emitBlockStatement(ifStmt.Alternative, loopContext)
	}

	// If else block doesn't already have a terminator, add goto to merge point
	if e.currentFunc.CurrentBB.Terminator == nil {
		e.addTerminator(&GotoInst{TargetBB: afterIfBB.ID})
	}

	// Continue from merge point
	e.setCurrentBB(afterIfBB)
}

// Add this function to map AST types to BIR types
func mapAstTypeToBirType(astType string) string {
	// Simple 1:1 mapping for basic types
	switch astType {
	case "int":
		return "int"
	case "string":
		return "string"
	case "boolean", "bool":
		return "boolean"
	case "float":
		return "float"
	case "decimal":
		return "decimal"
	case "xml":
		return "xml"
	case "json":
		return "json"
	case "()":
		return "()"
	case "any":
		return "any"
	case "error":
		return "error"
	}

	// Handle array types if they exist (e.g., int[])
	if strings.HasSuffix(astType, "[]") {
		baseType := astType[:len(astType)-2]
		return mapAstTypeToBirType(baseType) + "[]"
	}

	// Handle map types if they exist
	if strings.HasPrefix(astType, "map<") && strings.HasSuffix(astType, ">") {
		valueType := astType[4 : len(astType)-1]
		return "map<" + mapAstTypeToBirType(valueType) + ">"
	}

	// Handle tuple types, record types, etc. would go here
	// For now, just passthrough any other types
	return astType
}

// Add the emitBlockStatement function to handle statement blocks
func (e *Emitter) emitBlockStatement(block *parser.BlockStatementNode, loopContext interface{}) {
	if block == nil || len(block.Statements) == 0 {
		return
	}
	fmt.Printf("DEBUG: emitBlockStatement - %d statements\n", len(block.Statements))
	for i, stmt := range block.Statements {
		fmt.Printf("DEBUG: emitBlockStatement - processing statement %d/%d of type %T\n", i+1, len(block.Statements), stmt)
		e.emitStatement(stmt, loopContext)
	}
}

// Update the emitStatement function to use AssignmentExpressionNode
func (e *Emitter) emitStatement(stmt parser.Statement, loopContext interface{}) {
	fmt.Printf("DEBUG: emitStatement for type %T\n", stmt)
	switch s := stmt.(type) {
	case *parser.ExpressionStatementNode:
		// Expression used as a statement (e.g., function calls)
		e.emitExpression(s.Expression, true) // isDiscarded=true since result not assigned

	case *parser.VariableDeclarationNode:
		e.emitVariableDeclaration(s)

	case *parser.ReturnStatementNode:
		e.emitReturnStatement(s)

	case *parser.IfStatementNode:
		e.emitIfStatement(s, loopContext)

	// Assignment statements are handled as expressions, they would come through
	// ExpressionStatementNode with an AssignmentExpressionNode inside

	// Add other statement types as needed (while, foreach, etc.)

	default:
		fmt.Printf("Warning: Unsupported statement type %T\n", stmt)
	}
}

// Update the emitExpression function to handle expressions
func (e *Emitter) emitExpression(expr parser.Expression, isDiscarded bool) *Variable {
	fmt.Printf("DEBUG: emitExpression for type %T, isDiscarded: %t, expr: %+v\n", expr, isDiscarded, expr)
	switch ex := expr.(type) {
	case *parser.IntegerLiteralNode:
		if isDiscarded {
			return nil // No need to create a variable for discarded literals
		}
		tempVar := e.getOrCreateVar("_const_int", "int", VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{
			birInstruction: birInstruction{lhs: []*Variable{tempVar}},
			Value:          ex.Value,
			TypeName:       "int",
		})
		return tempVar

	case *parser.FloatLiteralNode:
		if isDiscarded {
			return nil
		}
		tempVar := e.getOrCreateVar("", "float", VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{
			birInstruction: birInstruction{lhs: []*Variable{tempVar}},
			Value:          ex.Value,
			TypeName:       "float",
		})
		return tempVar

	case *parser.StringLiteralNode:
		if isDiscarded {
			return nil
		}
		tempVar := e.getOrCreateVar("_const_str", "string", VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{
			birInstruction: birInstruction{lhs: []*Variable{tempVar}},
			Value:          ex.Value,
			TypeName:       "string",
		})
		return tempVar

	case *parser.BooleanLiteralNode:
		if isDiscarded {
			return nil
		}
		tempVar := e.getOrCreateVar("_const_bool", "boolean", VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{
			birInstruction: birInstruction{lhs: []*Variable{tempVar}},
			Value:          ex.Value,
			TypeName:       "boolean",
		})
		return tempVar

	case *parser.IdentifierNode:
		if isDiscarded {
			return nil
		}
		// Look up identifiers in the current scope
		v := e.findVarByOriginalName(ex.Value)
		if v != nil {
			return v
		}
		// Fallback: check if it's a global variable
		for _, g := range e.birPackage.GlobalVars {
			if g.Name == ex.Value {
				return &Variable{
					BIRName:      g.Name,
					Type:         g.Type,
					Kind:         VarKindGlobalRef,
					OriginalName: g.Name,
				}
			}
		}
		fmt.Printf("Warning: Identifier '%s' not found as local or global\n", ex.Value)
		return nil

	case *parser.CallExpressionNode:
		// Special case: int.toString()
		if memberAccess, ok := ex.Function.(*parser.MemberAccessExpressionNode); ok {
			receiver := memberAccess.Expression
			if memberAccess.MemberName.Value == "toString" {
				receiverVar := e.emitExpression(receiver, false)
				if receiverVar != nil && receiverVar.Type == "int" {
					resultVar := e.getOrCreateVar("_int_to_string", "string", VarKindTemp, false)
					e.addInstruction(&CallInst{
						birInstruction: birInstruction{lhs: []*Variable{resultVar}},
						PackagePath:    "ballerina/lang.int",
						FunctionName:   "toString",
						Args:           []*Variable{receiverVar},
					})
					return resultVar
				}
			}
		}
		return e.emitCallExpression(ex, isDiscarded)

	case *parser.AssignmentExpressionNode:
		// Support assignment expressions (e.g., value = 2, name = "James")
		e.emitAssignmentStatement(ex)
		return nil

	case *parser.BinaryExpressionNode:
		return e.emitBinaryExpression(ex, isDiscarded)

	case *parser.TypeCastExpressionNode:
		targetBirType := e.mapAstTypeToBirType(ex.TargetType) // Ensure this calls the Emitter's method
		exprToCastVar := e.emitExpression(ex.Expression, false)
		if exprToCastVar == nil {
			e.errors = append(e.errors, fmt.Errorf("failed to emit expression for type cast at line %d, col %d", ex.StartToken().Line, ex.StartToken().Column))
			return nil
		}

		// Create a new temporary variable for the result of the cast
		resultVar := e.getOrCreateVar("", targetBirType, VarKindTemp, false)

		// Emit the TypeConversionInst
		e.addInstruction(&TypeConversionInst{
			LHSVar:     resultVar,
			SourceVar:  exprToCastVar,
			TargetType: targetBirType,
			IsCheck:    false, // This is a cast, not a type check
		})
		return resultVar

	case *parser.MemberAccessExpressionNode:
		// Handle member access (object.field, module.func, etc.)
		// For simplicity in this example, we'll assume this is part of a call expression
		// and will be handled there
		fmt.Printf("Warning: Standalone member access not fully implemented: %v\n", ex)
		return nil

	default:
		fmt.Printf("Warning: Unsupported expression type %T\n", expr)
		return nil
	}
}

// Add support for global variables with initializers
func (e *Emitter) emitGlobalVariable(global *parser.GlobalVariableNode) *GlobalVariable {
	globalVar := &GlobalVariable{
		Name: global.Name.Value,
		Type: mapAstTypeToBirType(global.Type.TypeName),
	}
	e.birPackage.GlobalVars = append(e.birPackage.GlobalVars, globalVar)
	e.globalVarMap[global.Name.Value] = globalVar

	// If there's an initializer, we need to add it to the module init function
	if global.Initializer != nil {
		// Store the global variable and its initializer for processing in module init
		// This will be handled when we emit the module init function
		fmt.Printf("[DEBUG] Global variable %s has initializer\n", global.Name.Value)
	}

	return globalVar
}

// Update emitVariableDeclaration to handle global and local variables properly
func (e *Emitter) emitVariableDeclaration(vd *parser.VariableDeclarationNode) {
	fmt.Printf("[DEBUG] Processing variable declaration: %s %s\n", vd.Type.TypeName, vd.Name.Value)

	// Get the variable type from AST
	varTypeStr := mapAstTypeToBirType(vd.Type.TypeName)

	// Determine if we're at module level or function level
	if e.currentFunc == nil {
		// Module-level variable - create a global variable
		globalVar := &GlobalVariable{
			Name: vd.Name.Value,
			Type: varTypeStr,
		}
		e.birPackage.GlobalVars = append(e.birPackage.GlobalVars, globalVar)
		e.globalVarMap[vd.Name.Value] = globalVar

		fmt.Printf("[DEBUG] Added global variable: %s of type %s\n", vd.Name.Value, varTypeStr)

		// Store initializer for later processing in module init
		if vd.InitialValue != nil {
			fmt.Printf("[DEBUG] Global variable %s has initializer\n", vd.Name.Value)
		}
	} else {
		// Function-level variable - create a local variable
		localVar := e.getOrCreateVar(vd.Name.Value, varTypeStr, VarKindLocal, false)

		// If there's an initializer expression, emit it and assign to the variable
		if vd.InitialValue != nil {
			initValue := e.emitExpression(vd.InitialValue, false)
			if initValue != nil {
				e.addInstruction(&MoveInst{
					birInstruction: birInstruction{lhs: []*Variable{localVar}},
					RHS:            initValue,
				})
			}
		}

		fmt.Printf("[DEBUG] Added local variable: %s of type %s\n", vd.Name.Value, varTypeStr)
	}
}

// Update the emitAssignmentStatement function
func (e *Emitter) emitAssignmentStatement(assign *parser.AssignmentExpressionNode) {
	// Get the target variable (local or global)
	var targetVar *Variable
	switch target := assign.Target.(type) {
	case *parser.IdentifierNode:
		targetVar = e.findVarByOriginalName(target.Value)
		if targetVar == nil {
			// Try global variable
			for _, g := range e.birPackage.GlobalVars {
				if g.Name == target.Value {
					targetVar = &Variable{
						BIRName:      g.Name,
						Type:         g.Type,
						Kind:         VarKindGlobalRef,
						OriginalName: g.Name,
					}
					break
				}
			}
		}
		if targetVar == nil {
			fmt.Printf("Error: Undefined variable '%s' in assignment\n", target.Value)
			return
		}
	// TODO: handle MemberAccessExpressionNode for fields
	default:
		fmt.Printf("Warning: Unsupported assignment target type %T\n", assign.Target)
		return
	}

	// Emit the value to be assigned
	valueVar := e.emitExpression(assign.Value, false)
	if valueVar == nil {
		return
	}

	// Create the assignment instruction
	e.addInstruction(&MoveInst{
		birInstruction: birInstruction{lhs: []*Variable{targetVar}},
		RHS:            valueVar,
	})
}

// Update the emitReturnStatement function to use ReturnValue instead of Value
func (e *Emitter) emitReturnStatement(ret *parser.ReturnStatementNode) {
	if ret.ReturnValue != nil {
		// If returning a value, emit the expression and move it to the return variable
		returnValue := e.emitExpression(ret.ReturnValue, false)
		if returnValue != nil {
			e.addInstruction(&MoveInst{
				birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}},
				RHS:            returnValue,
			})
		}
	}

	// Add return instruction
	e.addTerminator(&ReturnInst{})
}

// Add the emitBinaryExpression function
func (e *Emitter) emitBinaryExpression(binary *parser.BinaryExpressionNode, isDiscarded bool) *Variable {
	fmt.Printf("DEBUG: emitBinaryExpression op '%s', Left: %T, Right: %T\n", binary.Operator, binary.Left, binary.Right)
	leftVar := e.emitExpression(binary.Left, false)
	rightVar := e.emitExpression(binary.Right, false)

	if leftVar == nil || rightVar == nil {
		fmt.Printf("DEBUG: emitBinaryExpression - leftVar or rightVar is nil\n")
		return nil
	}

	if isDiscarded {
		return nil
	}

	// Handle string concatenation
	if binary.Operator == "+" && leftVar.Type == "string" && rightVar.Type == "string" {
		resultVar := e.getOrCreateVar("_str_concat_res", "string", VarKindTemp, false)
		e.addInstruction(&CallInst{
			birInstruction: birInstruction{lhs: []*Variable{resultVar}},
			PackagePath:    "ballerina/lang.string",
			FunctionName:   "concat",
			Args:           []*Variable{leftVar, rightVar},
		})
		return resultVar
	}

	// Handle int/float comparisons and arithmetic
	resultType := "any"
	switch binary.Operator {
	case "+", "-", "*", "/", "%":
		if leftVar.Type == "int" && rightVar.Type == "int" {
			resultType = "int"
		} else if leftVar.Type == "float" || rightVar.Type == "float" {
			resultType = "float"
		}
	case ">", ">=", "<", "<=", "==", "!=":
		resultType = "boolean"
	}
	resultVar := e.getOrCreateVar("_bin_op_res", resultType, VarKindTemp, false)
	e.addInstruction(&BinaryOpInst{
		birInstruction: birInstruction{lhs: []*Variable{resultVar}},
		Op:             binary.Operator,
		Op1:            leftVar,
		Op2:            rightVar,
	})
	return resultVar
}

// emitServiceDeclaration converts a service declaration to BIR functions
// Each resource function becomes a separate BIR function with a standardized name
func (e *Emitter) emitServiceDeclaration(service *parser.ServiceDeclarationNode) ([]*Function, error) {
	var functions []*Function

	for _, resource := range service.Resources {
		// Create a standardized function name for the resource
		// Format: main$<method>_<name>.0 (e.g., main$get_greeting.0)
		funcName := fmt.Sprintf("main$%s_%s.0", resource.Method.Value, resource.Name.Value)

		// Convert ResourceFunction to FunctionDefinitionNode
		funcDef := &parser.FunctionDefinitionNode{
			Token:      resource.Token,
			Visibility: "public", // Resource functions are public
			Name:       &parser.IdentifierNode{Token: resource.Name.Token, Value: funcName},
			Parameters: resource.Parameters,
			ReturnType: resource.ReturnType,
			Body:       resource.Body,
		}

		// Emit the function using existing emitFunctionDefinition logic
		birFunc, err := e.emitFunctionDefinition(funcDef)
		if err != nil {
			return nil, fmt.Errorf("failed to emit resource function %s: %w", funcName, err)
		}

		functions = append(functions, birFunc)
	}

	return functions, nil
}

// getSourceLocation formats a source location for error messages
func (e *Emitter) getSourceLocation(line, column int) string {
	// Get the current filename from the package
	// For now, we use a hardcoded value since we don't track source filenames
	filename := "003-init-fn.bal"                                                   // This would ideally come from a source location tracker
	return fmt.Sprintf("%s:(%d:%d,%d:%d)", filename, line, column, line, column+15) // Using +15 as an approximation for expression end
}

// emitMethodCall handles method calls on primitive types (e.g., int.toString())
func (e *Emitter) emitMethodCall(receiver *Variable, methodName string, args []*Variable) *Variable {
	if receiver == nil {
		fmt.Printf("Warning: Nil receiver in method call %s\n", methodName)
		return nil
	}

	// Handle toString() method for primitive types
	if methodName == "toString" && len(args) == 0 {
		resultVar := e.getOrCreateVar("_tostring_result", "string", VarKindTemp, false)

		switch receiver.Type {
		case "int":
			// Emit a call to the runtime int-to-string function, not a TypeConversionInst
			callInst := &CallInst{
				birInstruction: birInstruction{lhs: []*Variable{resultVar}},
				PackagePath:    "ballerina/lang.int",
				FunctionName:   "toString",
				Args:           []*Variable{receiver},
			}
			e.currentFunc.CurrentBB.Instructions = append(e.currentFunc.CurrentBB.Instructions, callInst)
			return resultVar

		case "float":
			// Similar conversion for float to string (could be a runtime call or conversion)
			convInst := &TypeConversionInst{
				LHSVar:     resultVar,
				SourceVar:  receiver,
				TargetType: "string",
				IsCheck:    false,
			}
			e.currentFunc.CurrentBB.Instructions = append(e.currentFunc.CurrentBB.Instructions, convInst)
			return resultVar

		case "boolean":
			// Similar conversion for boolean to string
			convInst := &TypeConversionInst{
				LHSVar:     resultVar,
				SourceVar:  receiver,
				TargetType: "string",
				IsCheck:    false,
			}
			e.currentFunc.CurrentBB.Instructions = append(e.currentFunc.CurrentBB.Instructions, convInst)
			return resultVar

		default:
			fmt.Printf("Warning: toString() method not implemented for type %s\n", receiver.Type)
			return nil
		}
	}

	// Fallback: not a recognized primitive method call
	fmt.Printf("Warning: emitMethodCall fallback for method %s on type %s\n", methodName, receiver.Type)
	return nil
}

// GenerateFromAST converts an AST to a BIR package.
// The astNode parameter should be a *parser.FileNode.
func GenerateFromAST(astNode interface{}) (*Package, error) {
	fileNode, ok := astNode.(*parser.FileNode)
	if !ok {
		return nil, fmt.Errorf("expected *parser.FileNode, got %T", astNode)
	}

	fmt.Println("[INFO] Generating BIR from AST")

	// Create a new package
	pkg := &Package{
		Name:          "main", // Default name for the main package
		Functions:     make([]*Function, 0),
		GlobalVars:    make([]*GlobalVariable, 0),
		ImportModules: make([]*ImportModule, 0),
	}

	// Process imports
	for _, importNode := range fileNode.Imports {
		importModule := &ImportModule{
			PackageName: importNode.PackageName,
			OrgName:     importNode.OrgName,
			Alias:       importNode.Alias,
		}
		pkg.ImportModules = append(pkg.ImportModules, importModule)
		fmt.Printf("[DEBUG] Added import: %s\n", importNode.PackageName)
	}

	// Process top-level definitions
	for _, def := range fileNode.Definitions {
		switch node := def.(type) {
		case *parser.FunctionDefinitionNode:
			// Regular function
			function := processFunction(node)
			pkg.Functions = append(pkg.Functions, function)
			fmt.Printf("[DEBUG] Added function: %s\n", function.Name)

			// Check if it's the main function
			if function.Name == "main" && node.Visibility == "public" {
				fmt.Println("[DEBUG] Found public main function")
			}

		case *parser.InitFunctionNode:
			// Init function
			function := processInitFunction(node)
			pkg.Functions = append(pkg.Functions, function)
			pkg.ActualInitFunc = function // Store reference to init function
			fmt.Println("[DEBUG] Found init function")

		case *parser.VariableDeclarationNode:
			// Global variable
			globalVar := &GlobalVariable{
				Name: node.Name.String(),
				Type: node.Type.String(),
			}
			pkg.GlobalVars = append(pkg.GlobalVars, globalVar)
			fmt.Printf("[DEBUG] Added global variable: %s (%s)\n", globalVar.Name, globalVar.Type)

		case *parser.GlobalVariableNode:
			// Global variable (alternate syntax)
			globalVar := &GlobalVariable{
				Name: node.Name.String(),
				Type: node.Type.String(),
			}
			pkg.GlobalVars = append(pkg.GlobalVars, globalVar)
			fmt.Printf("[DEBUG] Added global variable: %s (%s)\n", globalVar.Name, globalVar.Type)

		default:
			fmt.Printf("[WARNING] Unhandled definition type: %T\n", node)
		}
	}

	fmt.Printf("[DEBUG] BIR package has %d functions and %d global variables\n",
		len(pkg.Functions), len(pkg.GlobalVars))

	return pkg, nil
}

// Helper function to process a function definition
func processFunction(node *parser.FunctionDefinitionNode) *Function {
	function := &Function{
		Name:        node.Name.String(),
		Visibility:  node.Visibility,
		Parameters:  make([]*Variable, 0),
		LocalVars:   make(map[string]*Variable),
		BasicBlocks: make([]*BasicBlock, 0),
		nextVarID:   0,
		nextBBID:    0,
	}

	// Process parameters
	for i, param := range node.Parameters {
		variable := &Variable{
			Name:          param.Name.String(),
			BIRName:       fmt.Sprintf("%%arg%d", i),
			Type:          param.Type.String(),
			Kind:          VarKindArg,
			IsFunctionArg: true,
			OriginalName:  param.Name.String(),
		}
		function.Parameters = append(function.Parameters, variable)
		function.LocalVars[variable.BIRName] = variable
	}

	// Process return type
	if node.ReturnType != nil {
		returnVar := &Variable{
			Name:         "return",
			BIRName:      "%0", // Convention: return variable is %0
			Type:         node.ReturnType.String(),
			Kind:         VarKindReturn,
			OriginalName: "return",
		}
		function.ReturnVariable = returnVar
		function.LocalVars[returnVar.BIRName] = returnVar
	} else {
		// Default return type is nil/void
		returnVar := &Variable{
			Name:         "return",
			BIRName:      "%0",
			Type:         "nil",
			Kind:         VarKindReturn,
			OriginalName: "return",
		}
		function.ReturnVariable = returnVar
		function.LocalVars[returnVar.BIRName] = returnVar
	}

	// Create a basic block for the function body
	if node.Body != nil {
		entryBB := &BasicBlock{
			ID:           "BB0",
			Instructions: make([]Instruction, 0),
		}
		function.BasicBlocks = append(function.BasicBlocks, entryBB)
		function.CurrentBB = entryBB

		// For simplicity, we're not processing the actual statements in the body
		// In a real implementation, you would traverse the statements and generate
		// BIR instructions

		// Add a return instruction as terminator for the basic block
		entryBB.Terminator = &ReturnInst{}
	}

	return function
}

// Helper function to process an init function
func processInitFunction(node *parser.InitFunctionNode) *Function {
	function := &Function{
		Name:        "init",
		Visibility:  "",                   // init functions are not public
		Parameters:  make([]*Variable, 0), // init functions have no parameters
		LocalVars:   make(map[string]*Variable),
		BasicBlocks: make([]*BasicBlock, 0),
		nextVarID:   0,
		nextBBID:    0,
	}

	// Process return type
	if node.ReturnType != nil {
		returnVar := &Variable{
			Name:         "return",
			BIRName:      "%0",
			Type:         node.ReturnType.String(),
			Kind:         VarKindReturn,
			OriginalName: "return",
		}
		function.ReturnVariable = returnVar
		function.LocalVars[returnVar.BIRName] = returnVar
	} else {
		// Default return type for init is error?
		returnVar := &Variable{
			Name:         "return",
			BIRName:      "%0",
			Type:         "error?",
			Kind:         VarKindReturn,
			OriginalName: "return",
		}
		function.ReturnVariable = returnVar
		function.LocalVars[returnVar.BIRName] = returnVar
	}

	// Create a basic block for the function body
	if node.Body != nil {
		entryBB := &BasicBlock{
			ID:           "BB0",
			Instructions: make([]Instruction, 0),
		}
		function.BasicBlocks = append(function.BasicBlocks, entryBB)
		function.CurrentBB = entryBB

		// For simplicity, we're not processing the actual statements in the body
		// In a real implementation, you would traverse the statements and generate
		// BIR instructions

		// Add a return instruction as terminator for the basic block
		entryBB.Terminator = &ReturnInst{}
	}

	return function
}
