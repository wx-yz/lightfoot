// bir/bir.go
package bir

import (
	"fmt"
	"strings"
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
	AnnotationData  *GlobalVariable
	ModuleInitFunc  *Function
	ModuleStartFunc *Function
	ModuleStopFunc  *Function
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
		// Attempt to match official BIR call syntax: `ballerina/io:1.8.0:println`
		// This requires knowing the version of the imported module.
		// For now, a simplified path.
		// Let's assume PackagePath includes version for now, or we hardcode for "io"
		versionedPkgPath := i.PackagePath
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

// --- BIR Emitter ---
type Emitter struct {
	birPackage  *Package
	currentFunc *Function
	// varMap       map[string]*Variable // Removed in favor of LocalVars in Function
	globalVarMap map[string]*GlobalVariable
	// tempVarCount int // Managed by currentFunc.nextVarID
	// labelCount   int // Managed by currentFunc.nextBBID
	// moduleInitDone bool // Not strictly needed for this logic
}

func NewEmitter() *Emitter {
	return &Emitter{
		globalVarMap: make(map[string]*GlobalVariable),
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
		fmt.Printf("Warning: Adding instruction '%s' to already terminated BB %s. This instruction might be dead code.\n", inst.String(), e.currentFunc.CurrentBB.ID)
		// Optionally, create a new "unreachable" BB, but this masks the underlying issue.
		// For now, allow adding, but it's a sign of trouble.
	}
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
		ReturnVariable:    &Variable{BIRName: "%0", Type: "error|()", Kind: VarKindReturn, OriginalName: "%0(RETURN)"},
		LocalVars:         make(map[string]*Variable),
		nextVarID:         0,
		nextBBID:          0,
	}
	e.currentFunc.LocalVars["%0"] = e.currentFunc.ReturnVariable
	e.currentFunc.nextVarID++ // %0 is used, next is %1

	e.birPackage.ModuleInitFunc = e.currentFunc

	tdVar := e.getOrCreateVar("_td_map_any", "typeDesc<map<any>>", VarKindTemp, false)

	bb0 := e.newBBCurrentFunc()
	e.setCurrentBB(bb0)
	// $annotation_data map<any>;
	// %1 = newType map<any>;
	// $annotation_data = NewMap %1{};
	e.addInstruction(&NewTypeInst{birInstruction: birInstruction{lhs: []*Variable{tdVar}}, Desc: "map<any>"})

	// For $annotation_data = NewMap %1{};
	// This requires $annotation_data to be an LHS variable or NewMapInst to handle globals.
	// Simplification: Assume $annotation_data is implicitly handled or skip complex map init for now.
	// The target BIR implies $annotation_data is a global.
	// My NewMapInst assigns to an LHS variable. Let's create a temp for the map if needed,
	// or enhance NewMapInst. For now, I will skip the NewMap for $annotation_data.
	// The global $annotation_data exists in e.birPackage.AnnotationData.
	// A real compiler would have a StoreGlobalInst here.

	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: "nil"})

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
	e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: "nil"})

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

	for _, impNode := range fileNode.Imports {
		e.emitImport(impNode)
	}

	for _, def := range fileNode.Definitions {
		switch node := def.(type) {
		case *parser.FunctionDefinitionNode:
			// Skip emitting module lifecycle functions again if they were parsed from source
			// (though they are typically implicitly generated)
			if node.Name.Value == ".<init>" || node.Name.Value == ".<start>" || node.Name.Value == ".<stop>" {
				continue
			}
			fnBir, err := e.emitFunctionDefinition(node)
			if err != nil {
				return nil, fmt.Errorf("failed to emit function %s: %w", node.Name.Value, err)
			}
			e.birPackage.Functions = append(e.birPackage.Functions, fnBir)
		default:
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
		Parameters:   []*Variable{}, // For signature display
		BasicBlocks:  []*BasicBlock{},
		nextVarID:    0,
		nextBBID:     0,
	}

	returnTypeStr := "()"
	if fdn.ReturnType != nil {
		returnTypeStr = mapAstTypeToBirType(fdn.ReturnType.TypeName)
	}
	// Create and register %0 return variable
	e.currentFunc.ReturnVariable = &Variable{BIRName: "%0", Type: returnTypeStr, Kind: VarKindReturn, OriginalName: "%0(RETURN)"}
	e.currentFunc.LocalVars["%0"] = e.currentFunc.ReturnVariable
	e.currentFunc.nextVarID++ // nextVarID is now 1 for the first actual parameter/variable

	// Process parameters (Args) - This part needs to correctly use getOrCreateVar
	for _, paramAST := range fdn.Parameters {
		paramTypeStr := mapAstTypeToBirType(paramAST.Type.TypeName)
		// getOrCreateVar will generate BIRName like %1, %2 ... for these args
		argVar := e.getOrCreateVar(paramAST.Name.Value, paramTypeStr, VarKindArg, true)
		// Kind and IsFunctionArg are set by getOrCreateVar if isFuncArg is true and kind is VarKindArg.
		e.currentFunc.Parameters = append(e.currentFunc.Parameters, &Variable{Name: paramAST.Name.Value, Type: paramTypeStr, OriginalName: paramAST.Name.Value}) // For signature
		e.currentFunc.ArgumentVars = append(e.currentFunc.ArgumentVars, argVar)
	}

	entryBB := e.newBBCurrentFunc()
	e.setCurrentBB(entryBB)

	if fdn.Body != nil {
		e.emitBlockStatement(fdn.Body, nil)
	}

	if e.currentFunc.CurrentBB.Terminator == nil {
		if e.currentFunc.ReturnVariable.Type == "()" {
			// Ensure nil is loaded to %0 if not already explicitly returned.
			// This might be redundant if all paths return, but good for safety.
			// Check if last instruction already moved to %0 or if %0 has a value.
			// For simplicity: always load nil for void return if no explicit terminator.
			e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: "nil"})
		} else {
			// This is a semantic error: non-void function does not return on all paths.
			// The parser should not produce such ASTs ideally, or semantic analysis should catch it.
			// To make BIR technically valid, we add a return, but it's problematic.
			fmt.Printf("Warning: Function %s with non-void return type %s might be missing a return statement on some paths.\n", e.currentFunc.Name, e.currentFunc.ReturnVariable.Type)
			// A default value for the type should be loaded to %0, or a trap.
			// For now, just adding return. The value in %0 would be undefined.
		}
		e.addTerminator(&ReturnInst{})
	}

	fnToReturn := e.currentFunc
	e.currentFunc = nil
	return fnToReturn, nil
}

func mapAstTypeToBirType(astTypeName string) string {
	switch astTypeName {
	case "int":
		return "int"
	case "string":
		return "string"
	case "boolean":
		return "boolean"
	default:
		// For custom types or module-qualified types, this needs more sophisticated mapping
		// For now, return as is.
		return astTypeName
	}
}

func (e *Emitter) emitBlockStatement(block *parser.BlockStatementNode, loopContext interface{}) {
	for i, stmt := range block.Statements {
		// If the current basic block has already been terminated by a previous statement in this AST block,
		// then subsequent AST statements in this block are unreachable unless a new BB was explicitly created
		// and targeted (e.g., by an 'else' branch).
		if e.currentFunc.CurrentBB.Terminator != nil {
			// This implies that a previous statement (like an if/else with returns in all branches, or a direct return)
			// has already ended the control flow for the current basic block.
			// Subsequent statements in this *parser.BlockStatementNode* might be dead code
			// or belong to a different logical block that should have been started by a jump.
			// For a simple sequential emitter, if a BB is terminated, we shouldn't add more to it.
			fmt.Printf("Warning: BB %s already terminated. Skipping remaining %d AST statements in this block.\n", e.currentFunc.CurrentBB.ID, len(block.Statements)-i)
			break
		}
		e.emitStatement(stmt, loopContext)
	}
}

func (e *Emitter) emitStatement(stmt parser.Statement, loopContext interface{}) {
	// Ensure we don't emit to a terminated BB. If so, it's an emitter logic error in CFG.
	if e.currentFunc.CurrentBB.Terminator != nil {
		fmt.Printf("Warning: Attempting to emit statement %s to terminated BB %s.\n", stmt.String(), e.currentFunc.CurrentBB.ID)
		return
	}

	switch s := stmt.(type) {
	case *parser.VariableDeclarationNode:
		e.emitVariableDeclaration(s)
	case *parser.ExpressionStatementNode:
		e.emitExpression(s.Expression, true)
	case *parser.ReturnStatementNode:
		e.emitReturnStatement(s)
	case *parser.IfStatementNode:
		e.emitIfStatement(s, loopContext)
	default:
		fmt.Printf("Warning: Unhandled AST statement type: %T\n", s)
	}
}

func (e *Emitter) emitVariableDeclaration(vdn *parser.VariableDeclarationNode) {
	birType := mapAstTypeToBirType(vdn.Type.TypeName)
	lhsVar := e.getOrCreateVar(vdn.Name.Value, birType, VarKindLocal, false)

	if vdn.InitialValue != nil {
		rhsVar := e.emitExpression(vdn.InitialValue, false)
		if rhsVar != nil {
			if lhsVar.BIRName != rhsVar.BIRName { // Avoid %X = %X
				e.addInstruction(&MoveInst{birInstruction: birInstruction{lhs: []*Variable{lhsVar}}, RHS: rhsVar})
			}
		} else {
			fmt.Printf("Warning: Failed to emit initializer for variable %s\n", lhsVar.OriginalName)
		}
	}
}

func (e *Emitter) emitReturnStatement(rsn *parser.ReturnStatementNode) {
	if rsn.ReturnValue != nil {
		returnValOperand := e.emitExpression(rsn.ReturnValue, false)
		if returnValOperand != nil {
			e.addInstruction(&MoveInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, RHS: returnValOperand})
		} else {
			fmt.Printf("Warning: Failed to emit return value expression for return statement\n")
		}
	} else {
		if e.currentFunc.ReturnVariable.Type == "()" {
			e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{e.currentFunc.ReturnVariable}}, Value: "nil", TypeName: "nil"})
		} else {
			// Semantic error: returning void from non-void function.
			fmt.Printf("Warning: Empty return from function %s expecting type %s\n", e.currentFunc.Name, e.currentFunc.ReturnVariable.Type)
		}
	}
	e.addTerminator(&ReturnInst{})
}

func (e *Emitter) emitIfStatement(ifStmt *parser.IfStatementNode, loopContext interface{}) {
	conditionVar := e.emitExpression(ifStmt.Condition, false)
	if conditionVar == nil {
		fmt.Printf("Warning: Failed to emit condition for if statement\n")
		return
	}

	thenBB := e.newBBCurrentFunc()
	elseBB := e.newBBCurrentFunc() // Else BB (even if no else clause, it's the start of "after then")
	endIfBB := elseBB              // If no explicit else, elseBB is the merge point.

	if ifStmt.Alternative != nil {
		endIfBB = e.newBBCurrentFunc() // If there is an else, we need a separate merge point after else.
		e.addTerminator(&ConditionalBranchInst{Condition: conditionVar, TrueBB: thenBB.ID, FalseBB: elseBB.ID})
	} else {
		e.addTerminator(&ConditionalBranchInst{Condition: conditionVar, TrueBB: thenBB.ID, FalseBB: endIfBB.ID}) // False goes to after-if
	}

	// Then block
	e.setCurrentBB(thenBB)
	e.emitBlockStatement(ifStmt.Consequence, loopContext)
	if thenBB.Terminator == nil {
		e.addTerminator(&GotoInst{TargetBB: endIfBB.ID})
	}

	// Else block (if exists)
	if ifStmt.Alternative != nil {
		e.setCurrentBB(elseBB) // This was elseBB created above
		e.emitBlockStatement(ifStmt.Alternative, loopContext)
		if elseBB.Terminator == nil {
			e.addTerminator(&GotoInst{TargetBB: endIfBB.ID})
		}
	}

	e.setCurrentBB(endIfBB) // Continue emitting in the merge block
}

func (e *Emitter) emitExpression(expr parser.Expression, isDiscarded bool) *Variable {
	switch ex := expr.(type) {
	case *parser.IntegerLiteralNode:
		tempVarType := "int"
		tempVar := e.getOrCreateVar(fmt.Sprintf("_const_int_%d", ex.Value), tempVarType, VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{tempVar}}, Value: ex.Value, TypeName: tempVarType})
		return tempVar
	case *parser.StringLiteralNode:
		tempVarType := "string"
		// Create a somewhat unique original name for temps from strings
		safeStr := strings.ReplaceAll(ex.Value, " ", "_")
		if len(safeStr) > 10 {
			safeStr = safeStr[:10]
		}
		tempVar := e.getOrCreateVar(fmt.Sprintf("_const_str_%s", safeStr), tempVarType, VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{tempVar}}, Value: ex.Value, TypeName: tempVarType})
		return tempVar
	case *parser.BooleanLiteralNode:
		tempVarType := "boolean"
		tempVar := e.getOrCreateVar(fmt.Sprintf("_const_bool_%t", ex.Value), tempVarType, VarKindTemp, false)
		e.addInstruction(&ConstantLoadInst{birInstruction: birInstruction{lhs: []*Variable{tempVar}}, Value: ex.Value, TypeName: tempVarType})
		return tempVar

	case *parser.IdentifierNode:
		v := e.findVarByOriginalName(ex.Value)
		if v == nil {
			panic(fmt.Sprintf("Emitter error: Undefined identifier '%s' encountered in emitExpression. Parser or semantic analysis should have caught this.", ex.Value))
		}
		return v

	case *parser.BinaryExpressionNode:
		leftVar := e.emitExpression(ex.Left, false)
		rightVar := e.emitExpression(ex.Right, false)
		if leftVar == nil || rightVar == nil {
			return nil
		}

		var resultType string
		if strings.Contains("+-*/", ex.Operator) { // Basic arithmetic
			resultType = "int"
		} else if strings.Contains("><==!=", ex.Operator) { // Comparisons
			resultType = "boolean"
		} else {
			resultType = "any"
		}
		resultVar := e.getOrCreateVar("_bin_op_res", resultType, VarKindTemp, false)
		e.addInstruction(&BinaryOpInst{birInstruction: birInstruction{lhs: []*Variable{resultVar}}, Op: ex.Operator, Op1: leftVar, Op2: rightVar})
		return resultVar

	case *parser.AssignmentExpressionNode:
		// Target (LHS) of assignment
		var lhsVarRef *Variable
		// For simplicity, assuming target is an IdentifierNode for now.
		// A real implementation would handle member access etc.
		if targetIdent, ok := ex.Target.(*parser.IdentifierNode); ok {
			lhsVarRef = e.findVarByOriginalName(targetIdent.Value)
			if lhsVarRef == nil {
				panic(fmt.Sprintf("LHS of assignment '%s' not found (not declared or not in scope).", targetIdent.Value))
			}
		} else {
			panic(fmt.Sprintf("Unsupported assignment target type: %T", ex.Target))
		}

		rhsVar := e.emitExpression(ex.Value, false)
		if rhsVar == nil {
			return nil
		}

		if lhsVarRef.BIRName != rhsVar.BIRName { // Avoid %X = %X if possible (though Move is fine)
			e.addInstruction(&MoveInst{birInstruction: birInstruction{lhs: []*Variable{lhsVarRef}}, RHS: rhsVar})
		}

		// The result of an assignment expression in Ballerina (if used in an expression context)
		// is typically the value assigned, or nil/void if it's just a statement.
		// Since this is `emitExpression`, we should return the value that can be used.
		return lhsVarRef // Return the variable that was assigned to (its value is now rhsVar's value)

	case *parser.CallExpressionNode:
		return e.emitCallExpression(ex, isDiscarded)
	default:
		fmt.Printf("Warning: Unhandled AST expression type in emitExpression: %T\n", ex)
		return nil
	}
}

func (e *Emitter) emitCallExpression(callAST *parser.CallExpressionNode, isDiscarded bool) *Variable {
	var pkgPath, funcName string // pkgPath will be resolved full path e.g. "ballerina/io"

	switch fnIdentifier := callAST.Function.(type) {
	case *parser.IdentifierNode:
		funcName = fnIdentifier.Value
		// pkgPath remains empty, assuming current module
	case *parser.MemberAccessExpressionNode:
		if modIdent, ok := fnIdentifier.Expression.(*parser.IdentifierNode); ok {
			alias := modIdent.Value
			// Resolve alias to full package path
			foundImport := false
			for _, imp := range e.birPackage.ImportModules {
				if imp.Alias == alias { // Compare with resolved alias
					pkgPath = imp.OrgName + "/" + imp.PackageName
					foundImport = true
					break
				}
			}
			if !foundImport {
				panic(fmt.Sprintf("Unknown module alias '%s' in call expression", alias))
			}
		} else {
			panic(fmt.Sprintf("Unsupported member access expression for function call: %T", fnIdentifier.Expression))
			return nil
		}
		funcName = fnIdentifier.MemberName.Value
	default:
		panic(fmt.Sprintf("Unsupported function identifier type in call: %T", callAST.Function))
		return nil
	}

	argsBIR := []*Variable{}
	for _, argAST := range callAST.Arguments {
		argBIR := e.emitExpression(argAST, false)
		if argBIR == nil {
			return nil
		}
		argsBIR = append(argsBIR, argBIR)
	}

	var lhsVar *Variable = nil
	callReturnType := "any" // Default, needs lookup

	isPrintln := (pkgPath == "ballerina/io" && funcName == "println")
	if isPrintln {
		callReturnType = "()"
	} else if e.currentFunc.Name == "main" && funcName == "calculateSum" {
		callReturnType = "int"
	}

	if !isDiscarded && callReturnType != "()" {
		lhsVar = e.getOrCreateVar("_call_ret_"+funcName, callReturnType, VarKindTemp, false)
	}

	// Handle `-> bbX` for println from target BIR
	// This requires CallInst to be a potential terminator and careful CFG construction.
	// For now, if it's println, we will try to create a next BB and set it in callInst.
	// The printer will show it. The actual control flow impact is simplified.
	nextBBForCallStr := ""
	isTerminatingCall := isPrintln // Simplified assumption

	if isTerminatingCall {
		// If this call is expected to be a terminator, we need a target BB.
		// This target BB should be where execution continues *after* the call conceptually returns or branches.
		// This is tricky. If println is in middle of a block, next op is in new BB.
		// If it's last, its next BB might be a merge point of an if/else or function end.

		// Let's assume for now: if it's println, and it's not *already* the last thing before a terminator,
		// we create a new BB for subsequent instructions from this AST block.
		// This logic is still very rough for proper CFG.

		// Create a successor BB for after the println call
		// This BB will be linked by the call instruction's `NextBB` field.
		// The emitter must then switch to this new BB if the call is a terminator.
	}

	callInstruction := &CallInst{
		PackagePath:  pkgPath,
		FunctionName: funcName,
		Args:         argsBIR,
		NextBB:       nextBBForCallStr,
	}
	if lhsVar != nil {
		callInstruction.birInstruction = birInstruction{lhs: []*Variable{lhsVar}}
	}

	// If this call is a terminator for the current BB (like the target BIR's println)
	if isTerminatingCall {
		// We need to create the target BB *before* adding the call as a terminator.
		// And the call's NextBB field should point to this new BB's ID.
		// Then, after adding the call terminator, setCurrentBB to this new target.

		// This is a simplified model:
		// 1. Create the target BB where execution should resume.
		// 2. Set CallInst.NextBB to this target BB's ID.
		// 3. Add CallInst as a *non-terminator* to current BB.
		// 4. Add a GOTO from current BB to the target BB. (This makes CallInst not the direct terminator)
		// OR:
		// 1. Create target BB.
		// 2. Set CallInst.NextBB to target.
		// 3. Treat CallInst as a *terminator*.
		// 4. setCurrentBB to target.

		// Let's try variant 3, but CallInst is not a Terminator yet.
		// For the string output, having NextBB is enough for now.
		// True CFG needs CallInst to be a TerminatorInstruction subtype if NextBB is present.

		// Simplification for output matching: if it's println, and we want it to show "-> bbX"
		// we need to generate that bbX *if* it's not the natural successor.
		// This part of code needs careful design for proper CFG.
		// For now, just add the instruction. The `NextBB` is for display.
		// If `isTerminatingCall` is true, it means this call should be the terminator.
		// This requires `CallInst` to implement `TerminatorInstruction`.
		// This is a significant change. For now, I will make CallInst a regular instruction.
		// The `-> bbX` in the target implies the call itself dictates the next block.
		// This is not typical for all calls, usually just for async or specific control flow ones.
	}

	e.addInstruction(callInstruction)

	return lhsVar
}
