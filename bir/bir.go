// bir/bir.go (Simplified BIR definitions)
package bir

import (
	"fmt"
	"strings"
	"wx-yz/lightfoot/parser" // For AST nodes
)

// --- BIR Core Structures ---

// Package represents a compiled Ballerina package in BIR form.
type Package struct {
	OrgName       string
	Name          string
	Version       string // e.g., "1.0.0"
	ImportModules []*ImportModule
	GlobalVars    []*GlobalVariable // Simplified
	Functions     []*Function
	// Constants, TypeDefinitions, Services, Listeners, etc. would go here
}

// ImportModule represents an imported module.
type ImportModule struct {
	OrgName     string
	PackageName string
	Alias       string
}

// GlobalVariable represents a global variable in BIR.
type GlobalVariable struct {
	Name string
	Type string // Simplified type representation (e.g., "int", "string")
	// Flags (public, final, etc.), InitialValue (reference to an instruction/constant)
}

// Function represents a Ballerina function in BIR.
type Function struct {
	Name         string
	Visibility   string      // "PUBLIC", "PRIVATE", "PACKAGE_PRIVATE"
	Flags        []string    // e.g., "NATIVE", "ISOLATED"
	Parameters   []*Variable // Parameters are also local variables
	ReturnType   string      // Simplified type, e.g. "int", "string", "()" for nil/void
	LocalVars    []*Variable // Includes parameters and other local vars
	Instructions []Instruction
	// ErrorTable, BasicBlocks, etc.
	currentScopeID int
	nextVarID      int
}

// Variable represents a variable (local, parameter, global ref) in BIR.
type Variable struct {
	ID      int     // Unique ID within the function for local vars
	Name    string  // Original variable name (for debug/BIR readability)
	Type    string  // Simplified type
	Kind    VarKind // Parameter, Local, Temp, Return, Self, etc.
	ScopeID int     // To handle scopes for variables
}

// VarKind differentiates kinds of variables.
type VarKind string

const (
	VarKindLocal     VarKind = "Local"
	VarKindParameter VarKind = "Parameter"
	VarKindTemp      VarKind = "Temp"     // For intermediate expression results
	VarKindReturn    VarKind = "Return"   // For return value operand
	VarKindGlobal    VarKind = "Global"   // Reference to a global var
	VarKindConstant  VarKind = "Constant" // For constants loaded
)

// --- BIR Instructions (Simplified Set) ---

// Instruction is the interface for all BIR instructions.
type Instruction interface {
	String() string // For printing/debugging
	// GetOperands(), GetLHS(), etc. for analysis and code gen
}

// TerminatorInstruction is a marker for instructions that end a basic block.
type TerminatorInstruction interface {
	Instruction
	IsTerminator() bool
}

// ConstantLoadInst loads a constant value into a variable.
type ConstantLoadInst struct {
	LHS      *Variable   // Variable to store the loaded constant
	Value    interface{} // int64, float64, string, bool, nil
	TypeName string      // "int", "string", "boolean", "nil"
}

func (i *ConstantLoadInst) String() string {
	return fmt.Sprintf("%s = const_load (%s) %v", i.LHS.Name, i.TypeName, i.Value)
}

// MoveInst moves a value from one variable to another.
type MoveInst struct {
	LHS *Variable // Destination
	RHS *Variable // Source
}

func (i *MoveInst) String() string { return fmt.Sprintf("%s = move %s", i.LHS.Name, i.RHS.Name) }

// BinaryOpInst performs a binary operation.
type BinaryOpInst struct {
	LHS *Variable // Variable to store the result
	Op  string    // e.g., "+", "-", "*", "/", "==", "<"
	Op1 *Variable // Left operand
	Op2 *Variable // Right operand
}

func (i *BinaryOpInst) String() string {
	return fmt.Sprintf("%s = %s %s %s", i.LHS.Name, i.Op1.Name, i.Op, i.Op2.Name)
}

// CallInst represents a function call.
type CallInst struct {
	LHS          *Variable // Variable to store the return value (can be nil)
	PackageAlias string    // e.g., "io" or "" for local call
	FunctionName string
	Args         []*Variable // Arguments passed to the function
}

func (i *CallInst) String() string {
	argsStr := []string{}
	for _, arg := range i.Args {
		argsStr = append(argsStr, arg.Name)
	}
	pkgPrefix := ""
	if i.PackageAlias != "" {
		pkgPrefix = i.PackageAlias + ":"
	}
	if i.LHS != nil && i.LHS.Name != "_" { // Allow discarding return value with '_'
		return fmt.Sprintf("%s = call %s%s(%s)", i.LHS.Name, pkgPrefix, i.FunctionName, strings.Join(argsStr, ", "))
	}
	return fmt.Sprintf("call %s%s(%s)", pkgPrefix, i.FunctionName, strings.Join(argsStr, ", "))
}

// ReturnInst returns from the current function.
type ReturnInst struct {
	// No explicit operand here; a prior instruction should move the return value
	// to a predefined return variable if the function has a non-nil return type.
	// For simplicity, we might attach it for debugging.
	ReturnVar *Variable // Optional: the variable holding the return value
}

func (i *ReturnInst) String() string {
	if i.ReturnVar != nil {
		return fmt.Sprintf("return %s", i.ReturnVar.Name)
	}
	return "return"
}
func (i *ReturnInst) IsTerminator() bool { return true }

// BranchInst represents a conditional or unconditional jump.
// For simplicity, we'll use labels as strings for now.
// In a real BIR, these would be BasicBlock pointers/IDs.
type BranchInst struct {
	TargetLabel string
}

func (i *BranchInst) String() string     { return fmt.Sprintf("br %s", i.TargetLabel) }
func (i *BranchInst) IsTerminator() bool { return true }

// CondBranchInst represents a conditional branch.
type CondBranchInst struct {
	Condition  *Variable // Boolean variable for condition
	TrueLabel  string
	FalseLabel string
}

func (i *CondBranchInst) String() string {
	return fmt.Sprintf("br %s ? %s : %s", i.Condition.Name, i.TrueLabel, i.FalseLabel)
}
func (i *CondBranchInst) IsTerminator() bool { return true }

// LabelInst is not an executable instruction but a marker for a label.
type LabelInst struct {
	Name string
}

func (i *LabelInst) String() string { return fmt.Sprintf("%s:", i.Name) }

// --- BIR Emitter ---

// Emitter converts AST to BIR.
type Emitter struct {
	birPackage *Package
	currentFn  *Function
	labelCount int // For generating unique labels
	// Potentially a symbol table here or passed around
}

// NewEmitter creates a new Emitter.
func NewEmitter() *Emitter {
	return &Emitter{}
}

func (e *Emitter) newLabel() string {
	e.labelCount++
	return fmt.Sprintf("L%d", e.labelCount)
}

func (e *Emitter) newTempVar(typeName string) *Variable {
	e.currentFn.nextVarID++
	v := &Variable{
		ID:      e.currentFn.nextVarID,
		Name:    fmt.Sprintf("_t%d", e.currentFn.nextVarID), // Temp var naming convention
		Type:    typeName,
		Kind:    VarKindTemp,
		ScopeID: e.currentFn.currentScopeID,
	}
	e.currentFn.LocalVars = append(e.currentFn.LocalVars, v)
	return v
}

func (e *Emitter) addInstruction(inst Instruction) {
	if e.currentFn == nil {
		// This should not happen if emitter is used correctly
		panic("attempting to add instruction outside a function context")
	}
	e.currentFn.Instructions = append(e.currentFn.Instructions, inst)
}

// Emit takes an AST FileNode and produces a BIR Package.
func (e *Emitter) Emit(fileNode *parser.FileNode) (*Package, error) {
	if fileNode == nil {
		return nil, fmt.Errorf("cannot emit BIR from nil AST FileNode")
	}
	e.birPackage = &Package{
		// OrgName, Name, Version would come from project files or default
		OrgName:   "myorg", // Placeholder
		Name:      "main",  // Placeholder, could be filename
		Version:   "0.1.0", // Placeholder
		Functions: []*Function{},
	}

	for _, impNode := range fileNode.Imports {
		e.emitImport(impNode)
	}

	for _, def := range fileNode.Definitions {
		switch node := def.(type) {
		case *parser.FunctionDefinitionNode:
			fnBir, err := e.emitFunctionDefinition(node)
			if err != nil {
				return nil, fmt.Errorf("failed to emit function %s: %w", node.Name.Value, err)
			}
			e.birPackage.Functions = append(e.birPackage.Functions, fnBir)
		// case *parser.GlobalVariableDeclarationNode: // Assuming such a node exists
		// e.g. `int globalVar = 10;` at package level
		// TODO: Add global variable handling if AST supports it directly.
		// For now, this example focuses on functions.
		default:
			// Other top-level definitions (services, listeners, constants, types)
			// For this subset, we only handle functions.
		}
	}

	if len(e.birPackage.Functions) == 0 && len(e.birPackage.ImportModules) == 0 {
		// If the file was empty or only comments, fileNode.Definitions might be empty.
		return nil, nil // No BIR to generate
	}

	return e.birPackage, nil
}

func (e *Emitter) emitImport(impNode *parser.ImportNode) {
	// For simplicity, Ballerina's `import ballerina/io;` or `import ballerina/io as bbio;`
	// The BIR needs org, package name, and alias.
	// The parser.ImportNode needs to capture these properly.
	// Assuming impNode.OrgName, impNode.PackageName, impNode.Alias are populated by parser.
	birImp := &ImportModule{
		OrgName:     impNode.OrgName,     // e.g. "ballerina"
		PackageName: impNode.PackageName, // e.g. "io"
		Alias:       impNode.PackageName, // Default alias to package name if not specified
	}
	if impNode.Alias != "" {
		birImp.Alias = impNode.Alias
	}
	e.birPackage.ImportModules = append(e.birPackage.ImportModules, birImp)
}

func (e *Emitter) emitFunctionDefinition(fdn *parser.FunctionDefinitionNode) (*Function, error) {
	e.currentFn = &Function{
		Name:           fdn.Name.Value,
		Visibility:     strings.ToUpper(fdn.Visibility), // e.g. PUBLIC
		Instructions:   []Instruction{},
		LocalVars:      []*Variable{},
		nextVarID:      0, // Reset for each function
		currentScopeID: 0, // Base scope
	}
	// Process parameters
	for _, paramAST := range fdn.Parameters {
		e.currentFn.nextVarID++
		paramBIR := &Variable{
			ID:      e.currentFn.nextVarID,
			Name:    paramAST.Name.Value,
			Type:    paramAST.Type.TypeName, // Simplified type
			Kind:    VarKindParameter,
			ScopeID: e.currentFn.currentScopeID,
		}
		e.currentFn.Parameters = append(e.currentFn.Parameters, paramBIR)
		e.currentFn.LocalVars = append(e.currentFn.LocalVars, paramBIR) // Params are also local vars
	}

	// Process return type
	if fdn.ReturnType != nil {
		e.currentFn.ReturnType = fdn.ReturnType.TypeName // Simplified
	} else {
		e.currentFn.ReturnType = "()" // Convention for nil/void return
	}

	// Process function body
	if fdn.Body != nil {
		e.emitBlockStatement(fdn.Body)
	}

	// Add an implicit return if the last instruction is not a terminator (and func is void)
	// A well-formed function should always end with a return.
	// If the function is non-void, it must have explicit return statements.
	// If void, an implicit return is added if not already terminated.
	if len(e.currentFn.Instructions) == 0 || !isTerminator(e.currentFn.Instructions[len(e.currentFn.Instructions)-1]) {
		if e.currentFn.ReturnType == "()" {
			e.addInstruction(&ReturnInst{})
		} else {
			// This indicates a semantic error: non-void function doesn't end with a return.
			// A full compiler would have semantic analysis to catch this.
			// For BIR emission, we could add a placeholder or error.
			// Let's assume semantic analysis would have caught this.
			// For now, if it's non-void and has no terminator, it's an issue.
			// We will add a return still to make BIR technically valid.
			e.addInstruction(&ReturnInst{}) // May need a return var if non-void
		}
	}

	fnToReturn := e.currentFn
	e.currentFn = nil // Clear context
	return fnToReturn, nil
}

func isTerminator(inst Instruction) bool {
	_, ok := inst.(TerminatorInstruction)
	return ok
}

func (e *Emitter) emitBlockStatement(block *parser.BlockStatementNode) {
	// TODO: Handle variable scopes within blocks if needed for more complex scenarios
	// e.currentFn.currentScopeID++ // Enter new scope
	for _, stmt := range block.Statements {
		e.emitStatement(stmt)
	}
	// e.currentFn.currentScopeID-- // Exit scope
}

func (e *Emitter) emitStatement(stmt parser.Statement) {
	switch s := stmt.(type) {
	case *parser.VariableDeclarationNode:
		e.emitVariableDeclaration(s)
	case *parser.ExpressionStatementNode:
		// The result of the expression is discarded if it's a statement
		e.emitExpression(s.Expression, true) // isDiscarded = true
	case *parser.ReturnStatementNode:
		e.emitReturnStatement(s)
	case *parser.IfStatementNode:
		e.emitIfStatement(s)
	// Other statement types: For, While, Match, etc.
	default:
		// Handle unknown statement type or error
		e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("unhandled AST statement type: %T", s)})

	}
}

// ErrorInst is a placeholder for errors encountered during BIR emission.
type ErrorInst struct {
	Msg string
}

func (i *ErrorInst) String() string { return fmt.Sprintf("ERROR_BIR: %s", i.Msg) }

func (e *Emitter) emitVariableDeclaration(vdn *parser.VariableDeclarationNode) {
	// Create the variable in BIR
	e.currentFn.nextVarID++
	birVar := &Variable{
		ID:      e.currentFn.nextVarID,
		Name:    vdn.Name.Value,
		Type:    vdn.Type.TypeName, // Simplified type
		Kind:    VarKindLocal,
		ScopeID: e.currentFn.currentScopeID,
	}
	e.currentFn.LocalVars = append(e.currentFn.LocalVars, birVar)

	// If there's an initial value, emit expression and move/assign
	if vdn.InitialValue != nil {
		valVar := e.emitExpression(vdn.InitialValue, false)
		if valVar != nil {
			e.addInstruction(&MoveInst{LHS: birVar, RHS: valVar})
		} else {
			// Error emitting initial value expression
			e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("failed to emit initializer for var %s", birVar.Name)})
		}
	} else {
		// Variables must be initialized in Ballerina.
		// If InitialValue is nil, parser should've caught it, or it's a language feature not in this subset.
		// For now, assume InitialValue is always present for declared vars if required by language.
		// If uninitialized vars are allowed, they'd get a default value or be marked.
		// Ballerina requires initialization. This else block implies a parser/semantic error.
		e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("variable %s declared without initializer (Ballerina requires initialization)", birVar.Name)})
	}
}

func (e *Emitter) emitReturnStatement(rsn *parser.ReturnStatementNode) {
	if rsn.ReturnValue != nil {
		returnValVar := e.emitExpression(rsn.ReturnValue, false)
		if returnValVar != nil {
			// In a more complex BIR, there would be a dedicated return variable slot.
			// For now, we just note which variable holds the value being returned.
			e.addInstruction(&ReturnInst{ReturnVar: returnValVar})
		} else {
			e.addInstruction(&ErrorInst{Msg: "failed to emit return value expression"})
			e.addInstruction(&ReturnInst{}) // Still add return to terminate block
		}
	} else {
		e.addInstruction(&ReturnInst{})
	}
}

func (e *Emitter) emitIfStatement(ifStmt *parser.IfStatementNode) {
	conditionVar := e.emitExpression(ifStmt.Condition, false)
	if conditionVar == nil {
		e.addInstruction(&ErrorInst{Msg: "failed to emit if-condition expression"})
		return // Cannot proceed without condition
	}

	thenLabel := e.newLabel()
	elseLabel := e.newLabel()
	endIfLabel := "" // Only needed if there's an else block

	if ifStmt.Alternative != nil {
		endIfLabel = e.newLabel()
		e.addInstruction(&CondBranchInst{Condition: conditionVar, TrueLabel: thenLabel, FalseLabel: elseLabel})
	} else {
		// If no else, branch to 'then' or skip to 'endIf' (which is 'elseLabel' here)
		e.addInstruction(&CondBranchInst{Condition: conditionVar, TrueLabel: thenLabel, FalseLabel: elseLabel /* this will be the end label */})
	}

	// Then block
	e.addInstruction(&LabelInst{Name: thenLabel})
	e.emitBlockStatement(ifStmt.Consequence)
	if ifStmt.Alternative != nil { // If there was an else, need to jump past it
		e.addInstruction(&BranchInst{TargetLabel: endIfLabel})
	}

	// Else block (or end label if no else)
	e.addInstruction(&LabelInst{Name: elseLabel})
	if ifStmt.Alternative != nil {
		e.emitBlockStatement(ifStmt.Alternative)
		e.addInstruction(&LabelInst{Name: endIfLabel}) // Common exit point after else
	}
	// If no else block, elseLabel effectively becomes the endIfLabel.
}

// emitExpression evaluates an AST expression and returns a BIR variable holding the result.
// isDiscarded indicates if the result of this expression will be used.
// If true, for some expressions (like function calls with no return to assign), LHS can be nil.
func (e *Emitter) emitExpression(expr parser.Expression, isDiscarded bool) *Variable {
	switch ex := expr.(type) {
	case *parser.IntegerLiteralNode:
		tempVar := e.newTempVar("int") // Assume type from literal
		e.addInstruction(&ConstantLoadInst{LHS: tempVar, Value: ex.Value, TypeName: "int"})
		return tempVar
	case *parser.StringLiteralNode:
		tempVar := e.newTempVar("string")
		e.addInstruction(&ConstantLoadInst{LHS: tempVar, Value: ex.Value, TypeName: "string"})
		return tempVar
	case *parser.BooleanLiteralNode:
		tempVar := e.newTempVar("boolean")
		e.addInstruction(&ConstantLoadInst{LHS: tempVar, Value: ex.Value, TypeName: "boolean"})
		return tempVar
	case *parser.IdentifierNode:
		// Find the variable in localVars (includes params)
		// This requires a symbol table or searching localVars.
		for _, v := range e.currentFn.LocalVars {
			if v.Name == ex.Value {
				return v // Return existing variable
			}
		}
		// TODO: Handle globals or undefined variables
		e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("undefined identifier: %s", ex.Value)})
		return e.newTempVar("error") // Return a dummy error var

	case *parser.BinaryExpressionNode:
		leftVar := e.emitExpression(ex.Left, false)
		rightVar := e.emitExpression(ex.Right, false)
		if leftVar == nil || rightVar == nil {
			e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("failed to emit operand for binary op %s", ex.Operator)})
			return e.newTempVar("error") // Dummy error var
		}

		// Determine result type (simplified: assume same as operands or context-dependent)
		// For this subset, assume int for arithmetic, boolean for comparisons.
		var resultType string
		switch ex.Operator {
		case "+", "-", "*", "/":
			resultType = "int" // Assuming integer arithmetic
		case "==", "!=", "<", "<=", ">", ">=":
			resultType = "boolean" // Comparison ops yield boolean
		default:
			resultType = "unknown" // Should be caught by parser or semantic analyzer
			e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("unsupported binary operator: %s", ex.Operator)})
			return e.newTempVar("error")
		}

		resultVar := e.newTempVar(resultType)
		e.addInstruction(&BinaryOpInst{LHS: resultVar, Op: ex.Operator, Op1: leftVar, Op2: rightVar})
		return resultVar

	case *parser.CallExpressionNode:
		return e.emitCallExpression(ex, isDiscarded)

	// Other expression types: Unary, MemberAccess (if not part of Call), ArrayLiterals, etc.
	default:
		e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("unhandled AST expression type: %T", ex)})
		return e.newTempVar("error") // Dummy error var
	}
}

func (e *Emitter) emitCallExpression(callAST *parser.CallExpressionNode, isDiscarded bool) *Variable {
	var pkgAlias, funcName string

	switch fnIdentifier := callAST.Function.(type) {
	case *parser.IdentifierNode: // Simple function call: foo()
		funcName = fnIdentifier.Value
	case *parser.MemberAccessExpressionNode: // Module call: io:println()
		if modIdent, ok := fnIdentifier.Expression.(*parser.IdentifierNode); ok {
			pkgAlias = modIdent.Value // This is the module alias/name
		} else {
			e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("unsupported member access expression in call: %T", fnIdentifier.Expression)})
			return e.newTempVar("error")
		}
		funcName = fnIdentifier.MemberName.Value
	default:
		e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("unsupported function identifier type in call: %T", callAST.Function)})
		return e.newTempVar("error")
	}

	argsBIR := []*Variable{}
	for _, argAST := range callAST.Arguments {
		argBIR := e.emitExpression(argAST, false)
		if argBIR == nil {
			e.addInstruction(&ErrorInst{Msg: fmt.Sprintf("failed to emit argument for call to %s", funcName)})
			return e.newTempVar("error")
		}
		argsBIR = append(argsBIR, argBIR)
	}

	// Determine return type of the function being called.
	// This requires looking up the function signature (from imports or current package).
	// For simplicity, we'll assume void return if isDiscarded, or a generic "any" type if not.
	// A real BIR emitter would have access to full type information.
	var lhsVar *Variable = nil
	if !isDiscarded {
		// This is a placeholder. The actual type should be known.
		// If the function call is part of an assignment `x = foo()`, the type of x could guide this.
		// Or, function signature lookup is needed.
		// For built-in like `io:println`, return is typically nil/void.
		// Let's assume for now that if it's not discarded, we need a temp var for a potential return.
		// The type is tricky without full type system. Let's use "any" as placeholder.
		// A better approach: if the function is known to be void, lhsVar remains nil even if !isDiscarded.
		// For now, we create a temp var if not discarded.
		// If it's an expression statement like `foo();`, then isDiscarded is true.
		// If it's `int x = foo();`, isDiscarded is false.

		// Simplification: if it's a known void function like println, don't create LHS.
		// This requires function signature knowledge.
		// For this subset, assume io:println is void.
		isKnownVoid := (pkgAlias == "io" && funcName == "println") // Very basic check

		if !isKnownVoid {
			lhsVar = e.newTempVar("any") // Placeholder type, should be function's actual return type
		} else {
			// If known void, but isDiscarded is false (e.g. `string s = io:println()` - a type error)
			// Semantic analysis should catch this. BIR just reflects the call.
			// If it's `_ = io:println()`, isDiscarded might be false, but lhs is special.
			// For `_ = expr`, we can use a special discard variable.
		}

	}

	e.addInstruction(&CallInst{
		LHS:          lhsVar,
		PackageAlias: pkgAlias,
		FunctionName: funcName,
		Args:         argsBIR,
	})
	return lhsVar // This will be nil if isDiscarded or function is known void and assigned to nothing.
}
