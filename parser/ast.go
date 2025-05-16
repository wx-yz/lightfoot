// parser/ast.go
package parser

import "wx-yz/lightfoot/lexer"

// Node is the interface for all AST nodes.
type Node interface {
	TokenLiteral() string // Used for debugging and testing
	String() string       // String representation of the node
}

// Statement is a sub-interface for statement nodes.
type Statement interface {
	Node
	statementNode()
}

// Expression is a sub-interface for expression nodes.
type Expression interface {
	Node
	expressionNode()
}

// FileNode represents a whole Ballerina file.
type FileNode struct {
	Token       lexer.Token // Typically the first token of the file or a synthetic one
	Imports     []*ImportNode
	Definitions []Node // Functions, services, global vars, etc.
}

func (fn *FileNode) TokenLiteral() string { return "FILE" }
func (fn *FileNode) String() string {
	// Basic string representation, can be expanded
	return "FileNode"
}

// ImportNode represents an import declaration.
// e.g., import ballerina/io; or import ballerina/math as m;
type ImportNode struct {
	Token       lexer.Token // The 'import' token
	OrgName     string      // e.g., "ballerina" (optional, can be inferred)
	PackageName string      // e.g., "io" or "math"
	Alias       string      // e.g., "m" (optional)
}

func (in *ImportNode) TokenLiteral() string { return in.Token.Literal }
func (in *ImportNode) String() string {
	s := "import "
	if in.OrgName != "" {
		s += in.OrgName + "/"
	}
	s += in.PackageName
	if in.Alias != "" {
		s += " as " + in.Alias
	}
	return s + ";"
}
func (in *ImportNode) statementNode() {} // Import can be considered a top-level statement

// FunctionDefinitionNode represents a function definition.
type FunctionDefinitionNode struct {
	Token      lexer.Token // The 'function' token
	Name       *IdentifierNode
	Visibility string // "public", "private", "" (package-private)
	Parameters []*ParameterNode
	ReturnType *TypeNode // nil if no return type (or void, which is implicit)
	Body       *BlockStatementNode
}

func (fdn *FunctionDefinitionNode) TokenLiteral() string { return fdn.Token.Literal }
func (fdn *FunctionDefinitionNode) String() string       { return "FunctionDefinition: " + fdn.Name.Value }
func (fdn *FunctionDefinitionNode) statementNode()       {} // A function definition is a top-level statement

// ParameterNode represents a function parameter.
type ParameterNode struct {
	Token lexer.Token // The token of the type
	Type  *TypeNode
	Name  *IdentifierNode
}

func (pn *ParameterNode) TokenLiteral() string { return pn.Token.Literal }
func (pn *ParameterNode) String() string       { return pn.Type.String() + " " + pn.Name.Value }

// TypeNode represents a type name.
type TypeNode struct {
	Token     lexer.Token // The token of the type (e.g., 'int', 'string')
	TypeName  string      // "int", "string", "boolean", "any", custom type
	IsArray   bool        // For array types like int[]
	IsNilable bool        // For nilable types like int?
}

func (tn *TypeNode) TokenLiteral() string { return tn.Token.Literal }
func (tn *TypeNode) String() string {
	name := tn.TypeName
	if tn.IsArray {
		name += "[]"
	}
	if tn.IsNilable {
		name += "?"
	}
	return name
}
func (tn *TypeNode) expressionNode() {} // Can be part of expressions in some contexts (e.g. type casts)

// BlockStatementNode represents a block of statements.
type BlockStatementNode struct {
	Token      lexer.Token // The '{' token
	Statements []Statement
}

func (bsn *BlockStatementNode) TokenLiteral() string { return bsn.Token.Literal }
func (bsn *BlockStatementNode) String() string       { return "{ ... }" }
func (bsn *BlockStatementNode) statementNode()       {}

// VariableDeclarationNode represents a variable declaration statement.
// e.g., int x = 10; string name = "Ballerina";
type VariableDeclarationNode struct {
	Token        lexer.Token // The type token or 'var' token
	Type         *TypeNode   // The declared type of the variable
	Name         *IdentifierNode
	InitialValue Expression // Can be nil if no initializer
}

func (vdn *VariableDeclarationNode) TokenLiteral() string { return vdn.Type.TokenLiteral() }
func (vdn *VariableDeclarationNode) String() string {
	s := vdn.Type.String() + " " + vdn.Name.Value
	if vdn.InitialValue != nil {
		s += " = " + vdn.InitialValue.String()
	}
	return s + ";"
}
func (vdn *VariableDeclarationNode) statementNode() {}

// ExpressionStatementNode represents a statement that is just an expression.
// e.g., a function call: foo();
type ExpressionStatementNode struct {
	Token      lexer.Token // The first token of the expression
	Expression Expression
}

func (esn *ExpressionStatementNode) TokenLiteral() string { return esn.Token.Literal }
func (esn *ExpressionStatementNode) String() string {
	if esn.Expression != nil {
		return esn.Expression.String() + ";"
	}
	return ""
}
func (esn *ExpressionStatementNode) statementNode() {}

// ReturnStatementNode represents a return statement.
type ReturnStatementNode struct {
	Token       lexer.Token // The 'return' token
	ReturnValue Expression  // Can be nil for a void return
}

func (rsn *ReturnStatementNode) TokenLiteral() string { return rsn.Token.Literal }
func (rsn *ReturnStatementNode) String() string {
	s := "return"
	if rsn.ReturnValue != nil {
		s += " " + rsn.ReturnValue.String()
	}
	return s + ";"
}
func (rsn *ReturnStatementNode) statementNode() {}

// IfStatementNode represents an if-else statement.
type IfStatementNode struct {
	Token       lexer.Token // The 'if' token
	Condition   Expression
	Consequence *BlockStatementNode
	Alternative *BlockStatementNode // Else block, can be nil
}

func (isn *IfStatementNode) TokenLiteral() string { return isn.Token.Literal }
func (isn *IfStatementNode) String() string       { return "if (...) { ... }" }
func (isn *IfStatementNode) statementNode()       {}

// IdentifierNode represents an identifier.
type IdentifierNode struct {
	Token lexer.Token // The identifier token
	Value string
}

func (in *IdentifierNode) TokenLiteral() string { return in.Token.Literal }
func (in *IdentifierNode) String() string       { return in.Value }
func (in *IdentifierNode) expressionNode()      {}

// IntegerLiteralNode represents an integer literal.
type IntegerLiteralNode struct {
	Token lexer.Token // The integer literal token
	Value int64
}

func (iln *IntegerLiteralNode) TokenLiteral() string { return iln.Token.Literal }
func (iln *IntegerLiteralNode) String() string       { return iln.Token.Literal }
func (iln *IntegerLiteralNode) expressionNode()      {}

// StringLiteralNode represents a string literal.
type StringLiteralNode struct {
	Token lexer.Token // The string literal token
	Value string
}

func (sln *StringLiteralNode) TokenLiteral() string { return sln.Token.Literal }
func (sln *StringLiteralNode) String() string       { return "\"" + sln.Value + "\"" }
func (sln *StringLiteralNode) expressionNode()      {}

// BooleanLiteralNode represents a boolean literal.
type BooleanLiteralNode struct {
	Token lexer.Token // The 'true' or 'false' token
	Value bool
}

func (bln *BooleanLiteralNode) TokenLiteral() string { return bln.Token.Literal }
func (bln *BooleanLiteralNode) String() string       { return bln.Token.Literal }
func (bln *BooleanLiteralNode) expressionNode()      {}

// BinaryExpressionNode represents a binary operation.
type BinaryExpressionNode struct {
	Token    lexer.Token // The operator token
	Left     Expression
	Operator string
	Right    Expression
}

func (ben *BinaryExpressionNode) TokenLiteral() string { return ben.Token.Literal }
func (ben *BinaryExpressionNode) String() string {
	return "(" + ben.Left.String() + " " + ben.Operator + " " + ben.Right.String() + ")"
}
func (ben *BinaryExpressionNode) expressionNode() {}

// CallExpressionNode represents a function call.
// e.g., foo(), io:println("Hello")
type CallExpressionNode struct {
	Token          lexer.Token // The '(' token or the function identifier token
	Function       Expression  // Identifier or MemberAccess (e.g., io:println)
	Arguments      []Expression
	IsMemberAccess bool // True if it's like module:function()
}

func (cen *CallExpressionNode) TokenLiteral() string { return cen.Token.Literal }
func (cen *CallExpressionNode) String() string {
	// Simplified string representation
	return cen.Function.String() + "(...)"
}
func (cen *CallExpressionNode) expressionNode() {}

// MemberAccessExpressionNode represents access to a member of a module/object
// e.g. io:println (where io is identifier, println is member identifier)
type MemberAccessExpressionNode struct {
	Token      lexer.Token     // The ':' token
	Expression Expression      // The expression on the left of ':' (e.g., an IdentifierNode for 'io')
	MemberName *IdentifierNode // The identifier on the right of ':' (e.g., 'println')
}

func (man *MemberAccessExpressionNode) TokenLiteral() string { return man.Token.Literal }
func (man *MemberAccessExpressionNode) String() string {
	return man.Expression.String() + ":" + man.MemberName.String()
}
func (man *MemberAccessExpressionNode) expressionNode() {}
