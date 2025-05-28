// parser/ast.go
package parser

import (
	"strings"
	"wx-yz/lightfoot/lexer"
)

// Node is the interface for all AST nodes.
type Node interface {
	TokenLiteral() string // Used for debugging and testing
	String() string       // String representation of the node
	// Adding StartToken() to all nodes to easily get their primary token for error reporting
	StartToken() lexer.Token
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
	Token       lexer.Token
	Imports     []*ImportNode
	Definitions []Node
}

func (fn *FileNode) TokenLiteral() string    { return "FILE" }
func (fn *FileNode) String() string          { return "FileNode" }
func (fn *FileNode) StartToken() lexer.Token { return fn.Token }

// ImportNode represents an import declaration.
type ImportNode struct {
	Token       lexer.Token
	OrgName     string
	PackageName string
	Alias       string
}

func (in *ImportNode) TokenLiteral() string    { return in.Token.Literal }
func (in *ImportNode) String() string          { /* ... */ return "import ...;" }
func (in *ImportNode) statementNode()          {}
func (in *ImportNode) StartToken() lexer.Token { return in.Token }

// FunctionDefinitionNode represents a function definition.
type FunctionDefinitionNode struct {
	Token      lexer.Token
	Name       *IdentifierNode
	Visibility string
	Parameters []*ParameterNode
	ReturnType *TypeNode
	Body       *BlockStatementNode
}

func (fdn *FunctionDefinitionNode) TokenLiteral() string    { return fdn.Token.Literal }
func (fdn *FunctionDefinitionNode) String() string          { return "FunctionDefinition: " + fdn.Name.Value }
func (fdn *FunctionDefinitionNode) statementNode()          {}
func (fdn *FunctionDefinitionNode) StartToken() lexer.Token { return fdn.Token }

// InitFunctionNode represents an init function declaration.
// init functions have specific constraints: no public visibility, no parameters, return type must be error? or ()
type InitFunctionNode struct {
	Token      lexer.Token
	ReturnType *TypeNode           // Must be error? or ()
	Body       *BlockStatementNode // Function body
}

func (ifn *InitFunctionNode) TokenLiteral() string    { return ifn.Token.Literal }
func (ifn *InitFunctionNode) String() string          { return "InitFunction" }
func (ifn *InitFunctionNode) statementNode()          {}
func (ifn *InitFunctionNode) StartToken() lexer.Token { return ifn.Token }

// ServiceDeclarationNode represents a service declaration.
// e.g., service / on new http:Listener(9090) { ... }
type ServiceDeclarationNode struct {
	Token     lexer.Token         // The 'service' token
	Path      Expression          // The service path (e.g., /, /api/v1)
	Listener  Expression          // The listener expression (e.g., new http:Listener(9090))
	Resources []*ResourceFunction // The resource functions within the service
}

func (sdn *ServiceDeclarationNode) TokenLiteral() string    { return sdn.Token.Literal }
func (sdn *ServiceDeclarationNode) String() string          { return "ServiceDeclaration" }
func (sdn *ServiceDeclarationNode) statementNode()          {}
func (sdn *ServiceDeclarationNode) StartToken() lexer.Token { return sdn.Token }

// ResourceFunction represents a resource function within a service.
// e.g., resource function get greeting() returns string { ... }
type ResourceFunction struct {
	Token      lexer.Token         // The 'resource' token
	Method     *IdentifierNode     // HTTP method (get, post, etc.)
	Name       *IdentifierNode     // Function name
	Parameters []*ParameterNode    // Function parameters
	ReturnType *TypeNode           // Return type
	Body       *BlockStatementNode // Function body
}

func (rf *ResourceFunction) TokenLiteral() string { return rf.Token.Literal }
func (rf *ResourceFunction) String() string {
	return "ResourceFunction: " + rf.Method.Value + " " + rf.Name.Value
}
func (rf *ResourceFunction) statementNode()          {}
func (rf *ResourceFunction) StartToken() lexer.Token { return rf.Token }

// NewExpressionNode represents a 'new' expression for object creation.
// e.g., new http:Listener(9090)
type NewExpressionNode struct {
	Token     lexer.Token  // The 'new' token
	Type      Expression   // The type being instantiated (e.g., http:Listener)
	Arguments []Expression // Constructor arguments
}

func (nen *NewExpressionNode) TokenLiteral() string    { return nen.Token.Literal }
func (nen *NewExpressionNode) String() string          { return "new expression" }
func (nen *NewExpressionNode) expressionNode()         {}
func (nen *NewExpressionNode) StartToken() lexer.Token { return nen.Token }

// ParameterNode represents a function parameter.
type ParameterNode struct {
	Token lexer.Token
	Type  *TypeNode
	Name  *IdentifierNode
}

func (pn *ParameterNode) TokenLiteral() string    { return pn.Type.TokenLiteral() } // Use type's token literal
func (pn *ParameterNode) String() string          { return pn.Type.String() + " " + pn.Name.Value }
func (pn *ParameterNode) StartToken() lexer.Token { return pn.Type.StartToken() } // Delegate to type's start token

// TypeNode represents a type name.
type TypeNode struct {
	Token     lexer.Token
	TypeName  string
	IsArray   bool
	IsNilable bool
}

func (tn *TypeNode) TokenLiteral() string    { return tn.Token.Literal }
func (tn *TypeNode) String() string          { /* ... */ return tn.TypeName }
func (tn *TypeNode) expressionNode()         {}
func (tn *TypeNode) StartToken() lexer.Token { return tn.Token }

// BlockStatementNode represents a block of statements.
type BlockStatementNode struct {
	Token      lexer.Token
	Statements []Statement
}

func (bsn *BlockStatementNode) TokenLiteral() string    { return bsn.Token.Literal }
func (bsn *BlockStatementNode) String() string          { return "{ ... }" }
func (bsn *BlockStatementNode) statementNode()          {}
func (bsn *BlockStatementNode) StartToken() lexer.Token { return bsn.Token }

// VariableDeclarationNode represents a variable declaration statement.
type VariableDeclarationNode struct {
	Token        lexer.Token
	Type         *TypeNode
	Name         *IdentifierNode
	InitialValue Expression
}

func (vdn *VariableDeclarationNode) TokenLiteral() string    { return vdn.Type.TokenLiteral() }
func (vdn *VariableDeclarationNode) String() string          { /* ... */ return "var decl" }
func (vdn *VariableDeclarationNode) statementNode()          {}
func (vdn *VariableDeclarationNode) StartToken() lexer.Token { return vdn.Type.StartToken() }

// ExpressionStatementNode represents a statement that is just an expression.
type ExpressionStatementNode struct {
	Token      lexer.Token
	Expression Expression
}

func (esn *ExpressionStatementNode) TokenLiteral() string    { return esn.Token.Literal }
func (esn *ExpressionStatementNode) String() string          { /* ... */ return "expr stmt" }
func (esn *ExpressionStatementNode) statementNode()          {}
func (esn *ExpressionStatementNode) StartToken() lexer.Token { return esn.Token }

// ReturnStatementNode represents a return statement.
type ReturnStatementNode struct {
	Token       lexer.Token
	ReturnValue Expression
}

func (rsn *ReturnStatementNode) TokenLiteral() string    { return rsn.Token.Literal }
func (rsn *ReturnStatementNode) String() string          { /* ... */ return "return" }
func (rsn *ReturnStatementNode) statementNode()          {}
func (rsn *ReturnStatementNode) StartToken() lexer.Token { return rsn.Token }

// IfStatementNode represents an if-else statement.
type IfStatementNode struct {
	Token       lexer.Token
	Condition   Expression
	Consequence *BlockStatementNode
	Alternative *BlockStatementNode
}

func (isn *IfStatementNode) TokenLiteral() string    { return isn.Token.Literal }
func (isn *IfStatementNode) String() string          { return "if (...) { ... }" }
func (isn *IfStatementNode) statementNode()          {}
func (isn *IfStatementNode) StartToken() lexer.Token { return isn.Token }

// IdentifierNode represents an identifier.
type IdentifierNode struct {
	Token lexer.Token
	Value string
}

func (in *IdentifierNode) TokenLiteral() string    { return in.Token.Literal }
func (in *IdentifierNode) String() string          { return in.Value }
func (in *IdentifierNode) expressionNode()         {}
func (in *IdentifierNode) StartToken() lexer.Token { return in.Token }

// IntegerLiteralNode represents an integer literal.
type IntegerLiteralNode struct {
	Token lexer.Token
	Value int64
}

func (iln *IntegerLiteralNode) TokenLiteral() string    { return iln.Token.Literal }
func (iln *IntegerLiteralNode) String() string          { return iln.Token.Literal }
func (iln *IntegerLiteralNode) expressionNode()         {}
func (iln *IntegerLiteralNode) StartToken() lexer.Token { return iln.Token }

// StringLiteralNode represents a string literal.
type StringLiteralNode struct {
	Token lexer.Token
	Value string
}

func (sln *StringLiteralNode) TokenLiteral() string    { return sln.Token.Literal }
func (sln *StringLiteralNode) String() string          { return "\"" + sln.Value + "\"" }
func (sln *StringLiteralNode) expressionNode()         {}
func (sln *StringLiteralNode) StartToken() lexer.Token { return sln.Token }

// BooleanLiteralNode represents a boolean literal.
type BooleanLiteralNode struct {
	Token lexer.Token
	Value bool
}

func (bln *BooleanLiteralNode) TokenLiteral() string    { return bln.Token.Literal }
func (bln *BooleanLiteralNode) String() string          { return bln.Token.Literal }
func (bln *BooleanLiteralNode) expressionNode()         {}
func (bln *BooleanLiteralNode) StartToken() lexer.Token { return bln.Token }

// BinaryExpressionNode represents a binary operation.
type BinaryExpressionNode struct {
	Token    lexer.Token // The operator token
	Left     Expression
	Operator string
	Right    Expression
}

func (ben *BinaryExpressionNode) TokenLiteral() string    { return ben.Token.Literal }
func (ben *BinaryExpressionNode) String() string          { /* ... */ return "binary expr" }
func (ben *BinaryExpressionNode) expressionNode()         {}
func (ben *BinaryExpressionNode) StartToken() lexer.Token { return ben.Token } // Operator token

// CallExpressionNode represents a function call.
type CallExpressionNode struct {
	Token          lexer.Token // The '(' token
	Function       Expression
	Arguments      []Expression
	IsMemberAccess bool
}

func (cen *CallExpressionNode) TokenLiteral() string    { return cen.Token.Literal }
func (cen *CallExpressionNode) String() string          { /* ... */ return "call expr" }
func (cen *CallExpressionNode) expressionNode()         {}
func (cen *CallExpressionNode) StartToken() lexer.Token { return cen.Function.StartToken() } // Token of the function identifier/member access

// MemberAccessExpressionNode represents access to a member of a module/object
type MemberAccessExpressionNode struct {
	Token      lexer.Token // The ':' token
	Expression Expression
	MemberName *IdentifierNode
}

func (man *MemberAccessExpressionNode) TokenLiteral() string    { return man.Token.Literal }
func (man *MemberAccessExpressionNode) String() string          { /* ... */ return "member access" }
func (man *MemberAccessExpressionNode) expressionNode()         {}
func (man *MemberAccessExpressionNode) StartToken() lexer.Token { return man.Expression.StartToken() } // Token of the expression being accessed

// AssignmentExpressionNode represents an assignment operation.
// e.g., x = 5
type AssignmentExpressionNode struct {
	Token  lexer.Token // The '=' token
	Target Expression  // The left-hand side (e.g., IdentifierNode)
	Value  Expression  // The right-hand side
}

func (aen *AssignmentExpressionNode) TokenLiteral() string { return aen.Token.Literal }
func (aen *AssignmentExpressionNode) String() string {
	return "(" + aen.Target.String() + " = " + aen.Value.String() + ")"
}
func (aen *AssignmentExpressionNode) expressionNode()         {}
func (aen *AssignmentExpressionNode) StartToken() lexer.Token { return aen.Target.StartToken() } // Or aen.Token (=), target is better for error source

// GlobalVariableNode represents a global variable declaration.
type GlobalVariableNode struct {
	Token       lexer.Token
	Name        *IdentifierNode
	Type        *TypeNode
	IsFinal     bool       // whether the variable is declared with 'final'
	Initializer Expression // optional
}

func (g *GlobalVariableNode) TokenLiteral() string { return g.Token.Literal }
func (g *GlobalVariableNode) String() string {
	var sb strings.Builder
	if g.IsFinal {
		sb.WriteString("final ")
	}
	sb.WriteString(g.Type.TypeName)
	sb.WriteString(" ")
	sb.WriteString(g.Name.Value)
	if g.Initializer != nil {
		sb.WriteString(" = ")
		sb.WriteString(g.Initializer.String())
	}
	sb.WriteString(";")
	return sb.String()
}
func (g *GlobalVariableNode) StartToken() lexer.Token { return g.Token }
func (g *GlobalVariableNode) statementNode()          {}
