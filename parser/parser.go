// parser/parser.go
package parser

import (
	"fmt"
	"strconv"
	"wx-yz/lightfoot/lexer"
)

// Precedence levels for operators
const (
	_ int = iota
	Lowest
	Equals       // ==
	LessGreater  // > or <
	Sum          // +
	Product      // *
	Prefix       // -X or !X
	Call         // myFunction(X)
	Index        // array[index]
	MemberAccess // object.field or module:function
)

var precedences = map[lexer.TokenType]int{
	lexer.TokenEqualEqual:         Equals,
	lexer.TokenNotEqual:           Equals,
	lexer.TokenLessThan:           LessGreater,
	lexer.TokenLessThanOrEqual:    LessGreater,
	lexer.TokenGreaterThan:        LessGreater,
	lexer.TokenGreaterThanOrEqual: LessGreater,
	lexer.TokenPlus:               Sum,
	lexer.TokenMinus:              Sum,
	lexer.TokenSlash:              Product,
	lexer.TokenAsterisk:           Product,
	lexer.TokenLParen:             Call,         // For call expressions
	lexer.TokenColon:              MemberAccess, // For qualified identifiers like io:println
	lexer.TokenDot:                MemberAccess, // For future field access
}

// Parser holds the state of the parser.
type Parser struct {
	l      *lexer.Lexer // Kept for error reporting with line/col if needed directly
	tokens []lexer.Token
	pos    int // current token position

	errors []string

	// Prefix and infix parsing functions
	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

type (
	prefixParseFn func() Expression
	infixParseFn  func(Expression) Expression // Takes the left expression as argument
)

// NewParser creates a new Parser.
func NewParser(tokens []lexer.Token) *Parser {
	p := &Parser{
		tokens: tokens,
		errors: []string{},
	}

	p.prefixParseFns = make(map[lexer.TokenType]prefixParseFn)
	p.registerPrefix(lexer.TokenIdentifier, p.parseIdentifier)
	p.registerPrefix(lexer.TokenIntLiteral, p.parseIntegerLiteral)
	p.registerPrefix(lexer.TokenStringLiteral, p.parseStringLiteral)
	p.registerPrefix(lexer.TokenBooleanLiteral, p.parseBooleanLiteral)
	p.registerPrefix(lexer.TokenLParen, p.parseGroupedExpression)
	// Add prefix operators if any (e.g., !, -)

	p.infixParseFns = make(map[lexer.TokenType]infixParseFn)
	p.registerInfix(lexer.TokenPlus, p.parseInfixExpression)
	p.registerInfix(lexer.TokenMinus, p.parseInfixExpression)
	p.registerInfix(lexer.TokenSlash, p.parseInfixExpression)
	p.registerInfix(lexer.TokenAsterisk, p.parseInfixExpression)
	p.registerInfix(lexer.TokenEqualEqual, p.parseInfixExpression)
	p.registerInfix(lexer.TokenNotEqual, p.parseInfixExpression)
	p.registerInfix(lexer.TokenLessThan, p.parseInfixExpression)
	p.registerInfix(lexer.TokenLessThanOrEqual, p.parseInfixExpression)
	p.registerInfix(lexer.TokenGreaterThan, p.parseInfixExpression)
	p.registerInfix(lexer.TokenGreaterThanOrEqual, p.parseInfixExpression)
	p.registerInfix(lexer.TokenLParen, p.parseCallExpression)        // For function calls
	p.registerInfix(lexer.TokenColon, p.parseMemberAccessExpression) // For module:function

	return p
}

func (p *Parser) currentToken() lexer.Token {
	if p.pos >= len(p.tokens) {
		// Return EOF if out of bounds
		lastToken := lexer.Token{Type: lexer.TokenEOF, Line: -1, Column: -1} // synthetic EOF
		if len(p.tokens) > 0 {
			lastToken = p.tokens[len(p.tokens)-1] // Use last known token's pos for EOF error
			lastToken.Type = lexer.TokenEOF
			lastToken.Literal = ""
		}
		return lastToken
	}
	return p.tokens[p.pos]
}

func (p *Parser) peekToken() lexer.Token {
	if p.pos+1 >= len(p.tokens) {
		return p.currentToken() // effectively EOF
	}
	return p.tokens[p.pos+1]
}

func (p *Parser) nextToken() {
	if p.pos < len(p.tokens) { // Ensure we don't go past EOF if already there
		p.pos++
	}
}

func (p *Parser) expectToken(t lexer.TokenType) bool {
	if p.currentToken().Type == t {
		p.nextToken()
		return true
	}
	p.errorExpectedToken(t)
	return false
}

func (p *Parser) errorExpectedToken(expected lexer.TokenType) {
	msg := fmt.Sprintf("line %d, col %d: expected token %s, got %s (%q)",
		p.currentToken().Line, p.currentToken().Column, expected, p.currentToken().Type, p.currentToken().Literal)
	p.errors = append(p.errors, msg)
}

func (p *Parser) errorUnexpectedToken(message string) {
	msg := fmt.Sprintf("line %d, col %d: %s, got %s (%q)",
		p.currentToken().Line, p.currentToken().Column, message, p.currentToken().Type, p.currentToken().Literal)
	p.errors = append(p.errors, msg)
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType lexer.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// ParseFile is the entry point for parsing a Ballerina file.
func (p *Parser) ParseFile() (*FileNode, error) {
	fileNode := &FileNode{Token: p.currentToken()} // Use first token for FileNode

	for p.currentToken().Type != lexer.TokenEOF {
		switch p.currentToken().Type {
		case lexer.TokenKwImport:
			imp := p.parseImportStatement()
			if imp != nil {
				fileNode.Imports = append(fileNode.Imports, imp)
			}
		case lexer.TokenKwPublic, lexer.TokenKwFunction: // Also handle private/package-private functions later
			fn := p.parseFunctionDefinition()
			if fn != nil {
				fileNode.Definitions = append(fileNode.Definitions, fn)
			}
		default:
			// Skip unknown top-level tokens for now, or error
			p.errorUnexpectedToken(fmt.Sprintf("unexpected top-level token %s", p.currentToken().Type))
			p.nextToken() // consume to avoid infinite loop
		}
		if len(p.errors) > 50 { // Prevent too many errors
			p.errors = append(p.errors, "too many errors, parsing aborted")
			break
		}
	}

	if len(p.errors) > 0 {
		// Concatenate errors into a single error object if desired
		// For simplicity, returning the first one or a summary
		return nil, fmt.Errorf("parsing failed with %d errors: %s", len(p.errors), p.errors[0])
	}
	return fileNode, nil
}

func (p *Parser) parseImportStatement() *ImportNode {
	stmt := &ImportNode{Token: p.currentToken()}
	if !p.expectToken(lexer.TokenKwImport) {
		return nil
	}

	// Simplified: ballerina/io or io (assuming ballerina org for now if no slash)
	// A full parser would handle org-name/pkg-name:version [as alias]
	identParts := []string{}
	firstIdent := p.parseIdentifier()
	if firstIdent == nil {
		p.errorUnexpectedToken("expected module name after import")
		return nil
	}
	identParts = append(identParts, firstIdent.Value)

	if p.currentToken().Type == lexer.TokenSlash {
		p.nextToken() // Consume '/'
		secondIdent := p.parseIdentifier()
		if secondIdent == nil {
			p.errorUnexpectedToken("expected package name after '/' in import")
			return nil
		}
		stmt.OrgName = identParts[0]
		stmt.PackageName = secondIdent.Value
	} else {
		stmt.PackageName = identParts[0] // e.g. import http;
	}

	// Optional 'as alias'
	// TODO: Add 'as' keyword and alias parsing. For now, this is skipped.

	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

func (p *Parser) parseFunctionDefinition() *FunctionDefinitionNode {
	stmt := &FunctionDefinitionNode{Token: p.currentToken()}

	if p.currentToken().Type == lexer.TokenKwPublic {
		stmt.Visibility = "public"
		p.nextToken() // Consume 'public'
	} else {
		stmt.Visibility = "" // package-private by default
	}

	if !p.expectToken(lexer.TokenKwFunction) {
		return nil
	}

	stmt.Name = p.parseIdentifier()
	if stmt.Name == nil {
		p.errorUnexpectedToken("expected function name")
		return nil
	}

	if !p.expectToken(lexer.TokenLParen) {
		return nil
	}
	stmt.Parameters = p.parseFunctionParameters()
	if !p.expectToken(lexer.TokenRParen) {
		return nil
	}

	// Optional return type
	if p.currentToken().Type != lexer.TokenLBrace { // If it's not body, it might be return type
		// Ballerina uses 'returns TypeAnnotation'
		// For simplicity, if not '{', assume it's a type for return
		// A more robust parser would look for 'returns' keyword.
		// This simplified parser assumes type directly.
		// Example: function foo() int { ... } (not standard Ballerina, standard is 'returns int')
		// Let's try to parse 'returns' keyword if present
		if p.currentToken().Literal == "returns" { // Not a token type, check literal
			p.nextToken() // consume "returns"
			stmt.ReturnType = p.parseType()
			if stmt.ReturnType == nil {
				p.errorUnexpectedToken("expected return type after 'returns'")
				return nil
			}
		} else if p.currentToken().Type == lexer.TokenKwInt ||
			p.currentToken().Type == lexer.TokenKwstring ||
			p.currentToken().Type == lexer.TokenKwBoolean ||
			p.currentToken().Type == lexer.TokenIdentifier { // For custom types
			// This is a simplified way to handle return types without "returns" keyword
			// which is not Ballerina standard but common in other languages.
			// For true Ballerina, 'returns' is mandatory for typed returns.
			// This part can be adjusted to strictly follow Ballerina spec.
			// For this example, let's assume direct type means return type if not LBrace
			// p.errorUnexpectedToken("expected 'returns' keyword for return type or '{' for function body")
			// return nil
			// OR, parse as a typenode (non-standard for ballerina without 'returns')
			// For this simplified subset, we will allow direct type declaration for return type.
			// If the subset should be stricter, this would be an error.
			// For now, let's assume `function foo() Type {` is what we're trying to parse
			// This is not standard Ballerina. Standard is `function foo() returns Type {`
			// For the sake of having *some* return type parsing:
			if p.currentToken().Type != lexer.TokenLBrace {
				stmt.ReturnType = p.parseType()
				if stmt.ReturnType == nil {
					p.errorUnexpectedToken("expected return type or '{' for function body")
					return nil
				}
			}
		}
	}

	if p.currentToken().Type != lexer.TokenLBrace {
		p.errorUnexpectedToken("expected '{' for function body")
		return nil
	}
	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseFunctionParameters() []*ParameterNode {
	params := []*ParameterNode{}
	if p.currentToken().Type == lexer.TokenRParen { // No parameters
		return params
	}

	param := p.parseParameter()
	if param != nil {
		params = append(params, param)
	} else {
		return nil // Error in parsing first param
	}

	for p.currentToken().Type == lexer.TokenComma {
		p.nextToken() // Consume ','
		param = p.parseParameter()
		if param != nil {
			params = append(params, param)
		} else {
			return nil // Error in parsing subsequent param
		}
	}
	return params
}

func (p *Parser) parseParameter() *ParameterNode {
	param := &ParameterNode{Token: p.currentToken()} // Token of type
	param.Type = p.parseType()
	if param.Type == nil {
		return nil
	}
	param.Name = p.parseIdentifier()
	if param.Name == nil {
		p.errorUnexpectedToken("expected parameter name")
		return nil
	}
	return param
}

func (p *Parser) parseType() *TypeNode {
	// Handles basic types like int, string, boolean
	// For arrays, custom types, nilable types, this needs expansion
	tok := p.currentToken()
	typeName := ""
	isBasicType := false

	switch tok.Type {
	case lexer.TokenKwInt:
		typeName = "int"
		isBasicType = true
	case lexer.TokenKwstring:
		typeName = "string"
		isBasicType = true
	case lexer.TokenKwBoolean:
		typeName = "boolean"
		isBasicType = true
	case lexer.TokenIdentifier: // Could be a custom type name
		typeName = tok.Literal
		isBasicType = false // Or true if it resolves to a basic type synonym later
	default:
		p.errorUnexpectedToken("expected type name")
		return nil
	}

	if isBasicType == true {
	}

	p.nextToken() // Consume type token

	// TODO: Add parsing for array type `[]` and nilable `?`
	return &TypeNode{Token: tok, TypeName: typeName}
}

func (p *Parser) parseBlockStatement() *BlockStatementNode {
	block := &BlockStatementNode{Token: p.currentToken()}
	if !p.expectToken(lexer.TokenLBrace) {
		return nil
	}

	block.Statements = []Statement{}
	for p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		} else {
			// Error already reported by parseStatement, consume token to prevent infinite loop if it didn't.
			// However, parseStatement should consume or error out.
			// If parseStatement returns nil and doesn't advance, this could loop.
			// For now, assume parseStatement correctly handles advancement or errors.
			// If a statement parsing fails and returns nil, it might be better to stop parsing the block.
			// For robustness, we can add p.nextToken() here if stmt is nil and errors occurred.
			if len(p.errors) > 0 && p.currentToken().Type != lexer.TokenRBrace { // Avoid skipping RBrace
				// This is a bit risky, as the failed statement parser might have already advanced.
				// A safer approach is for sub-parsers to always advance past what they tried to parse.
			}
		}
	}

	if !p.expectToken(lexer.TokenRBrace) {
		return nil
	}
	return block
}

func (p *Parser) parseStatement() Statement {
	switch p.currentToken().Type {
	case lexer.TokenKwInt, lexer.TokenKwstring, lexer.TokenKwBoolean, lexer.TokenKwVar: // Start of a variable declaration
		// Also could be TokenIdentifier if it's a custom type
		return p.parseVariableDeclarationStatement()
	case lexer.TokenKwReturn:
		return p.parseReturnStatement()
	case lexer.TokenKwIf:
		return p.parseIfStatement()
	default: // Assume expression statement (like a function call)
		// Check if it's a valid start of an expression
		if p.prefixParseFns[p.currentToken().Type] != nil || p.currentToken().Type == lexer.TokenIdentifier {
			return p.parseExpressionStatement()
		}
		p.errorUnexpectedToken("expected statement")
		p.nextToken() // Consume the problematic token to avoid infinite loop
		return nil
	}
}

func (p *Parser) parseVariableDeclarationStatement() *VariableDeclarationNode {
	stmt := &VariableDeclarationNode{Token: p.currentToken()}

	// Type parsing
	// If 'var', type is inferred (not fully supported in BIR emitter for this subset)
	if p.currentToken().Type == lexer.TokenKwVar {
		p.nextToken() // consume 'var'
		// TypeNode will be special, or nil, indicating inference
		stmt.Type = &TypeNode{Token: stmt.Token, TypeName: "var"} // Placeholder
	} else {
		typ := p.parseType()
		if typ == nil {
			return nil // Error in type parsing
		}
		stmt.Type = typ
	}

	stmt.Name = p.parseIdentifier()
	if stmt.Name == nil {
		p.errorUnexpectedToken("expected variable name")
		return nil
	}

	if p.currentToken().Type != lexer.TokenEqual {
		p.errorExpectedToken(lexer.TokenEqual)
		return nil
	}
	p.nextToken() // Consume '='

	stmt.InitialValue = p.parseExpression(Lowest)
	if stmt.InitialValue == nil {
		p.errorUnexpectedToken("expected expression for variable initialization")
		return nil
	}

	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

func (p *Parser) parseReturnStatement() *ReturnStatementNode {
	stmt := &ReturnStatementNode{Token: p.currentToken()}
	if !p.expectToken(lexer.TokenKwReturn) {
		return nil
	}

	// Check if there's a return value or just ';'
	if p.currentToken().Type != lexer.TokenSemicolon {
		stmt.ReturnValue = p.parseExpression(Lowest)
		if stmt.ReturnValue == nil {
			p.errorUnexpectedToken("expected expression or ';' after return")
			// No return nil here, as a return statement can be valid without an expression (if followed by ';')
		}
	}

	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

func (p *Parser) parseIfStatement() *IfStatementNode {
	stmt := &IfStatementNode{Token: p.currentToken()}
	if !p.expectToken(lexer.TokenKwIf) {
		return nil
	}

	// Ballerina condition is an expression, not necessarily in parentheses
	// However, `if expr {` is common. For simplicity, we don't mandate `()` around condition.
	// A full spec parser would handle this more precisely.
	// if p.currentToken().Type == lexer.TokenLParen {
	//  p.nextToken() // consume (
	// 	stmt.Condition = p.parseExpression(Lowest)
	// 	if !p.expectToken(lexer.TokenRParen) { return nil }
	// } else {
	stmt.Condition = p.parseExpression(Lowest)
	// }

	if stmt.Condition == nil {
		p.errorUnexpectedToken("expected condition for if statement")
		return nil
	}

	if p.currentToken().Type != lexer.TokenLBrace {
		p.errorExpectedToken(lexer.TokenLBrace)
		return nil
	}
	stmt.Consequence = p.parseBlockStatement()
	if stmt.Consequence == nil {
		return nil
	}

	// Else part
	if p.currentToken().Type == lexer.TokenKwElse {
		p.nextToken()                                 // Consume 'else'
		if p.currentToken().Type == lexer.TokenKwIf { // Else if
			// For simplicity, treating 'else if' as a nested if inside the else block
			// A more advanced AST might have an ElseIfChain field.
			// Here, we expect 'else { ... }' or 'else if ... { ... }'
			// For 'else if', the 'if' becomes a statement within an implicit block for 'else'.
			// This is a common simplification.
			// Or, make Alternative an IfStatementNode or BlockStatementNode
			// For now, assume 'else' is followed by a block or another 'if'.
			// If it's 'else if', the next token is 'if', so parseIfStatement will handle it.
			// But current parseBlockStatement expects '{'.
			// Let's make 'else' always expect a block. 'else if' is `else { if ... }`
			if p.currentToken().Type != lexer.TokenLBrace && p.currentToken().Type != lexer.TokenKwIf {
				p.errorUnexpectedToken("expected '{' or 'if' after 'else'")
				return nil
			}
			// This needs to be a block, even if it's `else if`.
			// `else if cond {}` is `else { if cond {} }` implicitly.
			// For this simple parser, `else` must be followed by a block.
			// An `else if` structure will be `else { IfStatementNode }`
			// This means the `parseStatement` within `parseBlockStatement` needs to handle `if`.
			// This structure is fine.

			// For 'else if', we'd create a new block with just the IfStatementNode.
			// This is getting complex for the subset.
			// Let's simplify: 'else' is ONLY followed by a BlockStatement.
			// So `else if` is not directly supported as `else IfStatementNode`. It has to be `else { if ... }`.
			// Which is fine and common.
			if p.currentToken().Type != lexer.TokenLBrace {
				p.errorUnexpectedToken("expected '{' after 'else'")
				return nil
			}
			stmt.Alternative = p.parseBlockStatement() // This handles `else { if ... }` correctly
		} else if p.currentToken().Type == lexer.TokenLBrace {
			stmt.Alternative = p.parseBlockStatement()
		} else {
			p.errorUnexpectedToken("expected '{' after 'else'")
			return nil
		}
	}
	return stmt
}

func (p *Parser) parseExpressionStatement() *ExpressionStatementNode {
	stmt := &ExpressionStatementNode{Token: p.currentToken()}
	stmt.Expression = p.parseExpression(Lowest)
	if stmt.Expression == nil {
		// Error already reported by parseExpression
		return nil
	}

	// Expression statements must end with a semicolon in Ballerina
	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

// Pratt parser core logic
func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns[p.currentToken().Type]
	if prefix == nil {
		p.errorUnexpectedToken(fmt.Sprintf("no prefix parse function for %s", p.currentToken().Type))
		return nil
	}
	leftExp := prefix()

	// Loop for infix operators
	for p.currentToken().Type != lexer.TokenSemicolon && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken().Type]
		if infix == nil {
			return leftExp // No infix operator found, or it's lower precedence
		}
		p.nextToken() // Consume the operator
		leftExp = infix(leftExp)
	}
	return leftExp
}

func (p *Parser) peekPrecedence() int {
	if pr, ok := precedences[p.peekToken().Type]; ok {
		return pr
	}
	return Lowest
}

func (p *Parser) currentPrecedence() int {
	if pr, ok := precedences[p.currentToken().Type]; ok {
		return pr
	}
	return Lowest
}

// Prefix parsing functions
func (p *Parser) parseIdentifier() *IdentifierNode {
	if p.currentToken().Type != lexer.TokenIdentifier {
		p.errorUnexpectedToken("expected identifier")
		return nil
	}
	ident := &IdentifierNode{Token: p.currentToken(), Value: p.currentToken().Literal}
	p.nextToken() // Consume identifier
	return ident
}

func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteralNode{Token: p.currentToken()}
	val, err := strconv.ParseInt(lit.Token.Literal, 0, 64)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: could not parse %q as integer: %v", lit.Token.Line, lit.Token.Column, lit.Token.Literal, err))
		p.nextToken() // Consume the bad literal
		return nil
	}
	lit.Value = val
	p.nextToken() // Consume literal
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	lit := &StringLiteralNode{Token: p.currentToken(), Value: p.currentToken().Literal}
	p.nextToken() // Consume literal
	return lit
}

func (p *Parser) parseBooleanLiteral() Expression {
	tok := p.currentToken()
	val := tok.Literal == "true"
	p.nextToken() // Consume literal
	return &BooleanLiteralNode{Token: tok, Value: val}
}

func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken() // Consume '('
	exp := p.parseExpression(Lowest)
	if !p.expectToken(lexer.TokenRParen) {
		return nil // Error: expected ')'
	}
	return exp
}

// Infix parsing functions
func (p *Parser) parseInfixExpression(left Expression) Expression {
	expr := &BinaryExpressionNode{
		Token:    p.currentToken(), // Operator token
		Operator: p.currentToken().Literal,
		Left:     left,
	}
	precedence := p.currentPrecedence()
	p.nextToken() // Consume operator
	expr.Right = p.parseExpression(precedence)
	if expr.Right == nil {
		p.errorUnexpectedToken("expected expression on right side of binary operator")
		return nil
	}
	return expr
}

func (p *Parser) parseCallExpression(function Expression) Expression {
	// 'function' is the expression representing the function name (Identifier or MemberAccess)
	// Current token is '(', which was used to dispatch here.
	callExpr := &CallExpressionNode{Token: p.currentToken(), Function: function}
	// p.nextToken() // Consume '(', already done by the Pratt dispatcher if LParen is infix

	callExpr.Arguments = p.parseExpressionList(lexer.TokenRParen)

	// We don't need expectToken(RParen) here because parseExpressionList consumes it.
	// Or, if parseExpressionList does not consume it:
	// if !p.expectToken(lexer.TokenRParen) { return nil }
	// The call to parseCallExpression happens when LParen is *peeked*.
	// The pratt loop advances to LParen (current token), then calls this.
	// So current token is LParen.

	if _, ok := function.(*MemberAccessExpressionNode); ok {
		callExpr.IsMemberAccess = true
	}

	return callExpr
}

func (p *Parser) parseMemberAccessExpression(left Expression) Expression {
	// 'left' is the module/object identifier.
	// Current token is ':', which was used to dispatch here.
	expr := &MemberAccessExpressionNode{
		Token:      p.currentToken(), // ':' token
		Expression: left,
	}
	// p.nextToken() // Consume ':', already done by Pratt dispatcher.

	if p.currentToken().Type != lexer.TokenIdentifier {
		p.errorUnexpectedToken("expected identifier for member name after ':'")
		return nil
	}
	expr.MemberName = p.parseIdentifier() // This will consume the identifier
	if expr.MemberName == nil {
		return nil // Error already reported by parseIdentifier
	}

	return expr
}

func (p *Parser) parseExpressionList(endToken lexer.TokenType) []Expression {
	list := []Expression{}

	if p.currentToken().Type == endToken { // Empty list like foo()
		p.nextToken() // Consume endToken
		return list
	}

	firstExpr := p.parseExpression(Lowest)
	if firstExpr == nil {
		p.errorUnexpectedToken("expected expression in list")
		return nil // Propagate error
	}
	list = append(list, firstExpr)

	for p.currentToken().Type == lexer.TokenComma {
		p.nextToken() // Consume ','
		expr := p.parseExpression(Lowest)
		if expr == nil {
			p.errorUnexpectedToken("expected expression after comma in list")
			return nil // Propagate error
		}
		list = append(list, expr)
	}

	if !p.expectToken(endToken) {
		// Error already reported by expectToken
		return nil // Indicate error by returning nil
	}

	return list
}
