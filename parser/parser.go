// parser/parser.go
package parser

import (
	"fmt"
	"strconv"
	"strings"
	"wx-yz/lightfoot/lexer"
)

// Precedence levels for operators
const (
	_            int = iota // 0
	Lowest                  // 1
	Assignment              // 2 =
	Equals                  // 3 ==, !=
	LessGreater             // 4 >, <, >=, <=
	Sum                     // 5 +, -
	Product                 // 6 *, /
	Prefix                  // 7 (Not used by infix, for potential prefix ops)
	Call                    // 8 myFunction(X)
	MemberAccess            // 9 module:function or object.field
)

// precedences maps token types to their precedence levels.
var precedences = map[lexer.TokenType]int{
	lexer.TokenEqual:              Assignment,
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
	lexer.TokenLParen:             Call,
	lexer.TokenColon:              MemberAccess,
	lexer.TokenDot:                MemberAccess,
}

type Parser struct {
	tokens []lexer.Token
	pos    int
	errors []string

	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

type (
	prefixParseFn func() Expression
	infixParseFn  func(Expression) Expression
)

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
	p.registerPrefix(lexer.TokenSlash, p.parseSlashLiteral)  // Add support for / as service path
	p.registerPrefix(lexer.TokenKwNew, p.parseNewExpression) // Add support for new expressions

	p.infixParseFns = make(map[lexer.TokenType]infixParseFn)
	p.registerInfix(lexer.TokenEqual, p.parseAssignmentExpression)
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
	p.registerInfix(lexer.TokenLParen, p.parseCallExpression)
	p.registerInfix(lexer.TokenColon, p.parseMemberAccessExpression)

	return p
}

func (p *Parser) currentToken() lexer.Token {
	if p.pos >= len(p.tokens) {
		line, col := -1, -1
		if len(p.tokens) > 0 {
			last := p.tokens[len(p.tokens)-1]
			line = last.Line
			col = last.Column + len(last.Literal)
		}
		return lexer.Token{Type: lexer.TokenEOF, Literal: "", Line: line, Column: col}
	}
	return p.tokens[p.pos]
}

func (p *Parser) nextToken() {
	if p.pos < len(p.tokens) {
		p.pos++
	}
}

func (p *Parser) expectToken(expectedType lexer.TokenType) bool {
	current := p.currentToken()
	if current.Type == expectedType {
		p.nextToken()
		return true
	}
	p.errorExpectedToken(expectedType, current)
	return false
}

func (p *Parser) errorExpectedToken(expectedType lexer.TokenType, actualToken lexer.Token) {
	msg := fmt.Sprintf("line %d, col %d: expected token %s, got %s (%q)",
		actualToken.Line, actualToken.Column, expectedType, actualToken.Type, actualToken.Literal)
	p.errors = append(p.errors, msg)
}

func (p *Parser) errorUnexpectedToken(message string, tokenForPos lexer.Token) {
	msg := fmt.Sprintf("line %d, col %d: %s, got %s (%q)",
		tokenForPos.Line, tokenForPos.Column, message, tokenForPos.Type, tokenForPos.Literal)
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

func (p *Parser) currentPrecedence() int {
	if prec, ok := precedences[p.currentToken().Type]; ok {
		return prec
	}
	return Lowest
}

// ParseFile is the entry point for parsing a Ballerina file.
// MODIFIED to correctly populate fileNode.Imports
func (p *Parser) ParseFile() (*FileNode, error) {
	fileNode := &FileNode{Token: p.currentToken()}
	// Initialize slices to prevent nil pointer issues if file is empty
	fileNode.Imports = []*ImportNode{}
	fileNode.Definitions = []Node{}

	for p.currentToken().Type != lexer.TokenEOF {
		initialPos := p.pos
		currentTokenForSwitch := p.currentToken()
		parsedSuccessfully := true // Assume success unless a parse function returns nil

		switch currentTokenForSwitch.Type {
		case lexer.TokenKwImport:
			imp := p.parseImportStatement()
			if imp != nil {
				fileNode.Imports = append(fileNode.Imports, imp)
			} else {
				parsedSuccessfully = false // parseImportStatement failed
			}
		case lexer.TokenKwPublic, lexer.TokenKwFunction:
			fn := p.parseFunctionDefinition()
			if fn != nil {
				fileNode.Definitions = append(fileNode.Definitions, fn)
			} else {
				parsedSuccessfully = false // parseFunctionDefinition failed
			}
		case lexer.TokenKwService:
			svc := p.parseServiceDeclaration()
			if svc != nil {
				fileNode.Definitions = append(fileNode.Definitions, svc)
			} else {
				parsedSuccessfully = false // parseServiceDeclaration failed
			}
		default:
			p.errorUnexpectedToken("unexpected top-level token", currentTokenForSwitch)
			p.nextToken()              // Consume the unexpected token
			parsedSuccessfully = false // Not a recognized top-level construct
		}

		// If parsing a construct failed (parser returned nil) AND the parser position didn't advance,
		// it means the specific parse function got stuck. Force advancement.
		if !parsedSuccessfully && p.pos == initialPos && p.currentToken().Type != lexer.TokenEOF {
			// This situation implies a bug in a sub-parser that returned nil without advancing
			// and potentially without logging a specific enough error.
			if len(p.errors) == 0 || p.errors[len(p.errors)-1] != fmt.Sprintf("line %d, col %d: unexpected top-level token, got %s (%q)", currentTokenForSwitch.Line, currentTokenForSwitch.Column, currentTokenForSwitch.Type, currentTokenForSwitch.Literal) {
				// Add a "stuck" error only if the default case didn't already cover this token
				p.errorUnexpectedToken("parser stuck after failing to parse top-level element", p.currentToken())
			}
			p.nextToken() // Force advancement
		}

		if len(p.errors) > 50 {
			p.errors = append(p.errors, "too many errors, parsing aborted")
			break
		}
	}

	if len(p.errors) > 0 {
		var errorBuilder strings.Builder
		errorBuilder.WriteString(fmt.Sprintf("parsing error: parsing failed with %d error(s):\n", len(p.errors)))
		for i, e := range p.errors {
			errorBuilder.WriteString(fmt.Sprintf("  %d: %s\n", i+1, e))
		}
		return nil, fmt.Errorf(errorBuilder.String())
	}
	return fileNode, nil
}

// Pratt parser core logic for expressions
func (p *Parser) parseExpression(precedence int) Expression {
	prefixFn := p.prefixParseFns[p.currentToken().Type]
	if prefixFn == nil {
		p.errorUnexpectedToken(fmt.Sprintf("no prefix parse function for token %s ('%s')", p.currentToken().Type, p.currentToken().Literal), p.currentToken())
		return nil
	}
	leftExp := prefixFn()

	if leftExp == nil {
		return nil
	}

	for {
		currentOpToken := p.currentToken()
		currentOpPrecedence := p.currentPrecedence()

		if precedence >= currentOpPrecedence {
			break
		}

		infixFn := p.infixParseFns[currentOpToken.Type]
		if infixFn == nil {
			break
		}
		leftExp = infixFn(leftExp)
		if leftExp == nil {
			return nil
		}
	}
	return leftExp
}

func (p *Parser) parseIdentifier() Expression {
	current := p.currentToken()
	if current.Type != lexer.TokenIdentifier {
		p.errorUnexpectedToken("expected identifier", current)
		return nil
	}
	ident := &IdentifierNode{Token: current, Value: current.Literal}
	p.nextToken()
	return ident
}

func (p *Parser) parseIntegerLiteral() Expression {
	tok := p.currentToken()
	if tok.Type != lexer.TokenIntLiteral {
		p.errorUnexpectedToken("expected integer literal", tok)
		return nil
	}
	val, err := strconv.ParseInt(tok.Literal, 0, 64)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: could not parse %q as integer: %v", tok.Line, tok.Column, tok.Literal, err))
		p.nextToken()
		return nil
	}
	p.nextToken()
	return &IntegerLiteralNode{Token: tok, Value: val}
}

func (p *Parser) parseStringLiteral() Expression {
	tok := p.currentToken()
	if tok.Type != lexer.TokenStringLiteral {
		p.errorUnexpectedToken("expected string literal", tok)
		return nil
	}
	p.nextToken()
	return &StringLiteralNode{Token: tok, Value: tok.Literal}
}

func (p *Parser) parseBooleanLiteral() Expression {
	tok := p.currentToken()
	if tok.Type != lexer.TokenBooleanLiteral {
		p.errorUnexpectedToken("expected boolean literal", tok)
		return nil
	}
	val := tok.Literal == "true"
	p.nextToken()
	return &BooleanLiteralNode{Token: tok, Value: val}
}

// parseSlashLiteral parses a '/' token as a service path
func (p *Parser) parseSlashLiteral() Expression {
	tok := p.currentToken()
	if tok.Type != lexer.TokenSlash {
		p.errorUnexpectedToken("expected slash", tok)
		return nil
	}
	// Treat "/" as a string literal for service paths
	p.nextToken()
	return &StringLiteralNode{Token: tok, Value: "/"}
}

// parseNewExpression parses a 'new' expression for object creation
// Format: new <type>(<args>)
// Example: new http:Listener(9090)
func (p *Parser) parseNewExpression() Expression {
	tok := p.currentToken()
	if tok.Type != lexer.TokenKwNew {
		p.errorUnexpectedToken("expected 'new'", tok)
		return nil
	}
	p.nextToken()

	// Parse the type (could be a member access like http:Listener)
	typeToken := p.currentToken()
	typeExpr := p.parseExpression(Call) // Use Call precedence to allow member access to be parsed
	if typeExpr == nil {
		p.errorUnexpectedToken("expected type after 'new'", typeToken)
		return nil
	}

	// Now we should be at the opening parenthesis of the constructor call
	if p.currentToken().Type != lexer.TokenLParen {
		p.errorExpectedToken(lexer.TokenLParen, p.currentToken())
		return nil
	}

	// Consume the opening parenthesis before calling parseExpressionList
	p.nextToken()

	arguments := p.parseExpressionList(lexer.TokenRParen)
	if arguments == nil {
		return nil
	}

	return &NewExpressionNode{Token: tok, Type: typeExpr, Arguments: arguments}
}

func (p *Parser) parseGroupedExpression() Expression {
	lParenToken := p.currentToken()
	if lParenToken.Type != lexer.TokenLParen {
		p.errorExpectedToken(lexer.TokenLParen, lParenToken)
		return nil
	}
	p.nextToken() // Consume LParen

	exp := p.parseExpression(Lowest)
	if exp == nil {
		if p.currentToken().Type != lexer.TokenRParen {
			p.errorUnexpectedToken("malformed expression or missing ')' in grouped expression", lParenToken)
		}
		return nil
	}
	if !p.expectToken(lexer.TokenRParen) {
		return nil
	}
	return exp
}

func (p *Parser) parseInfixExpression(left Expression) Expression {
	operatorToken := p.currentToken()
	expr := &BinaryExpressionNode{
		Token:    operatorToken,
		Operator: operatorToken.Literal,
		Left:     left,
	}
	currentOpPrecedence := p.currentPrecedence()

	p.nextToken() // Consume the operator.

	rhsStartToken := p.currentToken()
	expr.Right = p.parseExpression(currentOpPrecedence)
	if expr.Right == nil {
		if p.currentToken() == rhsStartToken {
			p.errorUnexpectedToken("expected expression on right side of binary operator", rhsStartToken)
		}
		return nil
	}
	return expr
}

func (p *Parser) parseAssignmentExpression(left Expression) Expression {
	assignToken := p.currentToken() // Current token is '='

	switch left.(type) {
	case *IdentifierNode, *MemberAccessExpressionNode:
		// Valid LValue for this subset
	default:
		p.errorUnexpectedToken(fmt.Sprintf("invalid assignment target, got %T", left), left.StartToken())
		return nil
	}

	assignNode := &AssignmentExpressionNode{
		Token:  assignToken,
		Target: left,
	}

	assignPrecedence := p.currentPrecedence()
	p.nextToken() // Consume '='

	rhsStartToken := p.currentToken()
	assignNode.Value = p.parseExpression(assignPrecedence - 1)
	if assignNode.Value == nil {
		if p.currentToken() == rhsStartToken {
			p.errorUnexpectedToken("expected expression after '=' in assignment", rhsStartToken)
		}
		return nil
	}
	return assignNode
}

func (p *Parser) parseCallExpression(functionName Expression) Expression {
	lParenToken := p.currentToken()
	p.nextToken() // Consume LParen

	args := p.parseExpressionList(lexer.TokenRParen)
	if args == nil {
		return nil
	}

	callExpr := &CallExpressionNode{
		Token:     lParenToken,
		Function:  functionName,
		Arguments: args,
	}
	if _, ok := functionName.(*MemberAccessExpressionNode); ok {
		callExpr.IsMemberAccess = true
	}
	return callExpr
}

func (p *Parser) parseMemberAccessExpression(left Expression) Expression {
	colonToken := p.currentToken()
	p.nextToken() // Consume ':'

	memberNameToken := p.currentToken()
	memberNameExpr := p.parseIdentifier()
	if memberNameExpr == nil {
		return nil
	}
	memberName, ok := memberNameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("member name must be an identifier (internal parser error: unexpected type)", memberNameToken)
		return nil
	}

	return &MemberAccessExpressionNode{
		Token:      colonToken,
		Expression: left,
		MemberName: memberName,
	}
}

func (p *Parser) parseExpressionList(endToken lexer.TokenType) []Expression {
	list := []Expression{}

	if p.currentToken().Type == endToken {
		p.nextToken()
		return list
	}

	firstExprTokenPos := p.currentToken()
	firstExpr := p.parseExpression(Lowest)
	if firstExpr == nil {
		if p.currentToken() == firstExprTokenPos && p.currentToken().Type != endToken {
			p.errorUnexpectedToken("expected expression in list, but parsing failed to advance", firstExprTokenPos)
		}
		return nil
	}
	list = append(list, firstExpr)

	for p.currentToken().Type == lexer.TokenComma {
		p.nextToken()

		if p.currentToken().Type == endToken {
			p.errorUnexpectedToken("unexpected end token after comma (trailing comma?)", p.currentToken())
			return nil
		}

		nextExprTokenPos := p.currentToken()
		expr := p.parseExpression(Lowest)
		if expr == nil {
			if p.currentToken() == nextExprTokenPos && p.currentToken().Type != endToken {
				p.errorUnexpectedToken("expected expression after comma, but parsing failed to advance", nextExprTokenPos)
			}
			return nil
		}
		list = append(list, expr)
	}

	if !p.expectToken(endToken) {
		return nil
	}
	return list
}

func (p *Parser) parseImportStatement() *ImportNode {
	stmt := &ImportNode{Token: p.currentToken()}
	if !p.expectToken(lexer.TokenKwImport) {
		return nil
	}

	firstIdentToken := p.currentToken()
	firstIdentExpr := p.parseIdentifier()
	if firstIdentExpr == nil {
		return nil
	}
	firstIdentNode, ok := firstIdentExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("module name component must be an identifier", firstIdentToken)
		return nil
	}

	if p.currentToken().Type == lexer.TokenSlash {
		stmt.OrgName = firstIdentNode.Value
		p.nextToken()

		packageNameToken := p.currentToken()
		secondIdentExpr := p.parseIdentifier()
		if secondIdentExpr == nil {
			return nil
		}
		secondIdentNode, ok := secondIdentExpr.(*IdentifierNode)
		if !ok {
			p.errorUnexpectedToken("package name component must be an identifier", packageNameToken)
			return nil
		}
		stmt.PackageName = secondIdentNode.Value
	} else {
		stmt.PackageName = firstIdentNode.Value
		// By default, OrgName is empty if not specified with a slash.
		// A later step or semantic analysis might assign a default org.
	}

	// TODO: Parse 'as <alias>' here and set stmt.Alias
	// For now, stmt.Alias remains "" if not set by 'as' clause.

	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

func (p *Parser) parseFunctionDefinition() *FunctionDefinitionNode {
	stmt := &FunctionDefinitionNode{Token: p.currentToken()}

	if p.currentToken().Type == lexer.TokenKwPublic {
		stmt.Visibility = "public"
		p.nextToken()
	} else {
		stmt.Visibility = ""
	}

	if !p.expectToken(lexer.TokenKwFunction) {
		return nil
	}

	nameToken := p.currentToken()
	nameExpr := p.parseIdentifier()
	if nameExpr == nil {
		return nil
	}
	nameIdent, ok := nameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("function name must be an identifier (internal parser error: unexpected type)", nameToken)
		return nil
	}
	stmt.Name = nameIdent

	if !p.expectToken(lexer.TokenLParen) {
		return nil
	}
	stmt.Parameters = p.parseFunctionParameters()
	if stmt.Parameters == nil && len(p.errors) > 0 && p.currentToken().Type != lexer.TokenRParen {
		return nil
	}
	if !p.expectToken(lexer.TokenRParen) {
		return nil
	}

	if p.currentToken().Literal == "returns" {
		p.nextToken()
		returnTypeToken := p.currentToken()
		stmt.ReturnType = p.parseType()
		if stmt.ReturnType == nil {
			p.errorUnexpectedToken("expected return type after 'returns' keyword", returnTypeToken)
			return nil
		}
	}

	bodyToken := p.currentToken()
	if bodyToken.Type != lexer.TokenLBrace {
		p.errorExpectedToken(lexer.TokenLBrace, bodyToken)
		return nil
	}
	stmt.Body = p.parseBlockStatement()
	if stmt.Body == nil {
		return nil
	}

	return stmt
}

func (p *Parser) parseFunctionParameters() []*ParameterNode {
	params := []*ParameterNode{}
	if p.currentToken().Type == lexer.TokenRParen {
		return params
	}

	param := p.parseParameter()
	if param == nil {
		return nil
	}
	params = append(params, param)

	for p.currentToken().Type == lexer.TokenComma {
		p.nextToken()
		nextParamToken := p.currentToken()
		param = p.parseParameter()
		if param == nil {
			isValidStartOfParam := p.currentToken().Type == lexer.TokenIdentifier ||
				p.currentToken().Type == lexer.TokenKwInt ||
				p.currentToken().Type == lexer.TokenKwstring ||
				p.currentToken().Type == lexer.TokenKwBoolean
			if !isValidStartOfParam && p.currentToken() != nextParamToken && p.currentToken().Type != lexer.TokenRParen {
				p.errorUnexpectedToken("expected parameter after comma", nextParamToken)
			}
			return nil
		}
		params = append(params, param)
	}
	return params
}

func (p *Parser) parseParameter() *ParameterNode {
	typeToken := p.currentToken()
	paramType := p.parseType()
	if paramType == nil {
		p.errorUnexpectedToken("expected parameter type", typeToken)
		return nil
	}

	nameToken := p.currentToken()
	nameExpr := p.parseIdentifier()
	if nameExpr == nil {
		return nil
	}
	nameIdent, ok := nameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("parameter name must be an identifier (internal parser error: unexpected type)", nameToken)
		return nil
	}

	return &ParameterNode{Token: typeToken, Type: paramType, Name: nameIdent}
}

func (p *Parser) parseType() *TypeNode {
	tok := p.currentToken()
	typeName := ""

	switch tok.Type {
	case lexer.TokenKwInt, lexer.TokenKwstring, lexer.TokenKwBoolean:
		typeName = tok.Literal
	case lexer.TokenIdentifier:
		typeName = tok.Literal
	default:
		return nil
	}
	p.nextToken()
	return &TypeNode{Token: tok, TypeName: typeName}
}

func (p *Parser) parseBlockStatement() *BlockStatementNode {
	blockToken := p.currentToken()
	block := &BlockStatementNode{Token: blockToken}
	if !p.expectToken(lexer.TokenLBrace) {
		return nil
	}

	block.Statements = []Statement{}
	for p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
		stmtStartToken := p.currentToken()
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		} else {
			if p.currentToken() == stmtStartToken && p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
				p.errorUnexpectedToken("failed to parse statement or parser stuck in block", p.currentToken())
				p.nextToken()
			}
		}
	}

	if !p.expectToken(lexer.TokenRBrace) {
		return nil
	}
	return block
}

func (p *Parser) parseStatement() Statement {
	var stmt Statement
	currentStatementStartToken := p.currentToken()
	switch currentStatementStartToken.Type {
	case lexer.TokenKwInt, lexer.TokenKwstring, lexer.TokenKwBoolean, lexer.TokenKwVar:
		stmt = p.parseVariableDeclarationStatement()
	case lexer.TokenKwReturn:
		stmt = p.parseReturnStatement()
	case lexer.TokenKwIf:
		stmt = p.parseIfStatement()
	default:
		if p.prefixParseFns[currentStatementStartToken.Type] != nil {
			stmt = p.parseExpressionStatement()
		} else {
			p.errorUnexpectedToken("expected statement", currentStatementStartToken)
			p.nextToken()
			return nil
		}
	}
	return stmt
}

func (p *Parser) parseVariableDeclarationStatement() *VariableDeclarationNode {
	stmt := &VariableDeclarationNode{Token: p.currentToken()}

	if p.currentToken().Type == lexer.TokenKwVar {
		varToken := p.currentToken()
		p.nextToken()
		stmt.Type = &TypeNode{Token: varToken, TypeName: "var"}
	} else {
		typeToken := p.currentToken()
		parsedType := p.parseType()
		if parsedType == nil {
			p.errorUnexpectedToken("expected type name in variable declaration", typeToken)
			return nil
		}
		stmt.Type = parsedType
	}

	nameToken := p.currentToken()
	nameExpr := p.parseIdentifier()
	if nameExpr == nil {
		return nil
	}
	nameIdent, ok := nameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("variable name must be an identifier (internal parser error: unexpected type)", nameToken)
		return nil
	}
	stmt.Name = nameIdent

	if !p.expectToken(lexer.TokenEqual) { // This is for `var_decl = expr`, not assignment expression
		return nil
	}

	initialValueTokenPos := p.currentToken()
	stmt.InitialValue = p.parseExpression(Lowest)
	if stmt.InitialValue == nil {
		if p.currentToken() == initialValueTokenPos {
			p.errorUnexpectedToken("expected expression for variable initialization", initialValueTokenPos)
		}
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

	if p.currentToken().Type != lexer.TokenSemicolon && p.currentToken().Type != lexer.TokenEOF {
		returnValueTokenPos := p.currentToken()
		stmt.ReturnValue = p.parseExpression(Lowest)
		if stmt.ReturnValue == nil {
			if p.currentToken() == returnValueTokenPos {
				p.errorUnexpectedToken("expected expression or ';' after return", returnValueTokenPos)
			}
			return nil
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

	conditionTokenPos := p.currentToken()
	stmt.Condition = p.parseExpression(Lowest)
	if stmt.Condition == nil {
		if p.currentToken() == conditionTokenPos {
			p.errorUnexpectedToken("expected condition for if statement", conditionTokenPos)
		}
		return nil
	}

	consequenceTokenPos := p.currentToken()
	if consequenceTokenPos.Type != lexer.TokenLBrace {
		p.errorExpectedToken(lexer.TokenLBrace, consequenceTokenPos)
		return nil
	}
	stmt.Consequence = p.parseBlockStatement()
	if stmt.Consequence == nil {
		return nil
	}

	if p.currentToken().Type == lexer.TokenKwElse {
		p.nextToken()

		alternativeTokenPos := p.currentToken()
		if alternativeTokenPos.Type != lexer.TokenLBrace {
			p.errorExpectedToken(lexer.TokenLBrace, alternativeTokenPos)
			return nil
		}
		stmt.Alternative = p.parseBlockStatement()
		if stmt.Alternative == nil {
			return nil
		}
	}
	return stmt
}

func (p *Parser) parseExpressionStatement() *ExpressionStatementNode {
	stmtStartToken := p.currentToken()
	stmt := &ExpressionStatementNode{Token: stmtStartToken}

	stmt.Expression = p.parseExpression(Lowest)
	if stmt.Expression == nil {
		if p.currentToken() == stmtStartToken {
		}
		return nil
	}

	if !p.expectToken(lexer.TokenSemicolon) {
		return nil
	}
	return stmt
}

// parseServiceDeclaration parses a service declaration.
// Format: service <path> on <listener> { <resources> }
// Example: service / on new http:Listener(9090) { resource function get greeting() returns string { ... } }
func (p *Parser) parseServiceDeclaration() *ServiceDeclarationNode {
	stmt := &ServiceDeclarationNode{Token: p.currentToken()}

	if !p.expectToken(lexer.TokenKwService) {
		return nil
	}

	// Parse service path (e.g., /, /api/v1)
	pathToken := p.currentToken()
	stmt.Path = p.parseExpression(Lowest)
	if stmt.Path == nil {
		p.errorUnexpectedToken("expected service path after 'service'", pathToken)
		return nil
	}

	// Expect 'on' keyword
	if !p.expectToken(lexer.TokenKwOn) {
		return nil
	}

	// Parse listener expression (e.g., new http:Listener(9090))
	listenerToken := p.currentToken()
	stmt.Listener = p.parseExpression(Lowest)
	if stmt.Listener == nil {
		p.errorUnexpectedToken("expected listener expression after 'on'", listenerToken)
		return nil
	}

	// Expect opening brace for service body
	if !p.expectToken(lexer.TokenLBrace) {
		return nil
	}

	// Parse resource functions
	stmt.Resources = []*ResourceFunction{}
	for p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
		resourceToken := p.currentToken()
		if resourceToken.Type == lexer.TokenKwResource {
			resource := p.parseResourceFunction()
			if resource != nil {
				stmt.Resources = append(stmt.Resources, resource)
			} else {
				p.errorUnexpectedToken("failed to parse resource function", resourceToken)
				p.nextToken() // Skip problematic token
			}
		} else {
			p.errorUnexpectedToken("expected 'resource' in service body", resourceToken)
			p.nextToken() // Skip unexpected token
		}
	}

	if !p.expectToken(lexer.TokenRBrace) {
		return nil
	}

	return stmt
}

// parseResourceFunction parses a resource function within a service.
// Format: resource function <method> <name>(<params>) returns <type> { <body> }
// Example: resource function get greeting() returns string { return "Hello, World!"; }
func (p *Parser) parseResourceFunction() *ResourceFunction {
	stmt := &ResourceFunction{Token: p.currentToken()}

	if !p.expectToken(lexer.TokenKwResource) {
		return nil
	}

	if !p.expectToken(lexer.TokenKwFunction) {
		return nil
	}

	// Parse HTTP method (get, post, etc.)
	methodToken := p.currentToken()
	methodExpr := p.parseIdentifier()
	if methodExpr == nil {
		return nil
	}
	methodIdent, ok := methodExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("resource method must be an identifier", methodToken)
		return nil
	}
	stmt.Method = methodIdent

	// Parse resource name
	nameToken := p.currentToken()
	nameExpr := p.parseIdentifier()
	if nameExpr == nil {
		return nil
	}
	nameIdent, ok := nameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("resource name must be an identifier", nameToken)
		return nil
	}
	stmt.Name = nameIdent

	// Parse parameters
	if !p.expectToken(lexer.TokenLParen) {
		return nil
	}
	stmt.Parameters = p.parseFunctionParameters()
	if stmt.Parameters == nil && len(p.errors) > 0 && p.currentToken().Type != lexer.TokenRParen {
		return nil
	}
	if !p.expectToken(lexer.TokenRParen) {
		return nil
	}

	// Parse return type (optional)
	if p.currentToken().Literal == "returns" {
		p.nextToken()
		returnTypeToken := p.currentToken()
		stmt.ReturnType = p.parseType()
		if stmt.ReturnType == nil {
			p.errorUnexpectedToken("expected return type after 'returns' keyword", returnTypeToken)
			return nil
		}
	}

	// Parse function body
	bodyToken := p.currentToken()
	if bodyToken.Type != lexer.TokenLBrace {
		p.errorExpectedToken(lexer.TokenLBrace, bodyToken)
		return nil
	}
	stmt.Body = p.parseBlockStatement()
	if stmt.Body == nil {
		return nil
	}

	return stmt
}
