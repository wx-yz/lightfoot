// parser/parser.go
package parser

import (
	"fmt" // Ensure fmt is imported
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
	lexer.TokenKwIs:               Equals, // or another appropriate precedence
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
	p.registerPrefix(lexer.TokenFloatLiteral, p.parseFloatLiteral) // Added
	p.registerPrefix(lexer.TokenStringLiteral, p.parseStringLiteral)
	p.registerPrefix(lexer.TokenBooleanLiteral, p.parseBooleanLiteral)
	p.registerPrefix(lexer.TokenLParen, p.parseGroupedExpression)
	p.registerPrefix(lexer.TokenSlash, p.parseSlashLiteral)          // Add support for / as service path
	p.registerPrefix(lexer.TokenKwNew, p.parseNewExpression)         // Add support for new expressions
	p.registerPrefix(lexer.TokenLessThan, p.parseTypeCastExpression) // Added for <float>n

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
	p.registerInfix(lexer.TokenDot, p.parseMemberAccessExpression)
	p.registerInfix(lexer.TokenKwIs, p.parseIsExpression)

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

// peekToken returns the next token without advancing the token position.
func (p *Parser) peekToken() lexer.Token {
	if p.pos+1 >= len(p.tokens) {
		line, col := -1, -1
		if len(p.tokens) > 0 {
			last := p.tokens[len(p.tokens)-1]
			line = last.Line
			col = last.Column + len(last.Literal)
		}
		return lexer.Token{Type: lexer.TokenEOF, Literal: "", Line: line, Column: col}
	}
	return p.tokens[p.pos+1]
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
	fmt.Printf("DEBUG: ParseFile started\n")
	fileNode := &FileNode{Token: p.currentToken()}
	// Initialize slices to prevent nil pointer issues if file is empty
	fileNode.Imports = []*ImportNode{}
	fileNode.Definitions = []Node{}

	for p.currentToken().Type != lexer.TokenEOF {
		fmt.Printf("DEBUG: ParseFile processing token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
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
			fmt.Printf("DEBUG: ParseFile - handling public/function, checking if init function\n")
			// Check if this is an init function
			if currentTokenForSwitch.Type == lexer.TokenKwFunction &&
				(p.peekToken().Type == lexer.TokenKwInit || p.peekToken().Literal == "init") {
				fmt.Printf("DEBUG: ParseFile - detected init function\n")
				initFn := p.parseInitFunction()
				if initFn != nil {
					fileNode.Definitions = append(fileNode.Definitions, initFn)
				} else {
					parsedSuccessfully = false // parseInitFunction failed
				}
			} else {
				fmt.Printf("DEBUG: ParseFile - parsing regular function definition\n")
				fn := p.parseFunctionDefinition()
				if fn != nil {
					fileNode.Definitions = append(fileNode.Definitions, fn)
				} else {
					parsedSuccessfully = false // parseFunctionDefinition failed
				}
			}
		case lexer.TokenKwInit:
			initFn := p.parseInitFunction()
			if initFn != nil {
				fileNode.Definitions = append(fileNode.Definitions, initFn)
			} else {
				parsedSuccessfully = false // parseInitFunction failed
			}
		case lexer.TokenKwService:
			svc := p.parseServiceDeclaration()
			if svc != nil {
				fileNode.Definitions = append(fileNode.Definitions, svc)
			} else {
				parsedSuccessfully = false // parseServiceDeclaration failed
			}
		// Check for global variables
		case lexer.TokenKwInt, lexer.TokenKwFloat, lexer.TokenKwstring, lexer.TokenKwBoolean, lexer.TokenKwFinal:
			// Parse global variable declaration
			globalVar := p.parseGlobalVariable()
			if globalVar != nil {
				fileNode.Definitions = append(fileNode.Definitions, globalVar)
			} else {
				parsedSuccessfully = false
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
	fmt.Printf("DEBUG: ParseFile completed successfully\n")
	return fileNode, nil
}

// Added support for top-level variable declarations
func (p *Parser) parseDefinition() Node {
	fmt.Printf("DEBUG: parseDefinition - current token: %s (%s)\n", p.currentToken().Type, p.currentToken().Literal)

	switch p.currentToken().Type {
	case lexer.TokenKwImport:
		return p.parseImportStatement()
	case lexer.TokenKwPublic:
		// Check if this is a public function
		if p.peekToken().Type == lexer.TokenKwFunction {
			return p.parseFunctionDefinition()
		}
		// Handle other public declarations if needed
		fmt.Printf("WARNING: Unhandled public declaration\n")
		return nil
	case lexer.TokenKwFunction:
		return p.parseFunctionDefinition()
	case lexer.TokenKwService:
		return p.parseServiceDeclaration()
	case lexer.TokenKwInit:
		return p.parseInitFunction()

	// Handle type keywords that start variable declarations
	case lexer.TokenKwInt, lexer.TokenKwFloat, lexer.TokenKwBoolean, lexer.TokenKwstring:
		fmt.Printf("DEBUG: parseDefinition - found type keyword, parsing global variable declaration\n")
		return p.parseGlobalVariable()

	default:
		fmt.Printf("ERROR: parseDefinition - unexpected token: %s (%s)\n", p.currentToken().Type, p.currentToken().Literal)
		// Try to recover by skipping the current token
		p.nextToken()
		return nil
	}
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

	// Special case for error constructor
	if current.Literal == "error" && p.peekToken().Type == lexer.TokenLParen {
		// This is an error constructor like error("message")
		errorToken := current
		p.nextToken() // Consume 'error'
		p.nextToken() // Consume '('

		// Parse the message (should be a string)
		messageExpr := p.parseExpression(Lowest)
		if messageExpr == nil {
			p.errorUnexpectedToken("expected message in error constructor", p.currentToken())
			return nil
		}

		// Consume ')'
		if !p.expectToken(lexer.TokenRParen) {
			return nil
		}

		// Create a specialized call expression for the error constructor
		return &CallExpressionNode{
			Token: errorToken,
			Function: &IdentifierNode{
				Token: errorToken,
				Value: "error",
			},
			Arguments: []Expression{messageExpr},
		}
	}

	// Regular identifier
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

func (p *Parser) parseFloatLiteral() Expression {
	fmt.Printf("DEBUG: parseFloatLiteral called with token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
	tok := p.currentToken() // Store current token before advancing
	lit := &FloatLiteralNode{Token: tok}

	// Pre-process the literal to remove 'f' or 'F' suffix if present
	// This is a workaround for the lexer including it. Ideally, lexer provides clean literal.
	floatStr := tok.Literal
	if strings.HasSuffix(floatStr, "f") || strings.HasSuffix(floatStr, "F") {
		floatStr = floatStr[:len(floatStr)-1]
	}

	value, err := strconv.ParseFloat(floatStr, 64)
	if err != nil {
		msg := fmt.Sprintf("line %d, col %d: could not parse %q as float (original: %q)",
			tok.Line, tok.Column, floatStr, tok.Literal)
		p.errors = append(p.errors, msg)
		p.nextToken() // CONSUME THE PROBLEMATIC TOKEN!
		fmt.Printf("DEBUG: parseFloatLiteral error, advancing token\n")
		return nil
	}

	lit.Value = value
	p.nextToken() // Consume the (now successfully parsed) token
	fmt.Printf("DEBUG: parseFloatLiteral success, token consumed\n")
	return lit
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

	// Check for type cast like (<float>)n
	if p.isTypeToken(p.currentToken()) && p.peekToken().Type == lexer.TokenRParen {
		// This is a type cast expression like (<float>)n
		// We need to backtrack and parse it as a type cast expression
		// For now, let's assume this specific syntax (<type>)expr is handled by parseTypeCastExpression
		// by registering '<' as a prefix operator.
		// This specific path for `(<type>)` might need a dedicated prefix function if it becomes ambiguous.
	}

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

func (p *Parser) parseTypeCastExpression() Expression {
	startToken := p.currentToken() // Should be '<'

	if startToken.Type != lexer.TokenLessThan {
		p.errorExpectedToken(lexer.TokenLessThan, startToken)
		return nil
	}
	p.nextToken() // Consume '<'

	targetType := p.parseType()
	if targetType == nil {
		p.errorUnexpectedToken("expected type name in cast expression", p.currentToken())
		return nil
	}

	if !p.expectToken(lexer.TokenGreaterThan) {
		// Error already logged by expectToken
		return nil
	}

	// Now parse the expression to be casted
	// The precedence here should be high enough to bind tightly to the expression following the cast.
	// Using a precedence like 'Prefix' or 'Call' might be appropriate.
	expressionToCast := p.parseExpression(Prefix) // Using Prefix precedence
	if expressionToCast == nil {
		p.errorUnexpectedToken("expected expression after type cast", p.currentToken())
		return nil
	}

	return &TypeCastExpressionNode{
		Token:      startToken, // The '<' token
		TargetType: targetType,
		Expression: expressionToCast,
	}
}

// Helper function to check if a token is a type keyword
func (p *Parser) isTypeToken(tok lexer.Token) bool {
	switch tok.Type {
	case lexer.TokenKwInt, lexer.TokenKwFloat, lexer.TokenKwBoolean, lexer.TokenKwstring:
		return true
	default:
		// Could also include TokenIdentifier if we support custom types more broadly here
		return false
	}
}

func isTypeToken(t lexer.TokenType) bool {
	return t == lexer.TokenKwInt || t == lexer.TokenKwFloat || t == lexer.TokenKwBoolean // add others as needed
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
	accessToken := p.currentToken()
	p.nextToken() // Consume ':' or '.'

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
		Token:      accessToken,
		Expression: left,
		MemberName: memberName,
	}
}

func (p *Parser) parseIsExpression(left Expression) Expression {
	expr := &IsExpressionNode{
		Token: p.currentToken(),
		Left:  left,
	}
	p.nextToken() // move to the type keyword (e.g., float)
	if !isTypeToken(p.currentToken().Type) {
		p.errors = append(p.errors, fmt.Sprintf("expected type after 'is', got %s", p.currentToken().Literal))
		return nil
	}
	expr.Type = p.parseType()
	return expr
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
	fmt.Printf("DEBUG: parseFunctionDefinition - starting with token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
	stmt := &FunctionDefinitionNode{Token: p.currentToken()}

	if p.currentToken().Type == lexer.TokenKwPublic {
		stmt.Visibility = "public"
		p.nextToken()
		fmt.Printf("DEBUG: parseFunctionDefinition - consumed 'public', now at: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
	} else {
		stmt.Visibility = ""
	}

	if !p.expectToken(lexer.TokenKwFunction) {
		fmt.Printf("DEBUG: parseFunctionDefinition - failed to find 'function' token\n")
		return nil
	}
	fmt.Printf("DEBUG: parseFunctionDefinition - consumed 'function', now at: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)

	nameToken := p.currentToken()
	nameExpr := p.parseIdentifier()
	if nameExpr == nil {
		fmt.Printf("DEBUG: parseFunctionDefinition - parseIdentifier returned nil\n")
		return nil
	}
	nameIdent, ok := nameExpr.(*IdentifierNode)
	if !ok {
		p.errorUnexpectedToken("function name must be an identifier (internal parser error: unexpected type)", nameToken)
		return nil
	}
	stmt.Name = nameIdent
	fmt.Printf("DEBUG: parseFunctionDefinition - function name parsed: %s\n", nameIdent.Value)

	if !p.expectToken(lexer.TokenLParen) {
		fmt.Printf("DEBUG: parseFunctionDefinition - failed to find '(' token\n")
		return nil
	}
	fmt.Printf("DEBUG: parseFunctionDefinition - parsing parameters\n")
	stmt.Parameters = p.parseFunctionParameters()
	if stmt.Parameters == nil && len(p.errors) > 0 && p.currentToken().Type != lexer.TokenRParen {
		fmt.Printf("DEBUG: parseFunctionDefinition - parseFunctionParameters failed\n")
		return nil
	}
	fmt.Printf("DEBUG: parseFunctionDefinition - parameters parsed, expecting ')'\n")
	if !p.expectToken(lexer.TokenRParen) {
		fmt.Printf("DEBUG: parseFunctionDefinition - failed to find ')' token\n")
		return nil
	}
	fmt.Printf("DEBUG: parseFunctionDefinition - parameters completed\n")

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
	fmt.Printf("DEBUG: parseFunctionDefinition - parsing body, current token: %s (%s)\n", bodyToken.Literal, bodyToken.Type)
	if bodyToken.Type != lexer.TokenLBrace {
		p.errorExpectedToken(lexer.TokenLBrace, bodyToken)
		return nil
	}
	stmt.Body = p.parseBlockStatement()
	if stmt.Body == nil {
		fmt.Printf("DEBUG: parseFunctionDefinition - parseBlockStatement returned nil\n")
		return nil
	}
	fmt.Printf("DEBUG: parseFunctionDefinition - function definition completed successfully\n")

	return stmt
}

// parseInitFunction parses an init function declaration.
func (p *Parser) parseInitFunction() *InitFunctionNode {
	token := p.currentToken() // 'function' token or 'init' token

	// If starting with 'function', next token should be 'init'
	if token.Type == lexer.TokenKwFunction {
		p.nextToken()
		// Accept either TokenKwInit (keyword) or "init" as an identifier
		if p.currentToken().Type != lexer.TokenKwInit &&
			!(p.currentToken().Type == lexer.TokenIdentifier && p.currentToken().Literal == "init") {
			p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: expected init, got %s",
				p.currentToken().Line, p.currentToken().Column, p.currentToken().Type))
			return nil
		}
	}

	// Next token should be '('
	p.nextToken()
	if p.currentToken().Type != lexer.TokenLParen {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: expected (, got %s",
			p.currentToken().Line, p.currentToken().Column, p.currentToken().Type))
		return nil
	}

	// Next token should be ')'
	p.nextToken()
	if p.currentToken().Type != lexer.TokenRParen {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: expected ), got %s",
			p.currentToken().Line, p.currentToken().Column, p.currentToken().Type))
		return nil
	}
	p.nextToken() // Consume ')'

	// Check for return type
	var returnType *TypeNode
	if p.currentToken().Type == lexer.TokenIdentifier && p.currentToken().Literal == "returns" {
		p.nextToken() // Consume 'returns'

		// Parse the return type
		typeToken := p.currentToken()
		typeName := typeToken.Literal
		p.nextToken() // Consume the type name

		// Check for question mark (optional type)
		if p.currentToken().Type == lexer.TokenQuestionMark {
			typeName += "?"
			p.nextToken() // Consume the question mark
		}

		returnType = &TypeNode{Token: typeToken, TypeName: typeName}
	}

	// Parse the function body
	body := p.parseBlockStatement()
	if body == nil {
		return nil
	}

	return &InitFunctionNode{
		Token:      token,
		ReturnType: returnType,
		Body:       body,
	}
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
	token := p.currentToken()
	fmt.Printf("DEBUG: parseType - current token: %s (%s) at pos %d\n", token.Literal, token.Type, p.pos)
	var typeName string
	var isArray bool = false // Keep for potential future use, but not parsing [] syntax here
	var isNilable bool = false

	switch token.Type {
	case lexer.TokenKwInt:
		typeName = "int"
	case lexer.TokenKwFloat:
		typeName = "float"
	case lexer.TokenKwBoolean:
		typeName = "boolean"
	case lexer.TokenKwstring:
		typeName = "string"
	case lexer.TokenIdentifier: // For complex types like http:Listener or user-defined types
		// This could be a simple type or a qualified type like foo:Bar
		// For now, assume it's a simple identifier representing the type name.
		// More complex type parsing (qualified, generic, etc.) would go here.
		typeName = token.Literal
	default:
		p.errorUnexpectedToken(fmt.Sprintf("expected type name, got %s", token.Type), token)
		return nil
	}
	p.nextToken() // Consume the type token
	fmt.Printf("DEBUG: parseType - after consuming type token, now at: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)

	// Check for nilable operator '?'
	if p.currentToken().Type == lexer.TokenQuestionMark {
		isNilable = true
		p.nextToken() // Consume '?'
	}

	// NOTE: Array type parsing (e.g., int[]) is not handled here yet.
	// If it were, we would check for lexer.TokenLBracket here.

	return &TypeNode{Token: token, TypeName: typeName, IsArray: isArray, IsNilable: isNilable}
}

func (p *Parser) parseBlockStatement() *BlockStatementNode {
	blockToken := p.currentToken()
	fmt.Printf("DEBUG: parseBlockStatement - starting with token: %s (%s) at pos %d\n", blockToken.Literal, blockToken.Type, p.pos)
	block := &BlockStatementNode{Token: blockToken}
	if !p.expectToken(lexer.TokenLBrace) {
		fmt.Printf("DEBUG: parseBlockStatement - failed to find opening brace\n")
		return nil
	}
	fmt.Printf("DEBUG: parseBlockStatement - found opening brace, current token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)

	block.Statements = []Statement{}
	statementCount := 0
	for p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
		stmtStartToken := p.currentToken()
		fmt.Printf("DEBUG: parseBlockStatement - parsing statement %d, current token: %s (%s) at pos %d\n", statementCount+1, stmtStartToken.Literal, stmtStartToken.Type, p.pos)

		stmt := p.parseStatement()
		if stmt != nil {
			fmt.Printf("DEBUG: parseBlockStatement - successfully parsed statement %d of type %T\n", statementCount+1, stmt)
			block.Statements = append(block.Statements, stmt)
			statementCount++
		} else {
			fmt.Printf("DEBUG: parseBlockStatement - failed to parse statement %d, current token: %s (%s) at pos %d\n", statementCount+1, p.currentToken().Literal, p.currentToken().Type, p.pos)
			if p.currentToken() == stmtStartToken && p.currentToken().Type != lexer.TokenRBrace && p.currentToken().Type != lexer.TokenEOF {
				p.errorUnexpectedToken("failed to parse statement or parser stuck in block", p.currentToken())
				p.nextToken()
				fmt.Printf("DEBUG: parseBlockStatement - advanced token after error, now at: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
			}
		}

		// Safety check to prevent infinite loops
		if statementCount > 100 {
			fmt.Printf("DEBUG: parseBlockStatement - too many statements, breaking to prevent infinite loop\n")
			break
		}
	}

	fmt.Printf("DEBUG: parseBlockStatement - finished parsing statements, current token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
	if !p.expectToken(lexer.TokenRBrace) {
		fmt.Printf("DEBUG: parseBlockStatement - failed to find closing brace\n")
		return nil
	}
	fmt.Printf("DEBUG: parseBlockStatement - successfully completed with %d statements\n", len(block.Statements))
	return block
}

func (p *Parser) parseStatement() Statement {
	fmt.Printf("DEBUG: parseStatement - current token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
	switch p.currentToken().Type {
	case lexer.TokenKwVar, lexer.TokenKwFloat, lexer.TokenKwInt, lexer.TokenKwBoolean, lexer.TokenKwstring:
		fmt.Printf("DEBUG: parseStatement - detected variable declaration\n")
		return p.parseVariableDeclarationStatement()
	case lexer.TokenKwReturn:
		fmt.Printf("DEBUG: parseStatement - detected return statement\n")
		return p.parseReturnStatement()
	case lexer.TokenKwIf:
		fmt.Printf("DEBUG: parseStatement - detected if statement\n")
		return p.parseIfStatement() // Ensure this advances the token
	// ...other cases like while, for, etc. as needed...
	default:
		fmt.Printf("DEBUG: parseStatement - trying to parse as expression statement\n")
		// Instead of just advancing the token, try to parse as an expression statement
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseVariableDeclarationStatement() *VariableDeclarationNode {
	fmt.Printf("DEBUG: parseVariableDeclarationStatement - starting with token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
	stmt := &VariableDeclarationNode{Token: p.currentToken()}

	if p.currentToken().Type == lexer.TokenKwVar {
		varToken := p.currentToken()
		p.nextToken()
		stmt.Type = &TypeNode{Token: varToken, TypeName: "var"}
		fmt.Printf("DEBUG: parseVariableDeclarationStatement - parsed var type\n")
	} else {
		typeToken := p.currentToken()
		fmt.Printf("DEBUG: parseVariableDeclarationStatement - parsing type: %s (%s)\n", typeToken.Literal, typeToken.Type)
		parsedType := p.parseType()
		if parsedType == nil {
			p.errorUnexpectedToken("expected type name in variable declaration", typeToken)
			return nil
		}
		stmt.Type = parsedType
		fmt.Printf("DEBUG: parseVariableDeclarationStatement - type parsed successfully: %s\n", parsedType.TypeName)
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

// parseGlobalVariable parses a global variable declaration.
func (p *Parser) parseGlobalVariable() *GlobalVariableNode {
	fmt.Printf("DEBUG: parseGlobalVariable - starting with token: %s (%s) at pos %d\n", p.currentToken().Literal, p.currentToken().Type, p.pos)
	var node GlobalVariableNode

	// Check if the variable is final
	if p.currentToken().Type == lexer.TokenKwFinal {
		node.Token = p.currentToken()
		node.IsFinal = true
		p.nextToken() // Consume 'final'
	} else {
		node.Token = p.currentToken() // Should be a type token
	}

	// Parse the type
	fmt.Printf("DEBUG: parseGlobalVariable - parsing type...\n")
	typeNode := p.parseType()
	if typeNode == nil {
		fmt.Printf("DEBUG: parseGlobalVariable - parseType returned nil\n")
		return nil
	}
	node.Type = typeNode
	fmt.Printf("DEBUG: parseGlobalVariable - type parsed successfully: %s\n", typeNode.TypeName)

	// Parse the variable name
	fmt.Printf("DEBUG: parseGlobalVariable - parsing variable name, current token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
	if p.currentToken().Type != lexer.TokenIdentifier {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: expected identifier, got %s",
			p.currentToken().Line, p.currentToken().Column, p.currentToken().Type))
		return nil
	}
	node.Name = &IdentifierNode{Token: p.currentToken(), Value: p.currentToken().Literal}
	p.nextToken() // Consume identifier
	fmt.Printf("DEBUG: parseGlobalVariable - variable name parsed: %s\n", node.Name.Value)

	// Check for initializer
	fmt.Printf("DEBUG: parseGlobalVariable - checking for initializer, current token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
	if p.currentToken().Type == lexer.TokenEqual {
		p.nextToken() // Consume '='
		fmt.Printf("DEBUG: parseGlobalVariable - parsing initializer expression, current token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
		initializer := p.parseExpression(Lowest)
		if initializer == nil {
			fmt.Printf("DEBUG: parseGlobalVariable - parseExpression returned nil\n")
			return nil
		}
		node.Initializer = initializer
		fmt.Printf("DEBUG: parseGlobalVariable - initializer parsed successfully\n")
	}

	// Expect semicolon
	fmt.Printf("DEBUG: parseGlobalVariable - checking for semicolon, current token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)
	if p.currentToken().Type != lexer.TokenSemicolon {
		p.errors = append(p.errors, fmt.Sprintf("line %d, col %d: expected semicolon, got %s",
			p.currentToken().Line, p.currentToken().Column, p.currentToken().Type))
		return nil
	}
	p.nextToken() // Consume ';'
	fmt.Printf("DEBUG: parseGlobalVariable - semicolon consumed, now at token: %s (%s)\n", p.currentToken().Literal, p.currentToken().Type)

	fmt.Printf("DEBUG: parseGlobalVariable - returning successfully\n")
	return &node
}
