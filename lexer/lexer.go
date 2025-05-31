// lexer/lexer.go
package lexer

import (
	"fmt"
	"strings" // Ensure strings is imported
	"unicode"
	"unicode/utf8"
)

// TokenType represents the type of a token.
type TokenType int

const (
	TokenEOF TokenType = iota
	TokenError
	TokenIdentifier         // main, x, println
	TokenIntLiteral         // 123, 0, 99
	TokenFloatLiteral       // 1.23, 0.5, 99.99f
	TokenStringLiteral      // "hello"
	TokenBooleanLiteral     // true, false
	TokenSemicolon          // ;
	TokenColon              // :
	TokenComma              // ,
	TokenLParen             // (
	TokenRParen             // )
	TokenLBrace             // {
	TokenRBrace             // }
	TokenPlus               // +
	TokenMinus              // -
	TokenAsterisk           // *
	TokenSlash              // /
	TokenEqual              // =
	TokenEqualEqual         // ==
	TokenNotEqual           // !=
	TokenLessThan           // <
	TokenLessThanOrEqual    // <=
	TokenGreaterThan        // >
	TokenGreaterThanOrEqual // >=
	TokenDot                // .
	TokenQuestionMark       // ?

	// Keywords
	keywordStart
	TokenKwImport
	TokenKwFunction
	TokenKwPublic
	TokenKwReturn
	TokenKwInt
	TokenKwFloat
	TokenKwBoolean
	TokenKwstring // Note: string is a predefined type, not a strict keyword in all contexts, but treated as such for simplicity
	TokenKwIf
	TokenKwElse
	TokenKwVar      // for inferred type (not fully implemented in this subset)
	TokenKwService  // for service declarations
	TokenKwResource // for resource functions
	TokenKwOn       // for service attachment (service / on new http:Listener())
	TokenKwNew      // for new object creation
	TokenKwInit     // for init functions
	TokenKwFinal    // for final keyword
	TokenKwIs       // for 'is' keyword
	keywordEnd
)

// String returns a string representation of the token type.
func (t TokenType) String() string {
	switch t {
	case TokenEOF:
		return "EOF"
	case TokenError:
		return "Error"
	case TokenIdentifier:
		return "Identifier"
	case TokenIntLiteral:
		return "IntLiteral"
	case TokenFloatLiteral:
		return "FloatLiteral"
	case TokenStringLiteral:
		return "StringLiteral"
	case TokenBooleanLiteral:
		return "BooleanLiteral"
	case TokenSemicolon:
		return "Semicolon"
	case TokenColon:
		return "Colon"
	case TokenComma:
		return "Comma"
	case TokenLParen:
		return "LParen"
	case TokenRParen:
		return "RParen"
	case TokenLBrace:
		return "LBrace"
	case TokenRBrace:
		return "RBrace"
	case TokenPlus:
		return "Plus"
	case TokenMinus:
		return "Minus"
	case TokenAsterisk:
		return "Asterisk"
	case TokenSlash:
		return "Slash"
	case TokenEqual:
		return "Equal"
	case TokenEqualEqual:
		return "EqualEqual"
	case TokenNotEqual:
		return "NotEqual"
	case TokenLessThan:
		return "LessThan"
	case TokenLessThanOrEqual:
		return "LessThanOrEqual"
	case TokenGreaterThan:
		return "GreaterThan"
	case TokenGreaterThanOrEqual:
		return "GreaterThanOrEqual"
	case TokenDot:
		return "Dot"
	case TokenQuestionMark:
		return "QuestionMark"
	case TokenKwImport:
		return "KwImport"
	case TokenKwFunction:
		return "KwFunction"
	case TokenKwPublic:
		return "KwPublic"
	case TokenKwReturn:
		return "KwReturn"
	case TokenKwInt:
		return "KwInt"
	case TokenKwFloat:
		return "KwFloat"
	case TokenKwBoolean:
		return "KwBoolean"
	case TokenKwstring:
		return "Kwstring"
	case TokenKwIf:
		return "KwIf"
	case TokenKwElse:
		return "KwElse"
	case TokenKwVar:
		return "KwVar"
	case TokenKwService:
		return "KwService"
	case TokenKwResource:
		return "KwResource"
	case TokenKwOn:
		return "KwOn"
	case TokenKwNew:
		return "KwNew"
	case TokenKwInit:
		return "KwInit"
	case TokenKwFinal:
		return "KwFinal"
	case TokenKwIs:
		return "KwIs"
	default:
		return fmt.Sprintf("UnknownToken(%d)", t)
	}
}

// Token represents a lexical token.
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

// Lexer transforms a string of source code into a slice of Tokens.
type Lexer struct {
	source  string
	pos     int  // current position in input (points to current char)
	readPos int  // current reading position in input (after current char)
	char    rune // current char under examination
	line    int
	column  int
}

var keywords = map[string]TokenType{
	"import":   TokenKwImport,
	"function": TokenKwFunction,
	"public":   TokenKwPublic,
	"return":   TokenKwReturn,
	"int":      TokenKwInt,
	"float":    TokenKwFloat,
	"boolean":  TokenKwBoolean,
	"string":   TokenKwstring,
	"if":       TokenKwIf,
	"else":     TokenKwElse,
	"true":     TokenBooleanLiteral,
	"false":    TokenBooleanLiteral,
	"var":      TokenKwVar,
	"service":  TokenKwService,
	"resource": TokenKwResource,
	"on":       TokenKwOn,
	"new":      TokenKwNew,
	"init":     TokenKwInit,
	"final":    TokenKwFinal,
	"is":       TokenKwIs,
}

// NewLexer creates a new Lexer.
func NewLexer(source string) *Lexer {
	l := &Lexer{source: source, line: 1, column: 0}
	l.readChar() // Initialize l.char, l.pos, l.readPos
	return l
}

func (l *Lexer) readChar() {
	if l.readPos >= len(l.source) {
		l.char = 0 // EOF
	} else {
		r, size := utf8.DecodeRuneInString(l.source[l.readPos:])
		l.char = r
		// Correctly advance readPos by the size of the rune
		l.readPos += size
	}
	l.pos = l.readPos - utf8.RuneLen(l.char) // Update pos to the start of the current char
	if l.char == 0 {                         // If EOF, ensure pos doesn't go out of bounds if source is empty
		l.pos = len(l.source)
	}
	l.column++
}

func (l *Lexer) peekChar() rune {
	if l.readPos >= len(l.source) {
		return 0
	}
	r, _ := utf8.DecodeRuneInString(l.source[l.readPos:])
	return r
}

func (l *Lexer) skipWhitespaceAndComments() {
	for {
		switch l.char {
		case ' ', '\t', '\r':
			l.readChar()
		case '\n':
			l.line++
			l.column = 0 // Reset column before reading next char
			l.readChar()
		case '/':
			if l.peekChar() == '/' {
				// Single line comment
				l.readChar() // consume the second '/'
				l.readChar()
				for l.char != '\n' && l.char != 0 {
					l.readChar()
				}
				if l.char == '\n' { // consume the newline as well
					l.line++
					l.column = 0
					l.readChar()
				}
			} else {
				return // Not a comment, might be a division operator
			}
		default:
			return
		}
	}
}

func (l *Lexer) readIdentifier() string {
	startPos := l.pos
	// Ballerina identifiers can start with a letter or underscore
	if !isLetter(l.char) && l.char != '_' {
		return "" // Should not happen if called correctly
	}
	l.readChar() // Consume the first character

	for isLetter(l.char) || isDigit(l.char) || l.char == '_' {
		l.readChar()
	}
	return l.source[startPos:l.pos]
}

func (l *Lexer) readNumber() string {
	startPos := l.pos
	// Read initial digits
	for isDigit(l.char) {
		l.readChar()
	}

	// Check for decimal point followed by digits
	if l.char == '.' && isDigit(l.peekChar()) {
		l.readChar() // consume the decimal point
		for isDigit(l.char) {
			l.readChar()
		}
	}

	// Check for float suffix 'f' or 'F'
	// This can appear after digits (e.g., 123f) or after a decimal part (e.g., 1.23f)
	if l.char == 'f' || l.char == 'F' {
		// Check if the character before 'f'/'F' was a digit or a decimal point
		// to ensure it's a valid float suffix and not part of an identifier.
		// This check is simplified; a more robust check might be needed if identifiers can contain 'f' or 'F'.
		// For now, we assume if readNumber is called, it's in a number context.
		l.readChar() // consume the suffix
	}

	numberStr := l.source[startPos:l.pos]
	return numberStr
}

func (l *Lexer) readString() (string, error) {
	startPos := l.pos + 1 // Skip the opening quote
	l.readChar()          // Consume opening quote
	for {
		if l.char == '"' {
			break
		}
		if l.char == '\\' { // Handle escape sequences (simplified)
			l.readChar() // Consume '\'
			if l.char == 0 {
				return "", fmt.Errorf("unterminated string literal at line %d, col %d", l.line, l.column)
			}
			// For simplicity, we just consume the escaped character.
			// A full implementation would handle specific escapes like \n, \t, \", \\, etc.
		}
		if l.char == 0 || l.char == '\n' { // Unterminated string
			return "", fmt.Errorf("unterminated string literal at line %d, col %d", l.line, l.column)
		}
		l.readChar()
	}
	endPos := l.pos
	l.readChar() // Consume closing quote
	return l.source[startPos:endPos], nil
}

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return unicode.IsDigit(ch)
}

// Lex performs lexical analysis and returns a list of tokens.
func (l *Lexer) Lex() ([]Token, error) {
	var tokens []Token

	for {
		l.skipWhitespaceAndComments()

		tok := Token{Line: l.line, Column: l.column}

		switch l.char {
		case 0:
			tok.Type = TokenEOF
			tok.Literal = ""
			tokens = append(tokens, tok)
			return tokens, nil
		case ';':
			tok.Type = TokenSemicolon
			tok.Literal = ";"
			l.readChar()
		case ':':
			tok.Type = TokenColon
			tok.Literal = ":"
			l.readChar()
		case ',':
			tok.Type = TokenComma
			tok.Literal = ","
			l.readChar()
		case '(':
			tok.Type = TokenLParen
			tok.Literal = "("
			l.readChar()
		case ')':
			tok.Type = TokenRParen
			tok.Literal = ")"
			l.readChar()
		case '{':
			tok.Type = TokenLBrace
			tok.Literal = "{"
			l.readChar()
		case '}':
			tok.Type = TokenRBrace
			tok.Literal = "}"
			l.readChar()
		case '+':
			tok.Type = TokenPlus
			tok.Literal = "+"
			l.readChar()
		case '-':
			tok.Type = TokenMinus
			tok.Literal = "-"
			l.readChar()
		case '*':
			tok.Type = TokenAsterisk
			tok.Literal = "*"
			l.readChar()
		case '?':
			tok.Type = TokenQuestionMark
			tok.Literal = "?"
			l.readChar()
		case '/':
			// Check for single line comment again (handled in skipWhitespace, but good for robustness)
			if l.peekChar() == '/' {
				l.skipWhitespaceAndComments()
				continue // Restart tokenization after skipping comment
			}
			tok.Type = TokenSlash
			tok.Literal = "/"
			l.readChar()
		case '.':
			tok.Type = TokenDot
			tok.Literal = "."
			l.readChar()
		case '=':
			if l.peekChar() == '=' {
				l.readChar()
				tok.Type = TokenEqualEqual
				tok.Literal = "=="
			} else {
				tok.Type = TokenEqual
				tok.Literal = "="
			}
			l.readChar()
		case '!':
			if l.peekChar() == '=' {
				l.readChar()
				tok.Type = TokenNotEqual
				tok.Literal = "!="
				l.readChar()
			} else {
				// Ballerina doesn't use '!' alone as an operator in this basic subset
				tok.Type = TokenError
				tok.Literal = string(l.char)
				err := fmt.Errorf("unexpected character: %q at line %d, col %d", l.char, l.line, l.column)
				l.readChar()
				tokens = append(tokens, tok)
				return tokens, err

			}
		case '<':
			if l.peekChar() == '=' {
				l.readChar()
				tok.Type = TokenLessThanOrEqual
				tok.Literal = "<="
			} else {
				tok.Type = TokenLessThan
				tok.Literal = "<"
			}
			l.readChar()
		case '>':
			if l.peekChar() == '=' {
				l.readChar()
				tok.Type = TokenGreaterThanOrEqual
				tok.Literal = ">="
			} else {
				tok.Type = TokenGreaterThan
				tok.Literal = ">"
			}
			l.readChar()
		case '"':
			literal, err := l.readString()
			if err != nil {
				tok.Type = TokenError
				tok.Literal = "ERROR" // Placeholder for error token literal
				// The error from readString already has line/col info
				tokens = append(tokens, tok)
				return tokens, err
			}
			tok.Type = TokenStringLiteral
			tok.Literal = literal
		default:
			if isLetter(l.char) || l.char == '_' {
				ident := l.readIdentifier()
				if keywordType, isKeyword := keywords[ident]; isKeyword {
					tok.Type = keywordType
					if keywordType == TokenBooleanLiteral { // "true" or "false"
						tok.Literal = ident
					} else {
						tok.Literal = ident // Store the keyword itself
					}
				} else {
					tok.Type = TokenIdentifier
					tok.Literal = ident
				}
			} else if isDigit(l.char) || (l.char == '.' && isDigit(l.peekChar())) { // Modified condition
				numberStr := l.readNumber()
				// Determine if this is an int or float based on the literal
				if strings.ContainsAny(numberStr, ".fF") {
					tok.Type = TokenFloatLiteral
				} else {
					tok.Type = TokenIntLiteral
				}
				tok.Literal = numberStr
			} else {
				tok.Type = TokenError
				tok.Literal = string(l.char)
				err := fmt.Errorf("unexpected character: %q at line %d, col %d", l.char, l.line, l.column)
				l.readChar()
				tokens = append(tokens, tok)
				return tokens, err
			}
		}
		tokens = append(tokens, tok)
	}
}
