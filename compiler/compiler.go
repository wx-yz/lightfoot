// compiler/compiler.go
package compiler

import (
	"fmt"
	"wx-yz/lightfoot/bir"
	"wx-yz/lightfoot/lexer"
	"wx-yz/lightfoot/parser"
)

// Compiler orchestrates the compilation process.
type Compiler struct{}

// NewCompiler creates a new Compiler instance.
func NewCompiler() *Compiler {
	return &Compiler{}
}

// Compile takes Ballerina source code as input and returns a BIR package or an error.
func (c *Compiler) Compile(source string) (*bir.Package, error) {
	l := lexer.NewLexer(source)
	tokens, err := l.Lex()
	if err != nil {
		return nil, fmt.Errorf("lexing error: %w", err)
	}

	// For debugging: print tokens
	// fmt.Println("Tokens:")
	// for _, token := range tokens {
	// 	fmt.Printf("%+v\n", token)
	// }
	// fmt.Println("---")

	p := parser.NewParser(tokens)
	astFile, err := p.ParseFile()
	if err != nil {
		return nil, fmt.Errorf("parsing error: %w", err)
	}

	// For debugging: print AST (simplified)
	// fmt.Println("AST:")
	// ast.PrintAST(astFile, 0) // Assuming you add a PrintAST function in ast package
	// fmt.Println("---")

	birEmitter := bir.NewEmitter()
	birPackage, err := birEmitter.Emit(astFile)
	if err != nil {
		return nil, fmt.Errorf("BIR emission error: %w", err)
	}

	return birPackage, nil
}
