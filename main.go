// main.go
package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"wx-yz/lightfoot/backend"
	"wx-yz/lightfoot/bir"   // Assuming this package exists and is structured as expected
	"wx-yz/lightfoot/lexer" // Assuming a lexer package
	"wx-yz/lightfoot/parser"
)

func main() {
	// Parse command line arguments
	inputFile := flag.String("input", "", "Input Ballerina source file")
	flag.Parse()

	if *inputFile == "" {
		log.Fatal("Input file is required. Use --input=<filename>")
	}

	// Read the input Ballerina file
	sourceCode, err := os.ReadFile(*inputFile)
	if err != nil {
		log.Fatalf("Failed to read input file %s: %v", *inputFile, err)
	}

	// Lex the source code
	lexer := lexer.NewLexer(string(sourceCode)) // Assuming lexer.NewLexer exists
	tokens, lexErr := lexer.Lex()               // Assuming lexer.Lex() ([]lexer.Token, error) exists
	if lexErr != nil {
		log.Fatalf("Lexing failed: %v", lexErr)
	}

	// Debug: Print all tokens
	fmt.Printf("DEBUG: Generated %d tokens:\n", len(tokens))
	for i, token := range tokens {
		fmt.Printf("  %d: %s (%s) at line %d, col %d\n", i, token.Literal, token.Type, token.Line, token.Column)
	}

	// Parse the tokens
	parser := parser.NewParser(tokens)  // Assuming parser.NewParser takes []lexer.Token
	ast, parseErr := parser.ParseFile() // Changed to ParseFile()
	if parseErr != nil {
		log.Fatalf("Parsing failed: %v", parseErr)
	}

	// BIR Generation using the proper Emitter
	birEmitter := bir.NewEmitter()
	birPackage, birErr := birEmitter.Emit(ast)
	if birErr != nil {
		log.Fatalf("BIR generation failed: %v", birErr)
	}

	// Create a new code generator
	codeGen := backend.NewCodeGenerator(birPackage) // birPackage would be the result of previous steps

	// Generate LLVM IR
	llvmIR, err := codeGen.Generate() // Assumes Generate() (string, error) exists in CodeGenerator
	if err != nil {
		log.Fatalf("Failed to generate LLVM IR: %v", err)
	}

	// Define output filenames
	baseFilename := strings.TrimSuffix(*inputFile, filepath.Ext(*inputFile))
	objectFile := baseFilename + ".o"
	executableFile := baseFilename

	// Write LLVM IR to a .ll file (optional, for debugging)
	llFile := baseFilename + ".ll"
	if err := os.WriteFile(llFile, []byte(llvmIR), 0644); err != nil {
		log.Printf("Warning: Failed to write LLVM IR file %s: %v", llFile, err)
	}

	// Compile LLVM IR (.ll file) to an object file (.o) using llc
	llcCmd := exec.Command("llc", "-filetype=obj", "-o", objectFile, llFile)
	llcOutput, err := llcCmd.CombinedOutput()
	if err != nil {
		log.Fatalf("llc compilation failed: %v\nOutput:\n%s", err, string(llcOutput))
	}
	fmt.Printf("llc compilation successful. Object file: %s\n", objectFile)

	// Link the object file with the runtime to create the executable
	err = backend.LinkObjectFile(objectFile, executableFile)
	if err != nil {
		log.Fatalf("Linking failed: %v", err)
	}

	fmt.Printf("Compilation successful. Executable: %s\n", executableFile)
}
