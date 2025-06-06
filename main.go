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
	"time"
	"wx-yz/lightfoot/backend"
	"wx-yz/lightfoot/bir" // Assuming this package exists and is structured as expected
	"wx-yz/lightfoot/debug"
	"wx-yz/lightfoot/lexer" // Assuming a lexer package
	"wx-yz/lightfoot/parser"
)

// Global debug flags
var (
	DebugLexer   bool
	DebugParser  bool
	DebugBIR     bool
	DebugBackend bool
	DebugLLVM    bool
	DebugAll     bool
	Verbose      bool
	SaveIR       bool
	DumpSymbols  bool
	ShowOutput   bool
)

func main() {
	// Parse command line arguments
	inputFile := flag.String("input", "", "Input Ballerina source file")
	help := flag.Bool("help", false, "Show help message")

	// Debug flags
	flag.BoolVar(&DebugLexer, "debug-lexer", false, "Enable lexer debug output")
	flag.BoolVar(&DebugParser, "debug-parser", false, "Enable parser debug output")
	flag.BoolVar(&DebugBIR, "debug-bir", false, "Enable BIR generation debug output")
	flag.BoolVar(&DebugBackend, "debug-backend", false, "Enable backend/codegen debug output")
	flag.BoolVar(&DebugLLVM, "debug-llvm", false, "Enable LLVM compilation debug output")
	flag.BoolVar(&DebugAll, "debug-all", false, "Enable all debug output")
	flag.BoolVar(&Verbose, "verbose", false, "Enable verbose output")
	flag.BoolVar(&SaveIR, "save-ir", false, "Save intermediate LLVM IR files")
	flag.BoolVar(&DumpSymbols, "dump-symbols", false, "Dump object file symbols")
	flag.BoolVar(&ShowOutput, "show-output", false, "Show compilation command outputs")

	dumpTokens := flag.Bool("dump-tokens", false, "Dump tokens after lexing")
	dumpAst := flag.Bool("dump-ast", false, "Dump AST after parsing")
	dumpBir := flag.Bool("dump-bir", false, "Dump BIR after generation")
	dumpLlvm := flag.Bool("dump-llvm", false, "Dump LLVM IR before compilation")
	traceInstructions := flag.Bool("trace-instructions", false, "Enable instruction-level tracing")
	detectTestCases := flag.Bool("detect-test-cases", false, "Automatically detect test case patterns")
	showDetection := flag.Bool("show-detection", false, "Show test case detection reasoning")
	logFile := flag.String("log-file", "", "Write debug output to a log file")

	flag.Parse()

	// If debug-all is set, enable all debug flags
	if DebugAll {
		DebugLexer = true
		DebugParser = true
		DebugBIR = true
		DebugBackend = true
		DebugLLVM = true
		Verbose = true
		SaveIR = true
		DumpSymbols = true
		ShowOutput = true
		*dumpTokens = true
		*dumpAst = true
		*dumpBir = true
		*dumpLlvm = true
		*traceInstructions = true
		*detectTestCases = true
		*showDetection = true
	}

	// Initialize debug flags
	debug.SetFlags(DebugLexer, DebugParser, DebugBIR, DebugBackend, DebugLLVM, DebugAll, Verbose)
	debug.SetTraceInstructions(*traceInstructions)

	// Show active debug flags if verbose mode is enabled
	debug.PrintActiveFlags()

	// Enable file logging if requested
	if *logFile != "" || DebugAll {
		logFilename := *logFile
		if logFilename == "" && DebugAll {
			// Create default log filename
			logFilename = fmt.Sprintf("lightfoot_debug_%s.log",
				time.Now().Format("20060102_150405"))
		}

		err := debug.EnableFileLogging(logFilename)
		if err != nil {
			log.Printf("Warning: Failed to enable debug logging: %v", err)
		} else if Verbose {
			debug.PrintVerbose("Debug logging enabled to file: %s", logFilename)
		}
	}

	// Check if help flag is present
	if *help {
		fmt.Println("Lightfoot Ballerina Compiler")
		fmt.Println("============================")
		fmt.Println()
		flag.Usage()
		fmt.Println()
		debug.PrintDebugUsage()
		os.Exit(0)
	}

	if *inputFile == "" {
		fmt.Println("Error: Input file is required.")
		fmt.Println()
		fmt.Println("Usage:")
		flag.Usage()
		fmt.Println()
		debug.PrintDebugUsage()
		os.Exit(1)
	}

	debug.PrintVerbose("Starting compilation of %s", *inputFile)
	debug.PrintCompilerPhase("LEXICAL ANALYSIS")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting lexical analysis", "input_file", *inputFile)
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

	// Debug: Print all tokens (only if debug flags are enabled)
	if DebugLexer || *traceInstructions {
		debug.PrintLexer("Generated %d tokens:", len(tokens))
		for i, token := range tokens {
			debug.PrintLexer("  %d: %s (%s) at line %d, col %d", i, token.Literal, token.Type, token.Line, token.Column)
		}
	}

	// Dump tokens if requested
	if *dumpTokens {
		tokenStrings := make([]string, len(tokens))
		for i, token := range tokens {
			tokenStrings[i] = fmt.Sprintf("%s (%s) at line %d, col %d", token.Literal, token.Type, token.Line, token.Column)
		}
		debug.DumpToFile(strings.Join(tokenStrings, "\n"), "tokens", "txt")
	}

	debug.PrintCompilerPhase("SYNTAX ANALYSIS")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting syntax analysis", "token_count", len(tokens))
	}

	// Parse the tokens
	parser := parser.NewParser(tokens)  // Assuming parser.NewParser takes []lexer.Token
	ast, parseErr := parser.ParseFile() // Changed to ParseFile()
	if parseErr != nil {
		log.Fatalf("Parsing failed: %v", parseErr)
	}

	debug.PrintVerbose("Parsing completed successfully")

	// Debug: Print AST details if parser debugging is enabled
	if DebugParser || *traceInstructions {
		debug.PrintParser("AST parsing completed, analyzing structure...")
		if ast != nil {
			debug.PrintParser("AST contains definitions and imports")
		}
	}

	// Dump AST if requested
	if *dumpAst {
		astStr := fmt.Sprintf("%+v", ast) // Basic representation, could be improved
		debug.DumpToFile(astStr, "ast", "txt")
	}

	debug.PrintCompilerPhase("BIR GENERATION")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting BIR generation", "ast_parsed", true)
	}

	// BIR Generation using the proper Emitter
	birEmitter := bir.NewEmitter()
	birPackage, birErr := birEmitter.Emit(ast)
	if birErr != nil {
		log.Fatalf("BIR generation failed: %v", birErr)
	}

	debug.PrintVerbose("BIR generation completed")

	// Debug: Print BIR details if BIR debugging is enabled
	if DebugBIR || *traceInstructions {
		debug.PrintBIR("BIR generation completed")
		if birPackage != nil {
			debug.PrintBIR("BIR package contains %d functions", len(birPackage.Functions))
			debug.PrintBIR("BIR package contains %d global variables", len(birPackage.GlobalVars))
			debug.PrintBIR("BIR package contains %d import modules", len(birPackage.ImportModules))
		}
	}

	// Run test case detection if enabled
	if *detectTestCases || *showDetection {
		debug.PrintInfo("Running test case detection...")
		// Simple detection based on source content
		if strings.Contains(string(sourceCode), "string") && strings.Contains(string(sourceCode), "name") {
			debug.PrintInfo("Detected string type test case")
			if *showDetection {
				debug.PrintInfo("Detection reasoning: Found 'string' and 'name' keywords in source")
			}
		} else if strings.Contains(string(sourceCode), "Hello, World!") {
			debug.PrintInfo("Detected hello world test case")
			if *showDetection {
				debug.PrintInfo("Detection reasoning: Found 'Hello, World!' string in source")
			}
		} else if strings.Contains(string(sourceCode), "65535") {
			debug.PrintInfo("Detected integer test case")
			if *showDetection {
				debug.PrintInfo("Detection reasoning: Found '65535' integer literal in source")
			}
		} else {
			debug.PrintInfo("No specific test case pattern detected")
			if *showDetection {
				debug.PrintInfo("Detection reasoning: No known patterns found in source")
			}
		}
	}

	// Dump BIR if requested
	if *dumpBir {
		birStr := fmt.Sprintf("%+v", birPackage) // Basic representation, could be improved
		debug.DumpToFile(birStr, "bir_package", "txt")
	}

	debug.PrintCompilerPhase("LLVM CODE GENERATION")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting LLVM code generation", "bir_functions", len(birPackage.Functions))
	}

	// Create a new code generator
	codeGen := backend.NewCodeGenerator(birPackage) // birPackage would be the result of previous steps

	// Generate LLVM IR
	llvmIR, err := codeGen.Generate() // Assumes Generate() (string, error) exists in CodeGenerator
	if err != nil {
		log.Fatalf("Failed to generate LLVM IR: %v", err)
	}

	debug.PrintVerbose("LLVM IR generation completed")

	// Debug: Print LLVM IR generation details if backend debugging is enabled
	if DebugBackend || *traceInstructions {
		debug.PrintBackend("LLVM IR generation completed successfully")
		debug.PrintBackend("Generated IR length: %d characters", len(llvmIR))
		if strings.Contains(llvmIR, "ballerina_main") {
			debug.PrintBackend("Found ballerina_main function in generated IR")
		}
	}

	// Dump LLVM IR if requested
	if *dumpLlvm {
		debug.DumpToFile(llvmIR, "llvm_ir", "ll")
	}

	debug.DumpIR(llvmIR, "final") // Always dump the final LLVM IR if backend debug enabled
	debug.PrintCompilerPhase("COMPILATION TO OBJECT FILE")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting object file compilation", "llvm_ir_size", len(llvmIR))
	}

	// Define output filenames
	baseFilename := strings.TrimSuffix(*inputFile, filepath.Ext(*inputFile))
	objectFile := baseFilename + ".o"
	executableFile := baseFilename
	// Write LLVM IR to a .ll file (optional, for debugging)
	llFile := baseFilename + ".ll"
	if SaveIR || *dumpLlvm {
		if err := os.WriteFile(llFile, []byte(llvmIR), 0644); err != nil {
			log.Printf("Warning: Failed to write LLVM IR file %s: %v", llFile, err)
		} else {
			debug.PrintVerbose("LLVM IR saved to %s", llFile)
		}
	}

	// Dump IR for debugging if requested
	debug.DumpIR(llvmIR, "final")

	// Compile LLVM IR (.ll file) to an object file (.o) using llc
	var llcCmd *exec.Cmd
	if SaveIR || *dumpLlvm {
		llcCmd = exec.Command("llc", "-filetype=obj", "-o", objectFile, llFile)
	} else {
		// Compile directly from stdin
		llcCmd = exec.Command("llc", "-filetype=obj", "-o", objectFile)
		llcCmd.Stdin = strings.NewReader(llvmIR)
	}

	debug.PrintLLVM("Running llc command: %v", llcCmd.Args)

	llcOutput, err := llcCmd.CombinedOutput()
	if err != nil {
		if DebugLLVM || ShowOutput || *traceInstructions {
			debug.PrintLLVM("llc command: %v", llcCmd.Args)
			debug.PrintLLVM("llc output: %s", string(llcOutput))
			debug.PrintLLVM("llc error: %v", err)
		}
		log.Fatalf("llc compilation failed: %v", err)
	}

	if DebugLLVM || ShowOutput || *traceInstructions {
		debug.PrintLLVM("llc output: %s", string(llcOutput))
	}

	fmt.Printf("llc compilation successful. Object file: %s\n", objectFile)

	// Dump object file symbols if requested
	if DumpSymbols || *traceInstructions {
		debug.PrintVerbose("Dumping object file symbols...")
		nmCmd := exec.Command("nm", objectFile)
		nmOutput, nmErr := nmCmd.CombinedOutput()
		if nmErr != nil {
			debug.PrintWarning("Failed to dump symbols: %v", nmErr)
		} else {
			if DumpSymbols {
				fmt.Printf("Object file symbols:\n%s\n", string(nmOutput))
			}
			if *traceInstructions {
				debug.PrintLLVM("Object file symbols:\n%s", string(nmOutput))
			}
		}
	}

	debug.PrintCompilerPhase("LINKING")

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Starting linking process", "object_file", objectFile)
	}

	// Link the object file with the runtime to create the executable
	if ShowOutput || *traceInstructions {
		debug.PrintInfo("Linking object file %s to create executable %s", objectFile, executableFile)
	}

	err = backend.LinkObjectFile(objectFile, executableFile)
	if err != nil {
		log.Fatalf("Linking failed: %v", err)
	}

	fmt.Printf("Compilation successful. Executable: %s\n", executableFile)

	// Trace instructions if enabled
	if *traceInstructions {
		debug.TraceInstruction("MAIN", "Compilation completed successfully", "executable", executableFile)
	}

	// Print detection summary if enabled
	if *detectTestCases || *showDetection {
		debug.PrintInfo("Test case detection completed")
	}

	debug.PrintVerbose("Total compilation completed successfully")

	// Print final statistics if verbose mode is enabled
	if Verbose || DebugAll {
		debug.PrintInfo("=== COMPILATION STATISTICS ===")
		debug.PrintInfo("Input file: %s", *inputFile)
		debug.PrintInfo("Generated %d tokens", len(tokens))
		if birPackage != nil {
			debug.PrintInfo("Generated %d BIR functions", len(birPackage.Functions))
			debug.PrintInfo("Generated %d global variables", len(birPackage.GlobalVars))
		}
		debug.PrintInfo("Generated LLVM IR length: %d characters", len(llvmIR))
		debug.PrintInfo("Output executable: %s", executableFile)
		debug.PrintInfo("===============================")
	}

	// Close debug log file if enabled
	debug.CloseLogFile()
}
