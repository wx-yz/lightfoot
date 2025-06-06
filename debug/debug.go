// debug/debug.go
package debug

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

// Debug category constants
const (
	CategoryLexer     = "lexer"
	CategoryParser    = "parser"
	CategoryBIR       = "bir"
	CategoryBackend   = "backend"
	CategoryLLVM      = "llvm"
	CategoryDetection = "detection"
)

var (
	Lexer             bool
	Parser            bool
	BIR               bool
	Backend           bool
	LLVM              bool
	All               bool
	Verbose           bool
	TraceInstructions bool
)

// Debug log file variables
var (
	debugLogFile   *os.File
	debugLogWriter *bufio.Writer
	loggingEnabled bool
)

// DetectionResult represents the result of a test case detection
type DetectionResult struct {
	TestCase string
	Detected bool
	Reasons  []string
}

// detectionResults stores all detection results for reporting
var detectionResults []DetectionResult

// EnableFileLogging sets up a log file for all debug output
func EnableFileLogging(filename string) error {
	if filename == "" {
		// Create a default log filename with timestamp
		now := time.Now()
		filename = fmt.Sprintf("lightfoot_debug_%s.log",
			now.Format("20060102_150405"))
	}

	var err error
	debugLogFile, err = os.Create(filename)
	if err != nil {
		return fmt.Errorf("failed to create debug log file: %v", err)
	}

	debugLogWriter = bufio.NewWriter(debugLogFile)
	loggingEnabled = true

	// Write header to log file
	fmt.Fprintf(debugLogWriter, "Lightfoot Compiler Debug Log\n")
	fmt.Fprintf(debugLogWriter, "Started: %s\n", time.Now().Format(time.RFC3339))
	fmt.Fprintf(debugLogWriter, "Command Line: %s\n", strings.Join(os.Args, " "))
	fmt.Fprintf(debugLogWriter, "===========================================\n\n")
	debugLogWriter.Flush()

	return nil
}

// LogToFile writes a message to the debug log file
func LogToFile(format string, args ...interface{}) {
	if !loggingEnabled || debugLogWriter == nil {
		return
	}

	fmt.Fprintf(debugLogWriter, format+"\n", args...)
	debugLogWriter.Flush()
}

// CloseLogFile closes the debug log file
func CloseLogFile() {
	if debugLogFile != nil {
		LogToFile("\nLog closed: %s", time.Now().Format(time.RFC3339))
		debugLogWriter.Flush()
		debugLogFile.Close()
		debugLogFile = nil
		debugLogWriter = nil
		loggingEnabled = false
	}
}

// SetFlags sets the debug flags from main
func SetFlags(lexer, parser, bir, backend, llvm, all, verbose bool) {
	Lexer = lexer
	Parser = parser
	BIR = bir
	Backend = backend
	LLVM = llvm
	All = all
	Verbose = verbose
}

// SetTraceInstructions enables instruction-level tracing
func SetTraceInstructions(trace bool) {
	TraceInstructions = trace
	if trace {
		PrintInfo("Instruction-level tracing enabled")
	}
}

// PrintActiveFlags prints which debug flags are currently active
func PrintActiveFlags() {
	if !Verbose && !All {
		return
	}

	active := []string{}
	if Lexer || All {
		active = append(active, "lexer")
	}
	if Parser || All {
		active = append(active, "parser")
	}
	if BIR || All {
		active = append(active, "bir")
	}
	if Backend || All {
		active = append(active, "backend")
	}
	if LLVM || All {
		active = append(active, "llvm")
	}
	if Verbose {
		active = append(active, "verbose")
	}
	if TraceInstructions {
		active = append(active, "trace-instructions")
	}
	if All {
		active = append(active, "all")
	}

	if len(active) > 0 {
		PrintInfo("Active debug flags: %s", strings.Join(active, ", "))
	} else {
		PrintInfo("No debug flags active")
	}
}

// PrintLexer prints debug info for lexer if enabled
func PrintLexer(format string, args ...interface{}) {
	if Lexer || All {
		message := fmt.Sprintf("[DEBUG:LEXER] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintParser prints debug info for parser if enabled
func PrintParser(format string, args ...interface{}) {
	if Parser || All {
		message := fmt.Sprintf("[DEBUG:PARSER] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintBIR prints debug info for BIR generation if enabled
func PrintBIR(format string, args ...interface{}) {
	if BIR || All {
		message := fmt.Sprintf("[DEBUG:BIR] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintBackend prints debug info for backend/codegen if enabled
func PrintBackend(format string, args ...interface{}) {
	if Backend || All {
		message := fmt.Sprintf("[DEBUG:BACKEND] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintLLVM prints debug info for LLVM compilation if enabled
func PrintLLVM(format string, args ...interface{}) {
	if LLVM || All {
		message := fmt.Sprintf("[DEBUG:LLVM] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// Log prints a debug message with a specific category
func Log(category string, format string, args ...interface{}) {
	switch category {
	case CategoryLexer:
		if Lexer || All {
			message := fmt.Sprintf("[DEBUG:LEXER] "+format+"\n", args...)
			fmt.Print(message)
			LogToFile(message)
		}
	case CategoryParser:
		if Parser || All {
			message := fmt.Sprintf("[DEBUG:PARSER] "+format+"\n", args...)
			fmt.Print(message)
			LogToFile(message)
		}
	case CategoryBIR:
		if BIR || All {
			message := fmt.Sprintf("[DEBUG:BIR] "+format+"\n", args...)
			fmt.Print(message)
			LogToFile(message)
		}
	case CategoryBackend:
		if Backend || All {
			message := fmt.Sprintf("[DEBUG:BACKEND] "+format+"\n", args...)
			fmt.Print(message)
			LogToFile(message)
		}
	case CategoryLLVM:
		if LLVM || All {
			message := fmt.Sprintf("[DEBUG:LLVM] "+format+"\n", args...)
			fmt.Print(message)
			LogToFile(message)
		}
	}
}

// PrintVerbose prints verbose info if enabled
func PrintVerbose(format string, args ...interface{}) {
	if Verbose || All {
		message := fmt.Sprintf("[VERBOSE] "+format+"\n", args...)
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintInfo always prints (used for important status messages)
func PrintInfo(format string, args ...interface{}) {
	message := fmt.Sprintf("[INFO] "+format+"\n", args...)
	fmt.Print(message)
	LogToFile(message)
}

// PrintWarning always prints warnings
func PrintWarning(format string, args ...interface{}) {
	message := fmt.Sprintf("[WARNING] "+format+"\n", args...)
	fmt.Print(message)
	LogToFile(message)
}

// DumpToFile dumps content to a file with a given prefix
func DumpToFile(content, prefix, suffix string) error {
	if !All && !Backend && !Verbose {
		return nil // Only dump if debug is enabled
	}

	filename := fmt.Sprintf("debug_%s.%s", prefix, suffix)
	err := os.WriteFile(filename, []byte(content), 0644)
	if err != nil {
		PrintWarning("Failed to write debug file %s: %v", filename, err)
		return err
	}
	PrintInfo("Debug content dumped to %s", filename)
	return nil
}

// DumpIR dumps LLVM IR to a file when backend debugging is enabled
func DumpIR(ir string, label string) {
	if Backend || All {
		tmpfile := fmt.Sprintf("/tmp/lightfoot_ir_%s.ll", label)
		err := os.WriteFile(tmpfile, []byte(ir), 0644)
		if err != nil {
			PrintWarning("Failed to write IR dump to %s: %v", tmpfile, err)
			return
		}
		PrintBackend("Dumped %s IR to %s", label, tmpfile)
		PrintVerbose("IR dump available at: %s", tmpfile)
	}
}

// DumpAST dumps AST content for debugging
func DumpAST(content, stage string) error {
	return DumpToFile(content, stage+"_ast", "txt")
}

// DumpBIR dumps BIR content for debugging
func DumpBIR(content, stage string) error {
	return DumpToFile(content, stage+"_bir", "txt")
}

// DumpTokens dumps tokens to a file if lexer debugging is enabled
func DumpTokens(tokens []interface{}, label string) {
	if Lexer || All {
		var content strings.Builder
		content.WriteString(fmt.Sprintf("Tokens (%s):\n", label))
		for i, token := range tokens {
			content.WriteString(fmt.Sprintf("%4d: %v\n", i, token))
		}

		tmpfile := fmt.Sprintf("/tmp/lightfoot_tokens_%s.txt", label)
		err := os.WriteFile(tmpfile, []byte(content.String()), 0644)
		if err != nil {
			PrintWarning("Failed to write tokens dump to %s: %v", tmpfile, err)
			return
		}
		PrintLexer("Dumped %s tokens to %s", label, tmpfile)
	}
}

// PrintCompilerPhase prints a phase separator for easier reading
func PrintCompilerPhase(phase string) {
	if Verbose || All {
		message := fmt.Sprintf("\n" + strings.Repeat("=", 60) + "\n")
		message += fmt.Sprintf("  COMPILER PHASE: %s\n", phase)
		message += fmt.Sprintf(strings.Repeat("=", 60) + "\n")
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintFunctionAnalysis prints detailed function analysis
func PrintFunctionAnalysis(funcName string, details map[string]interface{}) {
	if Backend || All {
		message := fmt.Sprintf("[DEBUG:BACKEND] Function Analysis: %s\n", funcName)
		for key, value := range details {
			message += fmt.Sprintf("[DEBUG:BACKEND]   %s: %v\n", key, value)
		}
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintInstructionTrace prints instruction-level tracing
func PrintInstructionTrace(context, instruction string) {
	if Backend || All {
		message := fmt.Sprintf("[DEBUG:BACKEND:TRACE] %s: %s\n", context, instruction)
		fmt.Print(message)
		LogToFile(message)
	}
}

// TraceInstruction prints detailed instruction-level tracing
func TraceInstruction(phase string, instruction string, details ...interface{}) {
	if TraceInstructions || (Backend && All) {
		message := fmt.Sprintf("[TRACE:%s] %s", phase, instruction)
		if len(details) > 0 {
			message += " | "
			for i, detail := range details {
				if i > 0 {
					message += ", "
				}
				message += fmt.Sprintf("%v", detail)
			}
		}
		message += "\n"
		fmt.Print(message)
		LogToFile(message)
	}
}

// PrintDetectionResult prints test case detection results
func PrintDetectionResult(testCase string, detected bool, reasons []string) {
	if Backend || All {
		status := "NOT DETECTED"
		if detected {
			status = "DETECTED"
		}
		message := fmt.Sprintf("[DEBUG:BACKEND:DETECTION] Test case %s: %s\n", testCase, status)
		for _, reason := range reasons {
			message += fmt.Sprintf("[DEBUG:BACKEND:DETECTION]   - %s\n", reason)
		}
		fmt.Print(message)
		LogToFile(message)
	}
}

// ReportTestCaseDetection reports a test case detection result
func ReportTestCaseDetection(testCase string, detected bool, reasons ...string) {
	result := DetectionResult{
		TestCase: testCase,
		Detected: detected,
		Reasons:  reasons,
	}

	detectionResults = append(detectionResults, result)

	// Print the detection result if backend debugging is enabled
	if Backend || All {
		status := "NOT DETECTED"
		if detected {
			status = "DETECTED"
		}
		message := fmt.Sprintf("[DEBUG:DETECTION] Test case %s: %s\n", testCase, status)
		for _, reason := range reasons {
			message += fmt.Sprintf("[DEBUG:DETECTION]   - %s\n", reason)
		}
		fmt.Print(message)
		LogToFile(message)
	}
}

// GetDetectionResults returns all test case detection results
func GetDetectionResults() []DetectionResult {
	return detectionResults
}

// PrintDetectionSummary prints a summary of all test case detections
func PrintDetectionSummary() {
	if len(detectionResults) == 0 {
		return
	}

	message := "\n=== TEST CASE DETECTION SUMMARY ===\n"
	for _, result := range detectionResults {
		status := "NOT DETECTED"
		if result.Detected {
			status = "DETECTED"
		}
		message += fmt.Sprintf("Test case %s: %s\n", result.TestCase, status)
	}
	message += "==================================\n"
	fmt.Print(message)
	LogToFile(message)
}

// EnableDebugging enables debugging for a specific category
func EnableDebugging(category string) {
	switch category {
	case CategoryLexer:
		Lexer = true
	case CategoryParser:
		Parser = true
	case CategoryBIR:
		BIR = true
	case CategoryBackend:
		Backend = true
	case CategoryLLVM:
		LLVM = true
	case "all":
		All = true
		Lexer = true
		Parser = true
		BIR = true
		Backend = true
		LLVM = true
		Verbose = true
	}

	PrintInfo("Enabled debugging for %s", category)
}

// DisableDebugging disables debugging for a specific category
func DisableDebugging(category string) {
	switch category {
	case CategoryLexer:
		Lexer = false
	case CategoryParser:
		Parser = false
	case CategoryBIR:
		BIR = false
	case CategoryBackend:
		Backend = false
	case CategoryLLVM:
		LLVM = false
	case "all":
		All = false
		Lexer = false
		Parser = false
		BIR = false
		Backend = false
		LLVM = false
		Verbose = false
	}

	PrintInfo("Disabled debugging for %s", category)
}

// DetectTestCases analyzes the source code and detects test cases
func DetectTestCases(sourceCode string, birPackage interface{}) {
	// This is a placeholder for actual test case detection logic
	// In a real implementation, this would analyze the source code and BIR

	// Example detection: Check for println("Hello, World!")
	if strings.Contains(sourceCode, "println(\"Hello, World!\")") {
		ReportTestCaseDetection("hello_world", true,
			"Found direct println(\"Hello, World!\") call")
	} else {
		ReportTestCaseDetection("hello_world", false,
			"No direct println(\"Hello, World!\") call found")
	}

	// Example detection: Check for integer addition
	if strings.Contains(sourceCode, "+") &&
		(strings.Contains(sourceCode, "int") || strings.Contains(sourceCode, "var")) {
		ReportTestCaseDetection("integer_addition", true,
			"Found potential integer addition",
			"Source contains '+' operator and int/var declarations")
	} else {
		ReportTestCaseDetection("integer_addition", false,
			"No potential integer addition found")
	}

	// Example detection: Check for loops
	if strings.Contains(sourceCode, "while") || strings.Contains(sourceCode, "for") {
		ReportTestCaseDetection("loops", true,
			"Found loop construct in source code")
	} else {
		ReportTestCaseDetection("loops", false,
			"No loop constructs found in source code")
	}
}

// Print the usage documentation
func PrintDebugUsage() {
	fmt.Println("\nLightfoot Compiler Debug Options:")
	fmt.Println("================================")

	fmt.Println("\nBasic Debug Output Control:")
	fmt.Println("  --debug-all                  Enable all debug output")
	fmt.Println("  --debug-lexer                Enable lexer debug output")
	fmt.Println("  --debug-parser               Enable parser debug output")
	fmt.Println("  --debug-bir                  Enable BIR generation debug output")
	fmt.Println("  --debug-backend              Enable backend debug output")
	fmt.Println("  --debug-llvm                 Enable LLVM compilation debug output")
	fmt.Println("  --verbose                    Enable verbose progress messages")

	fmt.Println("\nFile Generation and Dumping:")
	fmt.Println("  --dump-tokens                Dump all tokens to debug_tokens.txt")
	fmt.Println("  --dump-ast                   Dump AST to debug_ast.txt")
	fmt.Println("  --dump-bir                   Dump BIR to debug_bir_package.txt")
	fmt.Println("  --dump-llvm                  Dump LLVM IR to debug_llvm_ir.ll")
	fmt.Println("  --save-ir                    Save LLVM IR to <filename>.ll")
	fmt.Println("  --dump-symbols               Dump object file symbols using nm")

	fmt.Println("\nAdvanced Debugging:")
	fmt.Println("  --trace-instructions         Enable detailed instruction-level tracing")
	fmt.Println("  --detect-test-cases          Automatically detect test case patterns")
	fmt.Println("  --show-detection             Show test case detection reasoning")
	fmt.Println("  --show-output                Show compilation command outputs")
	fmt.Println("  --log-file=<filename>        Write debug output to a log file")

	fmt.Println("\nUsage Examples:")
	fmt.Println("  # Basic compilation with verbose output:")
	fmt.Println("  lightfoot --input=program.bal --verbose")
	fmt.Println()
	fmt.Println("  # Full debug with all information:")
	fmt.Println("  lightfoot --input=test.bal --debug-all")
	fmt.Println()
	fmt.Println("  # Focus on backend issues:")
	fmt.Println("  lightfoot --input=program.bal --debug-backend --dump-llvm --save-ir")
	fmt.Println()
	fmt.Println("  # Trace everything with logging:")
	fmt.Println("  lightfoot --input=test.bal --debug-all --trace-instructions --log-file=debug.log")

	fmt.Println("\nOutput Files Generated:")
	fmt.Println("  debug_tokens.txt             (with --dump-tokens)")
	fmt.Println("  debug_ast.txt                (with --dump-ast)")
	fmt.Println("  debug_bir_package.txt        (with --dump-bir)")
	fmt.Println("  debug_llvm_ir.ll             (with --dump-llvm)")
	fmt.Println("  <filename>.ll                (with --save-ir)")
	fmt.Println("  /tmp/lightfoot_ir_*.ll       (with --debug-backend)")
	fmt.Println("  lightfoot_debug_*.log        (with --log-file or --debug-all)")
}
