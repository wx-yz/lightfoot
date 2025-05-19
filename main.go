// main.go
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"wx-yz/lightfoot/backend"
	"wx-yz/lightfoot/bir"
	"wx-yz/lightfoot/compiler"
)

func printFunctionBIR(fn *bir.Function) {
	// For .<init> function, use special return type signature
	returnType := fn.ReturnVariable.Type
	if fn.Name == ".<init>" {
		returnType = "error{map<ballerina/lang.value:0.0.0:Cloneable>}|()"
	}

	fmt.Printf("\n%s %s function(", strings.ToLower(fn.Visibility), fn.Name)
	paramStrings := []string{}
	for _, p := range fn.Parameters {
		paramStrings = append(paramStrings, p.Type)
	}
	fmt.Printf("%s) -> %s {\n", strings.Join(paramStrings, ", "), returnType)

	printedVars := make(map[string]bool)

	// Print Return Var
	if fn.ReturnVariable != nil {
		fmt.Printf("    %s(%s) %s;\n", fn.ReturnVariable.BIRName, fn.ReturnVariable.Kind, fn.ReturnVariable.Type)
		printedVars[fn.ReturnVariable.BIRName] = true
	}
	// Print Arg Vars
	for _, v := range fn.ArgumentVars {
		if !printedVars[v.BIRName] {
			fmt.Printf("    %s(%s) %s; // original: %s\n", v.BIRName, v.Kind, v.Type, v.OriginalName)
			printedVars[v.BIRName] = true
		}
	}
	// Print other Local/Temp vars
	for birName, v := range fn.LocalVars {
		if !printedVars[birName] {
			if v.Kind != bir.VarKindArg && v.Kind != bir.VarKindReturn {
				fmt.Printf("    %s(%s) %s; // original: %s\n", v.BIRName, v.Kind, v.Type, v.OriginalName)
				printedVars[v.BIRName] = true
			}
		}
	}

	fmt.Println() // Blank line before basic blocks

	for _, bb := range fn.BasicBlocks {
		fmt.Printf("    %s {\n", bb.ID)
		for _, instr := range bb.Instructions {
			fmt.Printf("        %s;\n", instr.String())
		}
		if bb.Terminator != nil {
			fmt.Printf("        %s;\n", bb.Terminator.String())
		} else {
			fmt.Printf("        // Error: BB %s has no terminator\n", bb.ID)
		}
		fmt.Printf("    }\n")
	}
	fmt.Printf("}\n")
}

func main() {
	// Parse command line arguments
	inputFile := flag.String("input", "", "Input Ballerina source file")
	outputFile := flag.String("output", "", "Output object file")
	execFile := flag.String("exec", "", "Output executable file")
	showBIR := flag.Bool("show-bir", false, "Show BIR output")
	noLink := flag.Bool("no-link", false, "Don't link into executable")
	flag.Parse()

	if *inputFile == "" {
		fmt.Println("Error: Input file is required")
		flag.Usage()
		os.Exit(1)
	}

	// Set default output file name if not specified
	if *outputFile == "" {
		*outputFile = strings.TrimSuffix(*inputFile, ".bal") + ".o"
	}

	// Set default executable name if not specified
	if *execFile == "" {
		// Remove extension and add platform-specific executable extension
		base := strings.TrimSuffix(*inputFile, filepath.Ext(*inputFile))
		if runtime.GOOS == "windows" {
			*execFile = base + ".exe"
		} else {
			*execFile = base
		}
	}

	// Read input file
	input, err := os.ReadFile(*inputFile)
	if err != nil {
		fmt.Printf("Error reading input file: %v\n", err)
		os.Exit(1)
	}

	// Compile to BIR
	birPackage, err := compiler.Compile(string(input))
	if err != nil {
		fmt.Printf("Error compiling to BIR: %v\n", err)
		os.Exit(1)
	}

	// Show BIR if requested
	if *showBIR {
		fmt.Println("BIR Output:")
		for _, fn := range birPackage.Functions {
			printFunctionBIR(fn)
		}
		if birPackage.ModuleInitFunc != nil {
			printFunctionBIR(birPackage.ModuleInitFunc)
		}
		if birPackage.ModuleStartFunc != nil {
			printFunctionBIR(birPackage.ModuleStartFunc)
		}
		if birPackage.ModuleStopFunc != nil {
			printFunctionBIR(birPackage.ModuleStopFunc)
		}
	}

	// Generate native code
	codeGen := backend.NewCodeGenerator(birPackage)
	if err := codeGen.GenerateCode(*outputFile); err != nil {
		fmt.Printf("Error generating code: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Successfully generated object file: %s\n", *outputFile)

	// Link object file into executable if requested
	if !*noLink {
		if err := backend.LinkObjectFile(*outputFile, *execFile); err != nil {
			fmt.Printf("Error linking executable: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Successfully generated executable: %s\n", *execFile)
	}
}
