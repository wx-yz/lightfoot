// main.go
package main

import (
	"flag"
	"fmt"
	"os"
	"strings"
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

func printBIR(birPackage *bir.Package) {
	fmt.Println("================ Emitting Module ================")
	fmt.Printf("module %s/%s v %s;\n", birPackage.OrgName, birPackage.Name, birPackage.Version)
	fmt.Println() // Blank line

	for _, imp := range birPackage.ImportModules {
		fmt.Printf("import %s/%s v %s;\n", imp.OrgName, imp.PackageName, imp.Version)
	}
	// Target BIR has one blank line after imports, then $annotation_data, then one blank line.
	fmt.Println() // One blank line after all imports

	if birPackage.AnnotationData != nil {
		fmt.Printf("%s %s;\n", birPackage.AnnotationData.Name, birPackage.AnnotationData.Type)
		fmt.Println() // One blank line after AnnotationData
	} else {
		// If no annotation data, maybe still print a blank line if imports were present
		if len(birPackage.ImportModules) > 0 {
			fmt.Println() // Ensure a blank line before functions if only imports were present
		}
	}

	// Print lifecycle functions first
	if birPackage.ModuleInitFunc != nil {
		printFunctionBIR(birPackage.ModuleInitFunc)
	}
	if birPackage.ModuleStartFunc != nil {
		printFunctionBIR(birPackage.ModuleStartFunc)
	}
	if birPackage.ModuleStopFunc != nil {
		printFunctionBIR(birPackage.ModuleStopFunc)
	}

	// Print other functions
	for _, fn := range birPackage.Functions {
		printFunctionBIR(fn)
	}
	fmt.Println("\n================ Emitting Module ================")
}

func main() {
	// Define command line flags
	dumpBIR := flag.Bool("dump-bir", false, "Output the Ballerina Intermediate Representation")

	// Parse flags
	flag.Parse()

	// Check if there's a Ballerina file specified after the flags
	args := flag.Args()
	if len(args) < 1 {
		fmt.Println("Usage: lightfoot [--dump-bir] <file.bal>")
		os.Exit(1)
	}

	filePath := args[0]
	sourceCode, err := os.ReadFile(filePath)
	if err != nil {
		fmt.Printf("Error reading file %s: %v\n", filePath, err)
		os.Exit(1)
	}

	comp := compiler.NewCompiler()
	birPackage, errP := comp.Compile(string(sourceCode))
	if errP != nil {
		fmt.Printf("Compilation failed:\n%v", errP)
		os.Exit(1)
	}

	if birPackage == nil {
		fmt.Println("No BIR package generated.")
		return
	}

	// Only output BIR if the flag is set
	if *dumpBIR {
		printBIR(birPackage)
	} else {
		fmt.Printf("Successfully compiled %s\n", filePath)
		// Here you would normally perform other actions with the compiled code
		// Such as further compilation stages, execution, etc.
	}
}
