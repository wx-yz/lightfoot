// main.go
package main

import (
	"fmt"
	"os"
	"strings"             // For printing
	"wx-yz/lightfoot/bir" // For type access if needed
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
	for _, p := range fn.Parameters { // Use fn.Parameters for signature
		paramStrings = append(paramStrings, p.Type) // Simplified: just type
	}
	fmt.Printf("%s) -> %s {\n", strings.Join(paramStrings, ", "), returnType)

	// Print variable declarations (%N(KIND) type;)
	// Need to iterate LocalVars and group by kind or print as they appear in target.
	// The target format lists them at the top. Let's try to replicate.

	// Sort vars by BIRName for consistent printing (optional)
	// varNames := make([]string, 0, len(fn.LocalVars))
	// for name := range fn.LocalVars {
	// 	varNames = append(varNames, name)
	// }
	// sort.Strings(varNames)

	// For accurate printing like target, we'd need to collect vars in a specific order (Return, Args, then Locals/Temps by number)
	// This is simplified for now.
	printedVars := make(map[string]bool)

	// Print Return Var
	if fn.ReturnVariable != nil {
		fmt.Printf("    %s(%s) %s;\n", fn.ReturnVariable.BIRName, fn.ReturnVariable.Kind, fn.ReturnVariable.Type)
		printedVars[fn.ReturnVariable.BIRName] = true
	}
	// Print Arg Vars (as they appear in function.ArgumentVars which should be in order)
	for _, v := range fn.ArgumentVars {
		if !printedVars[v.BIRName] {
			fmt.Printf("    %s(%s) %s; // original: %s\n", v.BIRName, v.Kind, v.Type, v.OriginalName)
			printedVars[v.BIRName] = true
		}
	}
	// Print other Local/Temp vars, ensuring %0, %1, etc. are not reprinted if they were args/return
	for birName, v := range fn.LocalVars { // Iterate all known vars
		if !printedVars[birName] {
			// Only print if it's not an ARG or RETURN var already printed, or if it's a true LOCAL/TEMP
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
	if len(os.Args) < 2 {
		fmt.Println("Usage: ballerina_compiler <file.bal>")
		os.Exit(1)
	}

	filePath := os.Args[1]
	sourceCode, err := os.ReadFile(filePath)
	if err != nil {
		fmt.Printf("Error reading file %s: %v\n", filePath, err)
		os.Exit(1)
	}

	comp := compiler.NewCompiler()
	birPackage, errP := comp.Compile(string(sourceCode)) // Changed var name to avoid conflict
	if errP != nil {
		fmt.Printf("Compilation failed:\n%v", errP)
		os.Exit(1)
	}

	if birPackage == nil {
		fmt.Println("No BIR package generated.")
		return
	}

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
