// main.go
package main

import (
	"fmt"
	"os"
	"wx-yz/lightfoot/compiler"
)

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
	birPackage, err := comp.Compile(string(sourceCode))
	if err != nil {
		fmt.Printf("Compilation failed: %v\n", err)
		os.Exit(1)
	}

	// For demonstration, print the generated BIR (simplified representation)
	fmt.Println("Compilation Successful. Simplified BIR Output:")
	fmt.Println("-------------------------------------------")
	if birPackage == nil {
		fmt.Println("No BIR package generated (source might be empty or only comments).")
		return
	}

	fmt.Printf("Package: %s.%s version %s\n", birPackage.OrgName, birPackage.Name, birPackage.Version)
	fmt.Println("Imports:")
	for _, imp := range birPackage.ImportModules {
		fmt.Printf("  %s/%s as %s\n", imp.OrgName, imp.PackageName, imp.Alias)
	}

	fmt.Println("\nGlobal Variables:")
	for _, globalVar := range birPackage.GlobalVars {
		fmt.Printf("  %s %s\n", globalVar.Type, globalVar.Name) // Simplified: No initial value shown here
	}

	fmt.Println("\nFunctions:")
	for _, fn := range birPackage.Functions {
		fmt.Printf("  Function: %s\n", fn.Name)
		fmt.Printf("    Visibility: %s\n", fn.Visibility)
		fmt.Printf("    Return Type: %s\n", fn.ReturnType) // Assuming single return for simplicity
		fmt.Println("    Parameters:")
		for _, param := range fn.Parameters {
			fmt.Printf("      %s %s\n", param.Type, param.Name)
		}
		fmt.Println("    Local Variables:")
		for _, localVar := range fn.LocalVars {
			fmt.Printf("      %s %s (Kind: %s, Scope: %d)\n", localVar.Type, localVar.Name, localVar.Kind, localVar.ScopeID)
		}
		fmt.Println("    Body (Simplified Instructions):")
		for i, instruction := range fn.Instructions {
			fmt.Printf("      %d: %s\n", i, instruction.String())
		}
		fmt.Println("    ---")
	}
	fmt.Println("-------------------------------------------")

}
