// backend/debug_helpers.go
package backend

import (
	"fmt"
	"strings"
	"wx-yz/lightfoot/bir"
)

// detectSourcePattern analyzes source code to detect known test patterns
func detectSourcePattern(sourceCode string) {
	if sourceCode != "" {
		// Simple test case detection based on source content
		if strings.Contains(sourceCode, "65535") {
			fmt.Printf("[DEBUG] Test case detected: integer test case with value 65535\n")
		} else if strings.Contains(sourceCode, "Hello, World!") {
			fmt.Printf("[DEBUG] Test case detected: hello world test case\n")
		} else if strings.Contains(sourceCode, "string") && strings.Contains(sourceCode, "name") {
			fmt.Printf("[DEBUG] Test case detected: string type test case\n")
		}
	}
}

// detectBIRPattern analyzes BIR package structure to detect known test patterns
func detectBIRPattern(pkg *bir.Package) {
	if pkg == nil {
		return
	}

	// Simple BIR-based detection logic
	for _, fn := range pkg.Functions {
		if fn.Name == "main" {
			fmt.Printf("[DEBUG] Found main function with %d basic blocks\n", len(fn.BasicBlocks))

			// Check for specific patterns in the function
			for _, bb := range fn.BasicBlocks {
				for _, inst := range bb.Instructions {
					if constInst, ok := inst.(*bir.ConstantLoadInst); ok {
						if val, ok := constInst.Value.(int64); ok && val == 65535 {
							fmt.Printf("[DEBUG] BIR pattern detected: integer test case\n")
						} else if strVal, ok := constInst.Value.(string); ok && strVal == "Hello, World!" {
							fmt.Printf("[DEBUG] BIR pattern detected: hello world test case\n")
						}
					}
				}
			}
		}
	}
}

// Helper function to analyze function complexity
func analyzeFunctionComplexity(fn *bir.Function) {
	if fn == nil {
		return
	}

	fmt.Printf("[DEBUG] Function %s: %d basic blocks, %d parameters\n",
		fn.Name, len(fn.BasicBlocks), len(fn.Parameters))

	totalInstructions := 0
	for _, bb := range fn.BasicBlocks {
		totalInstructions += len(bb.Instructions)
	}
	fmt.Printf("[DEBUG] Function %s: %d total instructions\n", fn.Name, totalInstructions)
}
