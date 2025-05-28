// filepath: /Users/rch/src/lightfoot/backend/debug.go
package backend

import (
	"fmt"
	"wx-yz/lightfoot/bir"
)

// DebugPrintln prints debug information about the BIR package
func DebugPrintln(pkg *bir.Package) {
	fmt.Println("[DEBUG] Debug information for BIR package:")
	fmt.Printf("  Functions: %d\n", len(pkg.Functions))
	fmt.Printf("  Global Variables: %d\n", len(pkg.GlobalVars))
	if pkg.ActualInitFunc != nil {
		fmt.Printf("  Init Function: %s (Return Type: %s)\n",
			pkg.ActualInitFunc.Name,
			pkg.ActualInitFunc.ReturnVariable.Type)
	} else {
		fmt.Println("  Init Function: None")
	}
}

// PrintlnHandler handles println calls in the BIR package
func PrintlnHandler(pkg *bir.Package) {
	// This is a stub function for handling println calls
	// In a full implementation, this would analyze the BIR for println calls
	// and ensure they're properly executed
	fmt.Println("[DEBUG] Processing println calls in BIR")
}

// LinkExecutable is a fallback method for executable creation
func LinkExecutable(execPath string) error {
	// In the actual implementation, this would use the platform-specific
	// linker to create an executable.
	// Since we're using MockLinkObjectFile, this is just a stub
	fmt.Println("[DEBUG] LinkExecutable: Fallback executable linking called")
	return nil
}
