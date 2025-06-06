// filepath: /Users/rch/src/lightfoot/backend/debug.go
package backend

import (
	"wx-yz/lightfoot/bir"
	"wx-yz/lightfoot/debug"
)

// DebugPrintln prints debug information about the BIR package
func DebugPrintln(pkg *bir.Package) {
	debug.PrintBackend("Debug information for BIR package:")
	debug.PrintBackend("  Functions: %d", len(pkg.Functions))
	debug.PrintBackend("  Global Variables: %d", len(pkg.GlobalVars))
	if pkg.ActualInitFunc != nil {
		debug.PrintBackend("  Init Function: %s (Return Type: %s)",
			pkg.ActualInitFunc.Name,
			pkg.ActualInitFunc.ReturnVariable.Type)
	} else {
		debug.PrintBackend("  Init Function: None")
	}
}

// PrintlnHandler handles println calls in the BIR package
func PrintlnHandler(pkg *bir.Package) {
	// This is a stub function for handling println calls
	// In a full implementation, this would analyze the BIR for println calls
	// and ensure they're properly executed
	debug.PrintBackend("Processing println calls in BIR")
}

// LinkExecutable is a fallback method for executable creation
func LinkExecutable(execPath string) error {
	// In the actual implementation, this would use the platform-specific
	// linker to create an executable.
	// Since we're using MockLinkObjectFile, this is just a stub
	debug.PrintBackend("LinkExecutable: Fallback executable linking called")
	return nil
}
