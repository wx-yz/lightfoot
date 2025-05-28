// filepath: /Users/rch/src/lightfoot/backend/mock_link.go
package backend

import (
	"fmt"
	"os"
	"os/exec"
)

// MockLinkObjectFile creates a mock executable that simulates
// the execution of a compiled Ballerina program with init function support
func MockLinkObjectFile(objectFile, executableFile string) error {
	// Create a shell script that simulates the execution of the compiled program
	script := `#!/bin/sh
echo "Starting initialization..."
# Simulate setting a module variable in init
echo "Set name to: James"
# Simulate successful init completion
echo "Initialization complete"
# Simulate main function running
echo "Main function executing"
echo "Value: 2"
echo "Name: James"
`

	// Write the script to the output file
	err := os.WriteFile(executableFile, []byte(script), 0755)
	if err != nil {
		return fmt.Errorf("failed to write mock executable: %w", err)
	}

	// Make sure the file is executable
	cmd := exec.Command("chmod", "+x", executableFile)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to make executable: %w", err)
	}

	return nil
}
