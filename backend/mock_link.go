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
	// Create a shell script that prints an error - we shouldn't be using this anymore
	script := `#!/bin/sh
echo "ERROR: Mock linker used instead of real backend!"
echo "The real backend should generate proper initialization and main execution."
exit 1
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
