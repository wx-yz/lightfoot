// backend/llvm_debug.go
package backend

import (
	"fmt"
	"path/filepath"
	"strings"
)

// DebugInfo holds debug information for LLVM IR generation (stub implementation)
type DebugInfo struct {
	filename string
}

// NewDebugInfo creates a new debug info context (stub implementation)
func NewDebugInfo(filename string) *DebugInfo {
	return &DebugInfo{
		filename: filename,
	}
}

// CreateFunction creates debug info for a function (stub implementation)
func (di *DebugInfo) CreateFunction(name string, linkageName string, line int, isLocalToUnit bool, isDefinition bool, scopeLine int) interface{} {
	// Stub implementation - returns nil for now
	return nil
}

// CreateVariable creates debug info for a variable (stub implementation)
func (di *DebugInfo) CreateVariable(name string, varType string, line int, isLocal bool) interface{} {
	// Stub implementation - returns nil for now
	return nil
}

// PopFunction pops the current function from the scope stack (stub implementation)
func (di *DebugInfo) PopFunction() {
	// Stub implementation - does nothing for now
}

// CreateLocation creates a debug location (stub implementation)
func (di *DebugInfo) CreateLocation(line int, column int) interface{} {
	// Stub implementation - returns nil for now
	return nil
}

// Finalize finalizes the debug info (stub implementation)
func (di *DebugInfo) Finalize() {
	// Stub implementation - does nothing for now
}

// GetTypeName returns a normalized type name for debug info
func GetTypeName(birType string) string {
	// Normalize BIR types to debug-friendly names
	switch {
	case birType == "int":
		return "int32"
	case birType == "float":
		return "float32"
	case birType == "string":
		return "string"
	case birType == "boolean":
		return "bool"
	case strings.HasSuffix(birType, "[]"):
		elementType := GetTypeName(birType[:len(birType)-2])
		return fmt.Sprintf("[]%s", elementType)
	case strings.HasPrefix(birType, "map<"):
		valueType := GetTypeName(birType[4 : len(birType)-1])
		return fmt.Sprintf("map[string]%s", valueType)
	default:
		return birType
	}
}

// Helper function to get directory from filename
func getDir(filename string) string {
	return filepath.Dir(filename)
}

// Helper function to get base filename
func getBase(filename string) string {
	return filepath.Base(filename)
}
