package main

/*
#cgo CFLAGS: -I.
#include <stdlib.h>
#include "http_server.h"

// Callback for calling Ballerina resource functions
extern BallerinaString* call_ballerina_resource_function(void* handler);
*/
import "C"
import (
	"fmt"
	"net/http"
	"strconv"
	"sync"
	"unsafe"

	"github.com/gin-gonic/gin"
)

// Global variables to store router and registered handlers
var (
	globalRouter *gin.Engine
	globalPort   int
	handlers     = make(map[string]uintptr) // key: "METHOD:PATH", value: function pointer
	mu           sync.RWMutex
)

//export ballerina_http_server_start_go
func ballerina_http_server_start_go(port C.int, register_service_handlers_now C.int) C.int {
	fmt.Printf("Starting Gin HTTP server on port %d\n", int(port))

	// Set Gin to release mode to reduce output
	gin.SetMode(gin.ReleaseMode)

	// Create Gin router
	router := gin.New()
	router.Use(gin.Logger(), gin.Recovery())

	// Store the router globally so we can register routes later
	globalRouter = router
	globalPort = int(port)

	// Start server in a goroutine so it doesn't block
	go func() {
		portStr := ":" + strconv.Itoa(int(port))
		fmt.Printf("HTTP server listening on port %d\n", int(port))
		if err := router.Run(portStr); err != nil {
			fmt.Printf("Failed to start HTTP server: %v\n", err)
		}
	}()

	return 0 // Success
}

//export ballerina_http_register_resource_go
func ballerina_http_register_resource_go(path *C.char, method *C.char, handler unsafe.Pointer) {
	pathStr := C.GoString(path)
	methodStr := C.GoString(method)

	fmt.Printf("Registering HTTP resource: %s %s\n", methodStr, pathStr)

	if globalRouter == nil {
		fmt.Printf("Error: HTTP server not started yet\n")
		return
	}

	// Store the handler function pointer
	key := methodStr + ":" + pathStr
	mu.Lock()
	handlers[key] = uintptr(handler)
	mu.Unlock()

	// Create a Gin handler that calls the Ballerina resource function
	ginHandler := func(c *gin.Context) {
		mu.RLock()
		handlerPtr := handlers[key]
		mu.RUnlock()

		if handlerPtr == 0 {
			c.String(http.StatusInternalServerError, "Handler not found")
			return
		}

		// Call the Ballerina resource function and get result
		result := C.call_ballerina_resource_function(unsafe.Pointer(handlerPtr))
		if result != nil {
			resultStr := C.GoString(result.data)
			c.String(http.StatusOK, resultStr)
		} else {
			c.String(http.StatusOK, "Hello, World!")
		}
	}

	// Register the route with Gin based on HTTP method
	switch methodStr {
	case "GET":
		globalRouter.GET(pathStr, ginHandler)
	case "POST":
		globalRouter.POST(pathStr, ginHandler)
	case "PUT":
		globalRouter.PUT(pathStr, ginHandler)
	case "DELETE":
		globalRouter.DELETE(pathStr, ginHandler)
	case "PATCH":
		globalRouter.PATCH(pathStr, ginHandler)
	default:
		fmt.Printf("Unsupported HTTP method: %s\n", methodStr)
	}
}

//export ballerina_http_server_wait_go
func ballerina_http_server_wait_go() {
	// Keep the main thread alive
	select {}
}

func main() {
	// This is required for CGO
}
