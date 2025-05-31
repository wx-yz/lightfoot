package backend

/*
#include <stdio.h>
*/
import "C"

import (
	"fmt"
	"log"
	"net/http"

	"github.com/gin-gonic/gin"
)

// startHTTPServer starts a Gin HTTP server on port 9090
//
//export start_http_server
func start_http_server() {
	// Set Gin to release mode to reduce verbose output
	gin.SetMode(gin.ReleaseMode)

	// Create a new Gin router
	r := gin.Default()

	// Add middleware for CORS if needed
	r.Use(func(c *gin.Context) {
		c.Header("Access-Control-Allow-Origin", "*")
		c.Header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		c.Header("Access-Control-Allow-Headers", "Content-Type, Authorization")

		if c.Request.Method == "OPTIONS" {
			c.AbortWithStatus(http.StatusNoContent)
			return
		}

		c.Next()
	})

	// Define the greeting endpoint
	r.GET("/greeting", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{
			"message": "Hello, World!",
		})
	})

	// Add a health check endpoint
	r.GET("/health", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{
			"status": "healthy",
		})
	})

	// Add a root endpoint
	r.GET("/", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{
			"message": "Ballerina HTTP Service",
			"endpoints": []string{
				"/greeting - Returns Hello, World!",
				"/health - Health check",
			},
		})
	})

	// Start the server
	fmt.Println("Starting Gin HTTP server...")
	log.Fatal(r.Run(":9090"))
}

// For CGO export
func main() {} // Required for CGO but won't be called
