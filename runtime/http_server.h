#ifndef HTTP_SERVER_H
#define HTTP_SERVER_H

#include "runtime.h" // For BallerinaString, etc.

// Forward declaration
typedef struct BallerinaHTTPRequest BallerinaHTTPRequest;
typedef struct BallerinaHTTPResponse BallerinaHTTPResponse;

// Define simplified request/response structs
// In a real implementation, these would be more complex
struct BallerinaHTTPRequest {
    const char* path;
    const char* method;
    // Add fields for headers, body, query params etc.
    BallerinaString* placeholder_body; // Example
};

struct BallerinaHTTPResponse {
    int status_code;
    BallerinaString* body;
    // Add fields for headers, etc.
};

// Type for the Ballerina resource function handler (after LLVM compilation)
// It will be called by the C HTTP server with request/response objects.
typedef void (*ballerina_resource_func_ptr)(BallerinaHTTPRequest* req, BallerinaHTTPResponse* resp);

// Starts the HTTP server on the given port.
// Returns 0 on success, -1 on error.
int ballerina_http_server_start(int port, int register_service_handlers_now);

// Registers a resource function handler.
// The C runtime will call this function when a matching request arrives.
void ballerina_http_register_resource(const char* path, const char* method, ballerina_resource_func_ptr handler);

// Functions to interact with request/response from Ballerina (called from LLVM IR)
void ballerina_http_response_set_string_body(BallerinaHTTPResponse* resp, BallerinaString* body_str);
void ballerina_http_response_set_status_code(BallerinaHTTPResponse* resp, int status_code);
BallerinaString* ballerina_http_request_get_placeholder_body(BallerinaHTTPRequest* req); // Example accessor

#endif // HTTP_SERVER_H