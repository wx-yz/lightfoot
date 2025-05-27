#include "http_server.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Declare the Go functions
extern int ballerina_http_server_start_go(int port, int register_service_handlers_now);
extern void ballerina_http_register_resource_go(const char* path, const char* method, void* handler);
extern void ballerina_http_server_wait_go();

// Placeholder for registered resources (for debugging)
#define MAX_RESOURCES 10
typedef struct {
    char path[256];
    char method[16];
    ballerina_resource_func_ptr handler;
} RegisteredResource;

RegisteredResource resources[MAX_RESOURCES];
int resource_count = 0;
int server_started_port = -1;

int ballerina_http_server_start(int port, int register_service_handlers_now) {
    if (server_started_port != -1) {
        printf("HTTP Server: Already started on port %d\n", server_started_port);
        return 0;
    }
    server_started_port = port;
    printf("HTTP Server: Starting real Gin server on port %d\n", port);
    
    // Call the Go function to start the server
    return ballerina_http_server_start_go(port, register_service_handlers_now);
}

void ballerina_http_register_resource(const char* path, const char* method, ballerina_resource_func_ptr handler) {
    if (resource_count < MAX_RESOURCES) {
        strncpy(resources[resource_count].path, path, sizeof(resources[resource_count].path) - 1);
        strncpy(resources[resource_count].method, method, sizeof(resources[resource_count].method) - 1);
        resources[resource_count].handler = handler;
        resource_count++;
        printf("HTTP Server: Registering resource: %s %s\n", method, path);
        
        // Call the Go function to register the resource
        ballerina_http_register_resource_go(path, method, (void*)handler);
    } else {
        fprintf(stderr, "HTTP Server: Max resources reached. Cannot register %s %s.\n", method, path);
    }
}

// Function to keep the HTTP server running
void ballerina_http_server_wait() {
    printf("HTTP Server: Waiting for server to handle requests...\n");
    ballerina_http_server_wait_go();
}

// C function to call Ballerina resource function (called from Go)
BallerinaString* call_ballerina_resource_function(void* handler) {
    if (!handler) return NULL;
    
    ballerina_resource_func_ptr func = (ballerina_resource_func_ptr)handler;
    
    // Create a dummy request and response
    BallerinaHTTPRequest req_obj;
    req_obj.path = "/greeting";
    req_obj.method = "GET";
    req_obj.placeholder_body = NULL;

    BallerinaHTTPResponse resp_obj;
    resp_obj.status_code = 200;
    resp_obj.body = NULL;

    // Call the Ballerina resource function
    func(&req_obj, &resp_obj);

    // Return the response body
    return resp_obj.body;
}

// --- Implementation for Ballerina-facing HTTP utility functions ---

void ballerina_http_response_set_string_body(BallerinaHTTPResponse* resp, BallerinaString* body_str) {
    if (!resp) return;
    // In a real system, you might need to manage memory (e.g., copy the string)
    resp->body = body_str;
    printf("Runtime: ballerina_http_response_set_string_body: Set body to (addr: %p, len: %lld, data: %s)\n",
           (void*)body_str, body_str ? body_str->length : -1, body_str && body_str->data ? body_str->data : "NULL");
}

void ballerina_http_response_set_status_code(BallerinaHTTPResponse* resp, int status_code) {
    if (!resp) return;
    resp->status_code = status_code;
    printf("Runtime: ballerina_http_response_set_status_code: Set status to %d\n", status_code);
}

BallerinaString* ballerina_http_request_get_placeholder_body(BallerinaHTTPRequest* req) {
    if (!req) return NULL;
    printf("Runtime: ballerina_http_request_get_placeholder_body: Returning placeholder body (addr: %p)\n", (void*)req->placeholder_body);
    // This should return a new BallerinaString or a reference that's managed.
    // For now, just returning the direct pointer.
    return req->placeholder_body;
}

// You would need implementations for create_ballerina_string_from_c_str if not already in runtime.c
// For example:
/*
BallerinaString* create_ballerina_string_from_c_str(const char* c_str) {
    if (!c_str) return NULL;
    size_t len = strlen(c_str);
    BallerinaString* b_str = (BallerinaString*)malloc(sizeof(BallerinaString) + len + 1);
    if (!b_str) return NULL;
    b_str->length = len;
    strcpy((char*)(b_str->data), c_str);
    return b_str;
}
*/