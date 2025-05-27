#include "http_server.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --- Stubs for HTTP server ---
// In a real scenario, you'd use a library like libmicrohttpd or similar.

// Placeholder for registered resources
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
        // Potentially allow re-configuration or return an error
        // For now, just note it.
    }
    server_started_port = port;
    printf("HTTP Server: STUB: 'Starting' server on port %d. Call register_service_handlers_now=%d\n", port, register_service_handlers_now);
    // Actual server initialization would go here.
    // If register_service_handlers_now is true, it implies a model where handlers are registered after this call.
    // If false, it might imply handlers were registered before starting.
    return 0; // Success
}

void ballerina_http_register_resource(const char* path, const char* method, ballerina_resource_func_ptr handler) {
    if (resource_count < MAX_RESOURCES) {
        strncpy(resources[resource_count].path, path, sizeof(resources[resource_count].path) - 1);
        strncpy(resources[resource_count].method, method, sizeof(resources[resource_count].method) - 1);
        resources[resource_count].handler = handler;
        resource_count++;
        printf("HTTP Server: STUB: Registered resource: %s %s\n", method, path);

        // Example of how a request might be dispatched (very simplified):
        if (strcmp(path, "/greeting") == 0 && strcmp(method, "GET") == 0) {
            printf("HTTP Server: STUB: Simulating GET /greeting request...\n");
            BallerinaHTTPRequest req_obj;
            req_obj.path = "/greeting";
            req_obj.method = "GET";
            const char* req_body_c_str = "Simulated Request Body";
            req_obj.placeholder_body = ballerina_string_new_with_literal(req_body_c_str, strlen(req_body_c_str));

            BallerinaHTTPResponse resp_obj;
            resp_obj.status_code = 0; // Not set yet
            resp_obj.body = NULL;     // Not set yet

            handler(&req_obj, &resp_obj); // Call the Ballerina resource function

            printf("HTTP Server: STUB: Ballerina resource function executed.\n");
            printf("HTTP Server: STUB: Response status: %d\n", resp_obj.status_code);
            if (resp_obj.body != NULL && resp_obj.body->data != NULL) {
                printf("HTTP Server: STUB: Response body: %s\n", resp_obj.body->data);
            } else {
                printf("HTTP Server: STUB: Response body: (null)\n");
            }
            // In a real server, you'd send this response back to the client.
            // Free BallerinaString objects if they were created by the handler or runtime
            // If placeholder_body was allocated by ballerina_string_new_with_literal, it needs to be freed.
            // Assuming a convention where runtime-created strings are freed by the runtime or caller.
            // For this stub, let's assume it's managed. If ballerina_string_new_with_literal allocates,
            // and this is the end of its life, it should be freed.
            // For now, to match the commented out free:
            if (req_obj.placeholder_body) {
                 // free(req_obj.placeholder_body->data); // ballerina_string_new_with_literal allocates data
                 // free(req_obj.placeholder_body);       // and the struct itself
                 // This should be handled by a proper Ballerina GC or explicit free function for BallerinaString*
            }
            // if (resp_obj.body) free(resp_obj.body); // Responsibility for freeing response body needs to be clear
        }

    } else {
        fprintf(stderr, "HTTP Server: STUB: Max resources reached. Cannot register %s %s.\n", method, path);
    }
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