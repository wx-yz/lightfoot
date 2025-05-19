#ifndef BALLERINA_RUNTIME_H
#define BALLERINA_RUNTIME_H

#include <stdint.h>

// Type definitions matching the LLVM structs in runtime.go

typedef struct {
    int64_t length;
    char*   data;
} BallerinaString;

typedef struct {
    int64_t length;
    void*   data;
} BallerinaArray;

typedef struct {
    int64_t size;
    int64_t capacity;
    void*   data;
} BallerinaMap;

// Function declarations
BallerinaString* ballerina_string_new_with_literal(const char* data, int64_t len);
void ballerina_io_println(void* value);
BallerinaMap* ballerina_lang_map_new();
BallerinaArray* ballerina_lang_array_new(int64_t size, int64_t elementSize);
BallerinaString* ballerina_lang_string_concat(BallerinaString* str1, BallerinaString* str2);

#endif // BALLERINA_RUNTIME_H