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
void ballerina_io_println(BallerinaString* value);
void ballerina_io_println_array(BallerinaArray* array); // New function for array println
BallerinaMap* ballerina_lang_map_new();
BallerinaArray* ballerina_lang_array_new(int64_t size, int64_t elementSize);
void ballerina_lang_array_set(BallerinaArray* arr, int64_t index, void* value);
BallerinaString* ballerina_lang_string_concat(BallerinaString* str1, BallerinaString* str2);
BallerinaString* ballerina_runtime_int_to_string(int64_t val);
BallerinaString* ballerina_runtime_bool_to_string(int8_t val);

#endif // BALLERINA_RUNTIME_H