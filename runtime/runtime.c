#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

// String functions

// Create a string with a literal (used for constant strings)
BallerinaString* ballerina_string_new_with_literal(const char* data, int64_t length) {
    // Allocate memory for the string structure
    BallerinaString* str = (BallerinaString*)malloc(sizeof(BallerinaString));
    if (!str) {
        fprintf(stderr, "Error: Failed to allocate memory for string\n");
        exit(1);
    }
    
    // Allocate memory for the string data and copy it
    char* dataPtr = (char*)malloc(length + 1);
    if (!dataPtr) {
        fprintf(stderr, "Error: Failed to allocate memory for string data\n");
        free(str);
        exit(1);
    }
    
    // Copy the string data and ensure null termination
    memcpy(dataPtr, data, length);
    dataPtr[length] = '\0';  // Ensure null termination
    
    // Set the string fields
    str->length = length;
    str->data = dataPtr;
    
    fprintf(stderr, "[DEBUG] ballerina_string_new_with_literal: created string with length=%lld, data='%s'\n", 
            length, dataPtr);
            
    // Return the string pointer
    return str;
}

// Print a string (used by println)
void ballerina_io_println(void* value) {
    fprintf(stderr, "[DEBUG] ballerina_io_println: value=%p\n", value);
    
    if (!value) {
        printf("(null)\n");
        fflush(stdout);
        return;
    }
    
    // The value is a BallerinaString pointer
    BallerinaString* str = (BallerinaString*)value;
    fprintf(stderr, "[DEBUG] ballerina_io_println: str=%p, length=%lld, data=%p\n", 
            str, str->length, str->data);
    
    if (!str->data) {
        printf("(empty)\n");
        fflush(stdout);
        return;
    }
    
    // Add memory verification
    if (str->length < 0 || str->length > 10000) {
        fprintf(stderr, "[WARNING] ballerina_io_println: Suspicious string length: %lld\n", str->length);
        str->length = strlen(str->data); // Try to recover
        fprintf(stderr, "[WARNING] ballerina_io_println: Corrected length to: %lld\n", str->length);
    }
    
    // Print the string with proper null termination and honor the length field
    fprintf(stderr, "[DEBUG] ballerina_io_println: printing string: '%.*s'\n", 
            (int)str->length, str->data);
    printf("%.*s\n", (int)str->length, str->data);
    fflush(stdout);
}

// Create a new map
BallerinaMap* ballerina_lang_map_new() {
    BallerinaMap* map = (BallerinaMap*)malloc(sizeof(BallerinaMap));
    if (!map) {
        fprintf(stderr, "Failed to allocate memory for map\n");
        exit(1);
    }
    
    map->size = 0;
    map->capacity = 16; // Initial capacity
    map->data = NULL;   // No actual storage implemented yet
    
    return map;
}

// Create a new array with the given size and element size
BallerinaArray* ballerina_lang_array_new(int64_t size, int64_t elementSize) {
    BallerinaArray* arr = (BallerinaArray*)malloc(sizeof(BallerinaArray));
    if (!arr) {
        fprintf(stderr, "Failed to allocate memory for array\n");
        exit(1);
    }
    
    // Allocate memory for array elements
    void* data = NULL;
    if (size > 0) {
        data = calloc(size, elementSize);
        if (!data) {
            fprintf(stderr, "Failed to allocate memory for array data\n");
            free(arr);
            exit(1);
        }
    }
    
    arr->length = size;
    arr->data = data;
    
    return arr;
}

// Concatenate two strings
BallerinaString* ballerina_lang_string_concat(BallerinaString* str1, BallerinaString* str2) {
    if (!str1 || !str2) {
        return ballerina_string_new_with_literal("", 0);
    }
    
    int64_t newLen = str1->length + str2->length;
    BallerinaString* result = (BallerinaString*)malloc(sizeof(BallerinaString));
    if (!result) {
        fprintf(stderr, "Failed to allocate memory for concatenated string\n");
        exit(1);
    }
    
    char* newData = (char*)malloc(newLen + 1);
    if (!newData) {
        fprintf(stderr, "Failed to allocate memory for concatenated string data\n");
        free(result);
        exit(1);
    }
    
    memcpy(newData, str1->data, str1->length);
    memcpy(newData + str1->length, str2->data, str2->length);
    newData[newLen] = '\0';
    
    result->length = newLen;
    result->data = newData;
    
    return result;
}