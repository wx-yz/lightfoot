#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Ballerina string structure
typedef struct {
    long length;
    char* data;
} BallerinaString;

// Ballerina array structure
typedef struct {
    long length;
    void* data;
} BallerinaArray;

// Ballerina map structure
typedef struct {
    long size;
    long capacity;
    void* data;
} BallerinaMap;

// Runtime function implementations

void ballerina_io_println(char* str) {
    printf("%s\n", str);
}

char* ballerina_lang_string_concat(char* str1, char* str2) {
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    char* result = malloc(len1 + len2 + 1);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, str1, len1);
    memcpy(result + len1, str2, len2);
    result[len1 + len2] = '\0';
    return result;
}

BallerinaArray* ballerina_lang_array_new(long size, long elementSize) {
    BallerinaArray* array = malloc(sizeof(BallerinaArray));
    if (array == NULL) {
        return NULL;
    }
    array->length = size;
    array->data = calloc(size, elementSize);
    if (array->data == NULL) {
        free(array);
        return NULL;
    }
    return array;
}

BallerinaMap* ballerina_lang_map_new() {
    BallerinaMap* map = malloc(sizeof(BallerinaMap));
    if (map == NULL) {
        return NULL;
    }
    map->size = 0;
    map->capacity = 16; // Initial capacity
    map->data = malloc(map->capacity * sizeof(void*));
    if (map->data == NULL) {
        free(map);
        return NULL;
    }
    return map;
}

// Memory management functions

void ballerina_free_string(BallerinaString* str) {
    if (str != NULL) {
        free(str->data);
        free(str);
    }
}

void ballerina_free_array(BallerinaArray* arr) {
    if (arr != NULL) {
        free(arr->data);
        free(arr);
    }
}

void ballerina_free_map(BallerinaMap* map) {
    if (map != NULL) {
        free(map->data);
        free(map);
    }
} 