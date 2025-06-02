%BallerinaHTTPRequest = type { i8*, i8*, { i64, i8* }* }
%BallerinaHTTPResponse = type { i32, { i64, i8* }* }

@.str.0 = global [31 x i8] c"No init function found to call\00"

declare void @ballerina_io_println({ i64, i8* }* %value)

declare void @ballerina_io_println_array({ i64, i8* }* %array)

declare { i64, i8* }* @ballerina_lang_array_new(i64 %size, i64 %elementSize)

declare void @ballerina_lang_array_set({ i64, i8* }* %arr, i64 %index, i8* %value)

declare { i64, i8* }* @ballerina_lang_string_concat({ i64, i8* }* %str1, { i64, i8* }* %str2)

declare { i64, i64, i8* }* @ballerina_lang_map_new()

declare { i64, i8* }* @ballerina_string_new_with_literal(i8* %data, i64 %len)

declare i8* @malloc(i64 %size)

declare { i64, i8* }* @ballerina_runtime_int_to_string(i64 %val)

declare { i64, i8* }* @ballerina_runtime_bool_to_string(i1 %val)

declare i32 @ballerina_http_server_start(i32 %port, i32 %register_service_handlers_now)

declare void @ballerina_http_register_resource(i8* %path, i8* %method, void (%BallerinaHTTPRequest*, %BallerinaHTTPResponse*)* %handler)

declare void @ballerina_http_server_wait()

declare void @ballerina_http_response_set_string_body(%BallerinaHTTPResponse* %resp, { i64, i8* }* %body_str)

declare void @ballerina_http_response_set_status_code(%BallerinaHTTPResponse* %resp, i32 %status_code)

declare { i64, i8* }* @ballerina_http_request_get_placeholder_body(%BallerinaHTTPRequest* %req)

declare i8* @ballerina_error_create({ i64, i8* }* %message)

declare void @ballerina_error_print(i8* %error)

declare void @ballerina_io_println_float(double %value)

declare void @ballerina_io_println_bool(i1 %value)

declare void @ballerina_io_println_int(i64 %value)

define void @_ballerina_main() {
entry:
	%0 = alloca i64
	%1 = alloca i64
	store i64 1, i64* %0
	store i64 u0xFFFF, i64* %1
	%2 = load i64, i64* %0
	%3 = load i64, i64* %1
	%4 = add i64 %3, %2
	store i64 %4, i64* %1
	%5 = load i64, i64* %1
	call void @ballerina_io_println_int(i64 %5)
	ret void
}

define void @ballerina_main() {
entry:
	%0 = getelementptr [31 x i8], [31 x i8]* @.str.0, i32 0, i32 0
	%1 = call { i64, i8* }* @ballerina_string_new_with_literal(i8* %0, i64 30)
	call void @ballerina_io_println({ i64, i8* }* %1)
	call void @_ballerina_main()
	ret void
}
