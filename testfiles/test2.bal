import ballerina/io;

public function main() {
    int age = 30 + 5;
    string message = "Hello Ballerina";
    boolean isAwesome = true;

    string status = "not eligible";
    if age > 18 {
        io:println("Adult");
        status = "eligible";
    } else {
        io:println("Minor");
    }
    io:println(message);
    io:println(status);
    io:println(isAwesome);
}

function calculateSum(int a, int b) returns int {
    int sum = a + b;
    return sum;
}