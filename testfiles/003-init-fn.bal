import ballerina/io;

// Integer variable
int value;

// String variable
string name;

function init() returns error? {
    io:println("Starting initialization...");
    value = 2; // Change to a value less than 3 to avoid the error
    
    name = "James";
    io:println("Set name to: " + name);
    
    if value > 3 {
        io:println("Error: Value should be less than 3");
        return error("Value should be less than 3");
    }
    
    io:println("Initialization complete");
    return;
}

public function main() {
    io:println("Main function executing");
    io:println("Value: " + value.toString());
    io:println("Name: " + name);
}
