// Basic type definitions for the Ballerina HTTP module.
// The actual implementation is in the runtime.

public type Listener object {
    // Configuration for the listener, e.g., port
    public int port;

    // Constructor
    public function init(int port) {
        self.port = port;
        // In a full implementation, this might interact with the runtime
        // to prepare the listener, or the `service on new Listener()`
        // construct handles the actual runtime server start.
    }

    // Lifecycle methods (attach, detach, start, stop) would go here
    // For now, the `service on new Listener()` construct will directly
    // trigger runtime server start via the backend.
};

// Placeholder for Request and Response types.
// These would have many more fields and methods in a full implementation.
public type Request object {
    public string path = "";
    public string method = "";
    // headers, body, etc.
};

public type Response object {
    public int statusCode = 200;
    public string|json|xml|byte[] body = "";
    // headers, etc.

    public function setPayload(string|json|xml|byte[] payload) {
        self.body = payload;
    }
};

// Other HTTP related types like Headers, StatusCodes, etc.