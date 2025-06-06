CC := cc
CFLAGS := -Wall -Werror -g
LDFLAGS := 

RUNTIME_OBJ := runtime.o
HTTP_SERVER_OBJ := http_server.o
GO_HTTP_LIB := http_server_go.a
OBJS := runtime.o http_server.o $(GO_HTTP_LIB)

.PHONY: all clean rebuild

all: $(OBJS)

rebuild: clean all

$(RUNTIME_OBJ): runtime.c runtime.h
	$(CC) $(CFLAGS) -c -o $@ $<

$(HTTP_SERVER_OBJ): http_server.c http_server.h runtime.h
	$(CC) $(CFLAGS) -c http_server.c -o http_server.o

$(GO_HTTP_LIB): http_server.go
	go build -buildmode=c-archive -o $(GO_HTTP_LIB) http_server.go

clean:
	rm -f $(OBJS) http_server_go.h