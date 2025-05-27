CC := cc
CFLAGS := -Wall -Werror -g
LDFLAGS := 

RUNTIME_OBJ := runtime.o
OBJS := runtime.o http_server.o

.PHONY: all clean

all: $(OBJS)

$(RUNTIME_OBJ): runtime.c runtime.h
	$(CC) $(CFLAGS) -c -o $@ $<

http_server.o: http_server.c http_server.h runtime.h
	$(CC) $(CFLAGS) -c http_server.c -o http_server.o

clean:
	rm -f $(OBJS)