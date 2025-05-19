CC := cc
CFLAGS := -Wall -Werror -g
LDFLAGS := 

RUNTIME_OBJ := runtime.o

.PHONY: all clean

all: $(RUNTIME_OBJ)

$(RUNTIME_OBJ): runtime.c runtime.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(RUNTIME_OBJ)