CC        =gcc
SRCDIR    =../src/
CFLAGS    =-Wall -g -ansi -Werror -I$(SRCDIR)
SOURCES   =$(wildcard *.c)$
OBJECTS   =$(SOURCES:.c=.o)$
EXOBJECTS =$(shell find '../src/' -name '*.o' | grep -v main.o)$
TARGETS   =$(SOURCES:.c=)$
BIN_DIR   =./test_bin
LFLAGS    =-lm -lpthread

.PHONY: clean $(TARGETS)

all: $(TARGETS)
	./run_tests.hs

$(TARGETS): $(OBJECTS) $(BIN_DIR)
	$(CC) $(CFLAGS) $@.o $(EXOBJECTS) -o $(BIN_DIR)/$@ $(LFLAGS)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -f $(OBJECTS) $(BIN_DIR)/*
	rmdir $(BIN_DIR)
	rm -f *output.txt
