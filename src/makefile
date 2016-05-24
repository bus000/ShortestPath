CC      =gcc
CFLAGS  =-Wall -g -Werror -Wno-parentheses -std=gnu99
SOURCES =$(wildcard *.c)$
OBJECTS =$(SOURCES:.c=.o)$
TARGET  =../bin/diff
LFLAGS  =-L/usr/local/lib -lm -lpthread

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) -o $(TARGET) $(LFLAGS)

%.o: %.c
	$(CC) -o $@  $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
