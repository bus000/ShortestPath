CC      =gcc
CFLAGS  =-Wall -Werror -Wno-parentheses -std=gnu99 -D DEBUG_LEVEL=ERROR_LVL
SOURCES =$(wildcard *.c)$
OBJECTS =$(SOURCES:.c=.o)$
TARGET  =../bin/diff
LFLAGS  =-L/usr/local/lib -lm

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) -o $(TARGET) $(LFLAGS)

%.o: %.c
	$(CC) -o $@  $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
