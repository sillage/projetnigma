CC=gcc
CFLAGS=
LDFLAGS= -ansi -pedantic -W -Wall -Werror
SOURCES= stega.c
OBJECTS= stega.o
EXECUTABLE=stegano

all: $(SOURCES) $(EXECUTABLE)
$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
clean:
	rm *~ *#
