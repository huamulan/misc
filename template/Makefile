CC = gcc
CFLAGS = -Wall -g -O
LIB = -lSDL
#INCLUDE = -I/usr/include/

TARGET = YUVPlayer

SRC = $(wildcard *.c *.o)
OBJ = $(patsubst %.c, %.o, $(SRC))

%.o : %.c, %.h
	$(CC) -c $(CFLAGS) $< -o $@

$(TARGET) : $(OBJ)
	$(CC) $(CFLAGS) $^ -o $@ $(INCLUDE) $(LIB)
	strip $(TARGET) $(OBJ)

.PHONY: clean

clean:
	rm -rf $(TARGET) $(OBJ)
