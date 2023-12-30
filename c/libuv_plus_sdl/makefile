UVDIST=/home/kashyap/libuv/dist
SDLDIST=/home/kashyap/libuv/SDL/dist

all: main.exe server.exe onethread.exe

%.exe:%.c
	gcc -g3 -fsanitize=address -I $(UVDIST)/include -L $(UVDIST)/lib -I $(SDLDIST)/include -L $(SDLDIST)/lib $< -o $@ -lSDL2 -luv
