UVDIST=/home/kashyap/libuv/dist
SDLDIST=/home/kashyap/libuv/SDL/dist

all: main.exe

%.exe:%.c
	gcc -g3 -I $(UVDIST)/include -L $(UVDIST)/lib -I $(SDLDIST)/include -L $(SDLDIST)/lib $< -o $@ -lSDL2 -luv
