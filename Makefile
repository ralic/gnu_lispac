CFLAGS= -W -I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT

main: map.o screen.o tools.o main.o
	gcc -lSDL -lSDL_ttf -lm -o main map.o screen.o tools.o main.o

map.o: map.c objects.h tools.h
	gcc $(CFLAGS) -c map.c

screen.o: screen.c screen.h
	gcc $(CFLAGS) -c screen.c

tools.o: tools.c tools.h
	gcc $(CFLAGS) -c tools.c

main.o: main.c objects.h screen.h tools.h
	gcc $(CFLAGS) -c main.c

clean:
	rm *.o main