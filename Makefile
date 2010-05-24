##  Copyright (C) 2010  Mario Castelan Castro
## 	Copyright (C) 2010 Kevin Mas

##  This file is part of Lispac.
##
##  Lispac is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  Lispac is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with Lispac.  If not, see <http://www.gnu.org/licenses/>.

CFLAGS = -W $(shell sdl-config --cflags)
SDL_LDFLAGS := $(shell sdl-config --libs) -lSDL_ttf

LDFLAGS += $(SDL_LDFLAGS)
LDFLAGS += -lm

main: map.o screen.o tools.o main.o input.o
	gcc $(LDFLAGS) -o main map.o screen.o tools.o main.o input.o

input.o: input.c input.h objects.h
	gcc -c input.c $(CFLAGS)

map.o: map.c objects.h tools.h
	gcc -c map.c $(CFLAGS)

screen.o: screen.c screen.h
	gcc -c screen.c $(CFLAGS) 

tools.o: tools.c tools.h
	gcc -c tools.c $(CFLAGS) 

main.o: main.c objects.h screen.h tools.h
	gcc -c main.c $(CFLAGS)

clean:
	rm *.o main
