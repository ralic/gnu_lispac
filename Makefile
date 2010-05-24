##  Copyright (C) 2010  Mario Castelan Castro
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
