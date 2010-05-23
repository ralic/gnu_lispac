/* Copyright (C) 2010  Kevin Mas
 * This file is part of Lispac.
 *
 *  Lispac is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Lispac is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Lispac.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef _tools_H_
#define _tools_H_

#include <SDL/SDL.h>

/** Some Lisp called functions and other tools **/
unsigned math_random (const unsigned min, const unsigned max);
SDL_Surface *SDL_FilledSurface (const unsigned char r,
				const unsigned char g, const unsigned char b);
unsigned length (const unsigned int x1, const unsigned int x2);

#endif
