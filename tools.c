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

#include "tools.h"

#include <stdlib.h>
#include <math.h>

extern SDL_Surface *g_screen;

unsigned
math_random (const unsigned min, const unsigned max)
{
  return (rand () % max) + min;
}

SDL_Surface *
SDL_FilledSurface (const unsigned char r,
		   const unsigned char g, const unsigned char b)
{
  SDL_Surface *surface = SDL_CreateRGBSurface (SDL_SWSURFACE,
					       g_screen->w, g_screen->h,
					       g_screen->format->BitsPerPixel,
					       0, 0, 0, 0);
  if (!surface)
    {
      fputs (SDL_GetError (), stderr);
      exit (1);
    }
  const uint32_t color = SDL_MapRGB (surface->format, r, g, b);
  if (SDL_FillRect (surface, NULL, color) != 0)
    {
      fputs (SDL_GetError (), stderr);
    }
  return surface;
}

unsigned
length (const unsigned int x1, const unsigned int x2)
{
  return sqrt (x1 * x1 + x2 * x2);
}
