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

#include "objects.h"
#include "screen.h"
#include "tools.h"

#include <time.h>
#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>

SDL_Surface *g_screen;
SDL_Surface *g_score;
SDL_Surface *g_lifes;
SDL_Surface *g_menu_info;
TTF_Font *g_font;
struct map *g_map;
SDL_Surface *g_walkable_surface;
SDL_Surface *g_wall_surface;
SDL_Surface *g_state_bar;

int
main (void)
{
  srand (time (NULL));
  if (SDL_Init (SDL_INIT_AUDIO | SDL_INIT_VIDEO) < 0)
    {
      fputs (SDL_GetError (), stderr);
      return EXIT_FAILURE;
    }
  if (TTF_Init () < 0)
    {
      fputs (TTF_GetError (), stderr);
      return EXIT_FAILURE;
    }
  init_screen ();
  menu_screen ();
  g_walkable_surface = SDL_FilledSurface (0, 0, 0);
  g_wall_surface = SDL_FilledSurface (255, 0, 0);
  SDL_Delay (1000);
  g_map = malloc (sizeof (struct map));
  generate_map (g_map);
  game_screen ();
  getchar ();
  SDL_Quit ();
}
