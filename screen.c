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

#include "screen.h"

#include <stdio.h>
#include <SDL/SDL_ttf.h>

extern SDL_Surface *g_screen;
extern SDL_Surface *g_score;
extern SDL_Surface *g_lifes;
extern SDL_Surface *g_position;
extern SDL_Surface *g_menu_info;
extern SDL_Surface *g_state_bar;
extern TTF_Font *g_font;
extern struct map *g_map;

static void
print_state_bar (void)
{
  g_font = TTF_OpenFont ("inc/lispacman.ttf", FONT_INFO_SIZE);
  if (!g_font)
    {
      fputs (TTF_GetError (), stderr);
      abort ();
    }
  TTF_SetFontStyle (g_font, 0);
  g_state_bar = SDL_FilledSurface (255, 255, 255);
  SDL_Rect rect = {
    .x = (g_screen->w / 10) * 9,
    .y = g_screen->h,
    .w = (g_screen->w / 10),
    .h = g_screen->h
  };
  SDL_BlitSurface (g_state_bar, NULL, g_screen, &rect);
  SDL_FreeSurface (g_state_bar);
}

static bool
add_menu_entry (const unsigned int padding, const unsigned int line,
		const char *text, const unsigned char r,
		const unsigned char g, const unsigned char b)
{
  unsigned int width;
  TTF_SizeText (g_font, text, &width, NULL);
  const unsigned int height = TTF_FontHeight (g_font);
  const SDL_Rect rect = {
    .x = padding * (width / strlen (text)),
    .y = line * height,
    .w = width,
    .h = height
  };
  const SDL_Color color = { r, g, b };
  SDL_Surface *surface = TTF_RenderText_Blended (g_font, text, color);
  if (!surface)
    {
      fputs (TTF_GetError (), stderr);
      return false;
    }
  SDL_BlitSurface (surface, NULL, g_screen, &rect);
  SDL_FreeSurface (surface);
}

static void
print_menu_text (void)
{
  g_font = TTF_OpenFont ("inc/lispacman.ttf", FONT_MENU_SIZE);
  if (!g_font)
    {
      fputs (TTF_GetError (), stderr);
      abort ();
    }
  TTF_SetFontStyle (g_font, 0);
  add_menu_entry (0, 0, "Select mode:", 0xFF, 0, 0);
  add_menu_entry (5, 1, "1) Lisp mode:", 0, 0xFF, 0);
  add_menu_entry (5, 2, "2) Manual mode:", 0, 0, 0xFF);
  TTF_CloseFont (g_font);
}

void
init_screen (void)
{
  const SDL_VideoInfo *best_video = SDL_GetVideoInfo ();
  g_screen = SDL_SetVideoMode (best_video->current_w / 2,
			       best_video->current_h / 2,
			       best_video->vfmt->BitsPerPixel, SDL_SWSURFACE);
  if (!g_screen)
    {
      fputs (SDL_GetError (), stderr);
      abort ();
    }
}


void
menu_screen (void)
{
  const uint32_t black = SDL_MapRGB (g_screen->format, 0, 0, 0);
  SDL_FillRect (g_screen, NULL, black);
  print_menu_text ();
  SDL_Flip (g_screen);
}

void
game_screen (void)
{
  const uint32_t black = SDL_MapRGB (g_screen->format, 0, 0, 0);
  SDL_FillRect (g_screen, NULL, black);
  printf ("Map height %d width %d\n", g_map->height, g_map->width);
  unsigned int x = 0;
  for (; x < g_map->width; ++x)
    {
      unsigned int y = 0;
      for (; y < g_map->height; ++y)
	{
	  const SDL_Rect rect = {
	    .x = x * 32,
	    .y = y * 32,
	    .w = 32,
	    .h = 32
	  };
	  SDL_BlitSurface (g_map->tiles[x][y].surface, NULL, g_screen, &rect);
	}
    }
  /*for (x = 0; x < g_map->entity_count; ++x) {
     const SDL_Rect rect = {
     .x = g_map->entities[x]->position.x * 32,
     .y = g_map->entities[x]->position.y * 32,
     .w = 32,
     .h = 32
     };                   
     SDL_BlitSurface(g_map->entities[x]->surface, NULL,
     g_screen, &rect);
     } */
  print_state_bar ();
  SDL_Flip (g_screen);
}

void
update_game_tile (const struct position pos)
{
  const SDL_Rect rect = {
    .x = pos.x * 32,
    .y = pos.y * 32,
    .w = 32,
    .h = 32
  };
  SDL_BlitSurface (g_map->tiles[pos.x][pos.y].surface, &rect, g_screen,
		   &rect);
  SDL_Flip (g_screen);
}

void
update_game_creature (const struct position pos)
{
}
