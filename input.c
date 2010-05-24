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

#include "input.h"

static void
get_SDL_event (SDL_Event * event)
{
  if (!SDL_PollEvent (event))
    {
      SDL_WaitEvent (event);
    }
}

struct keyboard_input
get_keyboard_event (void)
{
  struct keyboard_input input = { 0, 0, 0 };
  SDL_Event input_event;
  get_SDL_event (&input_event);
  switch (input_event->type)
    {
    case SDL_KEYDOWN:
    case SDL_KEYUP:
      input.event = input_event->type;
      input.key = input_event->type.key.keysym.sym;
      input_modifiers = input_event->key.keysym.mod;
    }
  return input;
}

bool
is_key_pressed (const uint8_t key)
{
		/** TODO: killing the array? maybe SDL uses a dynamic
		 * array, or maybe is a readonly static array. **/

  const uint8_t *keys = SDL_GetKeyboardState (NULL);
  return keys[key];
}

struct mouse_input
get_mouse_event (void)
{
  struct mouse_input input = { 0, {0, 0}, {0, 0} };
  SDL_Event input_event;
  get_SDL_event (&input_event);
  switch (input_event->type)
    {
    case SDL_MOUSEMOTION:
      input.event = SDL_MOUSEMOTION;
      input.relative.x = input_event->motion.xrel;
      input.relative.y = input_event->motion.yrel;
      input.raw_position.x = input_event->motion.x;
      input.raw_position.y = input_event->motion.y;
      break;
    case SDL_MOUSEBUTTONUP:
    case SDL_MOUSEBUTTONDOWN:
      input.event = input_event->type;
      input.raw_position.x = input_event->button.x;
      input.raw_position.y = input_event->button.y;
      break;
    }
  if (input.event != 0)
    {
      input.game_position.x = input.raw_position.x / SPRITE_SIZE;
      input.game_position.y = input.raw_position.y / SPRITE_SIZE;
    }
  return input;
}
