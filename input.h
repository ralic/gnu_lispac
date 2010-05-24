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

#ifndef _input_H_
#define _input_H_

#include "objects.h"

#include <SDL/SDL.h>
#include <string.h>

struct keyboard_input
{
  int event;
  int key;
  int modifiers;
};

struct mouse_input
{
  int event;
  struct position game_position;
  struct position raw_position;
  struct position relative;	// only on SDL_MOUSEMOTION event
};

struct mouse_input_area
{
  struct position from;
  struct position to;
};

struct mouse_click_info
{
  struct position game_position;
  struct position raw_position;
};

struct keyboard_input get_keyboard_event (void);
/** int getchar() -> from standard input **/
/** high level keyboard checks **/
bool is_key_pressed (const uint8_t key);

/** Mouse **/

struct mouse_input get_mouse_event (void);
struct mouse_click_info get_mouse_click (void);

#endif
