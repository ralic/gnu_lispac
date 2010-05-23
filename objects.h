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

#ifndef _objects_H_
#define _objects_H_

#include <stdbool.h>
#include <stdint.h>
#include <SDL/SDL.h>

#include "tools.h"

#define FONT_MENU_SIZE (g_screen->w / 45)
#define FONT_INFO_SIZE (g_screen->h / 80)
#define SPRITE_SIZE 32
#define MIN_MAP_WIDTH 5
#define MIN_MAP_HEIGHT 5
#define MAX_MAP_WIDTH (g_screen->w / SPRITE_SIZE)
#define MAX_MAP_HEIGHT (g_screen->w / SPRITE_SIZE)

/** Structures and enumerations definitions */

/** Defines a position in the map **/
struct position
{
  int x;
  int y;
};

/** Point types, POINT_LIFE is special because adds a live, and not points **/
enum point_type
{
  POINT_NONE = 0,
  POINT_COIN = 50,
  POINT_BIG_COIN = 100,
  POINT_CHERRY = 250,
  POINT_STRAWBERRY = 500,
  POINT_MELON = 750,
  POINT_WATERMELON = 1000,
  POINT_BANANA = 2000,
  POINT_GOLDEN_CHERRY = 5000,
  POINT_DIAMOND = 10000,
  POINT_LIFE = 1,
};

/** A tile in the map **/
struct tile
{
  bool walkable;
  enum point_type point_type;
  SDL_Surface *surface;
};

struct entity
{
  bool is_player;
  struct position position;
  SDL_Surface *surface;
};

struct player
{
  bool is_player;
  struct position position;
  SDL_Surface *surface;

  char *name;
  uint64_t points;
  unsigned int lifes;
};

struct monster
{
  bool is_player;
  struct position position;
  SDL_Surface *surface;

  bool is_weak;
};

struct map
{
  unsigned int height;
  unsigned int width;
  struct tile **tiles;
  struct entity **entities;
  unsigned int entity_count;
  struct position monster_spawn;
  struct position player_spawn;
};

/** Entity functions **/
/** Monsters and players can call those functions **/
void walk_left (struct entity *entity);
void walk_right (struct entity *entity);
void walk_up (struct entity *entity);
void walk_down (struct entity *entity);

/** Map functions **/
/** The map schedules all global functions, like adding points, killing,
 * teleporting and so on. **/
bool can_walk_left (const struct entity *entity, const struct map *map);
bool can_walk_right (const struct entity *entity, const struct map *map);
bool can_walk_up (const struct entity *entity, const struct map *map);
bool can_walk_down (const struct entity *entity, const struct map *map);

struct entity *check_entity_collision (const struct map *map,
				       const struct entity *entity);
void collide (struct map *map, struct entity *ent1, struct entity *ent2);
void teleport_start (const struct map *map, struct entity *entity);

void add_point (struct map *map, const struct position pos,
		const enum point_type type);
void get_points (struct map *map, struct entity *entity);
bool generate_map (struct map *map);

#endif
