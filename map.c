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
#include "tools.h"

#include <assert.h>

extern SDL_Surface *g_walkable_surface;
extern SDL_Surface *g_wall_surface;
extern SDL_Surface *g_screen;

bool
check_colindant (const struct map *map, const int x, const int y)
{
  if (x == 0 || y == 0 || x == map->width - 1 || y == map->height - 1)
    {
      return false;
    }
  if (map->tiles[x - 1][y].walkable && map->tiles[x + 1][y].walkable)
    {
      return false;
    }
  if (map->tiles[x][y - 1].walkable && map->tiles[x][y + 1].walkable)
    {
      return false;
    }
  return true;
}

static void
create_path (struct map *map, const struct position *A,
	     const struct position *B)
{
  register int x, y;
  if (A->x > B->x)
    {
      for (x = A->x; x > B->x; --x)
	{
	  if (!check_colindant (map, x, A->y))
	    {
	      break;
	    }
	  map->tiles[x][A->y].walkable = true;
	  map->tiles[x][A->y].surface = g_walkable_surface;
	}
    }
  else
    {
      for (x = A->x; x < B->x; ++x)
	{
	  if (!check_colindant (map, x, A->y))
	    {
	      break;
	    }
	  map->tiles[x][A->y].walkable = true;
	  map->tiles[x][A->y].surface = g_walkable_surface;
	}
    }
  if (A->y > B->y)
    {
      for (y = A->y; y > B->y; --y)
	{
	  if (!check_colindant (map, y, A->x))
	    {
	      break;
	    }
	  map->tiles[x][A->y].walkable = true;
	  map->tiles[x][A->y].surface = g_walkable_surface;
	}
    }
  else
    {
      for (y = A->y; y < B->y; ++y)
	{
	  if (!check_colindant (map, y, A->x))
	    {
	      break;
	    }
	  map->tiles[x][A->y].walkable = true;
	  map->tiles[x][A->y].surface = g_walkable_surface;
	}
    }
}

static void
optimize_tile (struct map *map, const int x, const int y)
{
  if (map->tiles[x][y + 1].walkable && map->tiles[x][y + 2].walkable)
    {
      map->tiles[x][y + 1].walkable = false;
      map->tiles[x][y + 1].surface = g_wall_surface;
    }
  if (map->tiles[x + 1][y].walkable && map->tiles[x + 2][y].walkable)
    {
      if (!map->tiles[x][y - 1].walkable || !map->tiles[x][y + 1].walkable)
	{
	  map->tiles[x + 1][y].walkable = false;
	  map->tiles[x + 1][y].surface = g_wall_surface;
	}
    }
  if (!map->tiles[x - 1][y].walkable && !map->tiles[x][y - 1].walkable &&
      !map->tiles[x + 1][y].walkable && !map->tiles[x][y + 1].walkable)
    {
      map->tiles[x][y + 1].walkable = true;
      map->tiles[x][y + 1].surface = g_walkable_surface;
    }
}

static void
optimize_walks (struct map *map)
{
  register int x = 0, y = 0;
  for (; x < map->width; ++x)
    {
      for (; y < map->height; ++y)
	{
	  if (map->tiles[x][y].walkable)
	    {
	      optimize_tile (map, x, y);
	    }
	}
    }
}

static void
generate_walkable (struct map *map)
{
  register int it = 0;
  for (it = 0; it < sqrt (map->width * map->width +
			  map->height * map->height); ++it)
    {
      const struct position A = {
	.x = math_random (0, map->width),
	.y = math_random (0, map->height)
      }, B =
      {
      .x = math_random (0, map->width),.y = math_random (0, map->height)}, C =
      {
      .x = math_random (0, map->width),.y = math_random (0, map->height)}, P =
      {
      .x = abs (((B.x - C.x) / 2) + B.x) / 2,.y =
	  abs (((B.y - C.y) / 2) + B.y) / 2}, p =
      {
      .x = abs (((A.x - P.x) / 2) + A.x) / 2,.y =
	  abs (((A.y - P.y) / 2) + A.y) / 2}, X =
      {
      .x = abs (((A.x - p.x) / 2) + p.x) / 2,.y =
	  abs (((A.y - p.y) / 2) + p.y) / 2}, Z =
      {
      .x = abs (((A.x - p.x) / -2) + p.x) / 2,.y =
	  abs (((A.y - p.y) / -2) + p.y) / 2};
      printf ("A(%d,%d)\n" "B(%d,%d)\n" "C(%d,%d)\n" "P(%d,%d)\n" "p(%d,%d)\n"
	      "X(%d,%d)\n" "Z(%d,%d)\n", A.x, A.y, B.x, B.y, C.x, C.y, P.x,
	      P.y, p.x, p.y, X.x, X.y, Z.x, Z.y);
      create_path (map, &B, &p);
      create_path (map, &B, &A);
      create_path (map, &C, &P);
      create_path (map, &C, &p);
      create_path (map, &A, &X);
      create_path (map, &A, &Z);
      create_path (map, &p, &X);
      create_path (map, &p, &Z);
    }
}



static void
generate_walls (struct map *map)
{
  register int x;
  for (x = 0; x < map->width; ++x)
    {
      register int y = 0;
      for (; y < map->height; ++y)
	{
	  map->tiles[x][y].walkable = false;
	  map->tiles[x][y].surface = g_wall_surface;
	}
    }
}

static bool
position_equals (const struct entity *et1, const struct entity *et2)
{
  return (et1->position.x == et2->position.x &&
	  et2->position.y == et2->position.y);
}

bool
can_walk_left (const struct entity * entity, const struct map * map)
{
  const int x = entity->position.x - 1;
  const int y = entity->position.y;
  if (x < 0)
    {
      return false;
    }
  struct tile *target = &map->tiles[x][y];
  return target->walkable;
}

bool
can_walk_right (const struct entity * entity, const struct map * map)
{
  const int x = entity->position.x + 1;
  const int y = entity->position.y;
  if (x >= map->width)
    {
      return false;
    }
  struct tile *target = &map->tiles[x][y];
  return target->walkable;
}

bool
can_walk_up (const struct entity * entity, const struct map * map)
{
  const int x = entity->position.x;
  const int y = entity->position.y - 1;
  if (y < 0)
    {
      return false;
    }
  struct tile *target = &map->tiles[x][y];
  return target->walkable;
}

bool
can_walk_down (const struct entity * entity, const struct map * map)
{
  const int x = entity->position.x;
  const int y = entity->position.y + 1;
  if (y >= map->height)
    {
      return false;
    }
  struct tile *target = &map->tiles[x][y];
  return target->walkable;
}


struct entity *
check_entity_collision (const struct map *map, const struct entity *entity)
{
  int it = 0;
  for (; it < map->entity_count; ++it)
    {
      if (position_equals (entity, map->entities[it]))
	{
	  return map->entities[it];
	}
    }
  return NULL;
}

void
collide (struct map *map, struct entity *ent1, struct entity *ent2)
{
  if (!ent1->is_player && !ent2->is_player)
    {
      teleport_start (map, ent1);
      teleport_start (map, ent2);
      return;
    }
  struct player *player = ent1->is_player ? ent1 : ent2;
  struct monster *monster = ent1->is_player ? ent2 : ent1;
  if (monster->is_weak)
    {
      teleport_start (map, monster);
      player->points += 250;
      return;
    }
  else
    {
      teleport_start (map, player);
      player->lifes--;
    }
}

void
teleport_start (const struct map *map, struct entity *entity)
{
  if (entity->is_player)
    {
      entity->position.x = map->player_spawn.x;
      entity->position.y = map->player_spawn.y;
    }
  else
    {
      entity->position.x = map->monster_spawn.x;
      entity->position.y = map->monster_spawn.y;
    }
}

void
add_point (struct map *map, const struct position pos,
	   const enum point_type type)
{
  if (map->tiles[pos.x][pos.y].point_type == POINT_NONE)
    {
      map->tiles[pos.x][pos.y].point_type = type;
    }
}

void
get_points (struct map *map, struct entity *entity)
{
  if (!entity->is_player)
    {
      return;
    }
  struct player *player = entity;
  struct tile *tile = &map->tiles[player->position.x][player->position.y];
  switch (tile->point_type)
    {
    case POINT_LIFE:
      player->lifes++;
    case POINT_NONE:
      break;
    default:
      player->points += tile->point_type;
    }
  tile->point_type = POINT_NONE;
}

bool
generate_map (struct map *map)
{
  map->height = math_random (MIN_MAP_HEIGHT, MAX_MAP_HEIGHT);
  map->width = math_random (MIN_MAP_WIDTH, MAX_MAP_WIDTH);
  map->tiles = malloc (sizeof (char *) * map->width);
  printf ("Map width %u height %u\n", map->width, map->height);
  if (!map->tiles)
    {
      return false;
    }
  register int x = 0;
  for (; x < map->width; ++x)
    {
      map->tiles[x] = malloc (sizeof (struct tile) * map->height);
    }
  generate_walls (map);
  puts ("Walls generated");
  generate_walkable (map);
  return true;
}
