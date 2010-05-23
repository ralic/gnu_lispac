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

extern struct map *g_map;

void
walk_left (struct entity *entity)
{
  if (can_walk_left (entity, g_map) == true)
    {
      entity->position.x -= 1;
      struct entity *collider = check_entity_collision (g_map, entity);
      if (collider != NULL)
	{
	  collide (g_map, entity, collider);
	}
    }
}

void
walk_right (struct entity *entity)
{
  if (can_walk_right (entity, g_map) == true)
    {
      entity->position.x += 1;
      struct entity *collider = check_entity_collision (g_map, entity);
      if (collider != NULL)
	{
	  collide (g_map, entity, collider);
	}
    }
}

void
walk_up (struct entity *entity)
{
  if (can_walk_up (entity, g_map) == true)
    {
      entity->position.y -= 1;
      struct entity *collider = check_entity_collision (g_map, entity);
      if (collider != NULL)
	{
	  collide (g_map, entity, collider);
	}
    }
}

void
walk_down (struct entity *entity)
{
  if (can_walk_down (entity, g_map) == true)
    {
      entity->position.y += 1;
      struct entity *collider = check_entity_collision (g_map, entity);
      if (collider != NULL)
	{
	  collide (g_map, entity, collider);
	}
    }
}
