/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

 /** @file planner.h Functions related to planner build tools. */

#ifndef BUILD_PLANNER
#define BUILD_PLANNER

#include <queue>
#include <unordered_map>
#include <unordered_set>

typedef uint16 PathCost;
#define PATHCOST_MAX UINT16_MAX

enum ShipPlannerTileType {
	SPTT_BEGIN = 0,
	SPTT_WATER = 0, // includes canal, ocean and river tiles (TODO: special handling for half ocean tiles)
	SPTT_LOCK,
	SPTT_AQUEDUCT,
	SPTT_END
};

DECLARE_POSTFIX_INCREMENT(ShipPlannerTileType)

typedef struct _ship_node {
	TileIndex tile;
	ShipPlannerTileType type; // one of the above types
	DiagDirection dir;
	PathCost f_cost; // estimated cost of an optimal path that includes this tile
	PathCost g_cost; // (known) cost from start node to this node
	_ship_node* prev; // pointer to predecessor node
} *ShipNode;

struct CompareShipNodes {
	bool operator ()(const ShipNode& a, const ShipNode& b)
	{
		if (a->f_cost == b->f_cost) {
			return a->tile > b->tile;
		} else {
			return a->f_cost > b->f_cost;
		}
	}
};

typedef std::priority_queue<ShipNode, std::vector<ShipNode>, CompareShipNodes> ShipNodeQueue;
typedef std::unordered_map<uint64, ShipNode> ShipNodeSet;
typedef std::unordered_set<TileIndex> planner_tileindex_set;
extern planner_tileindex_set PathHighlightSet; // highlight the finished path for the various planner tools

extern TileIndex ship_planner_start_tile;
extern TileIndex ship_planner_end_tile;

inline ShipNode newShipNode(TileIndex tile = INVALID_TILE, DiagDirection dir = INVALID_DIAGDIR)
{
	ShipNode new_node = new _ship_node();
	new_node->prev = NULL;
	new_node->tile = tile;
	new_node->dir = dir;
	new_node->f_cost = 0;
	new_node->g_cost = 0;
	return new_node;
}

// create a key from a node, for use in ClosedSet
inline uint64 HashShipNode(const ShipPlannerTileType& type, const TileIndex& tile, const DiagDirection& dir)
{
	return ((uint64)((type << 8) | dir) << 32) | tile;
}

inline uint64 HashShipNode(const ShipNode& node)
{
	return HashShipNode(node->type, node->tile, node->dir);
}

// check whether a canal can be built on this tile
inline bool ShipPlannerValidCanalTile(const TileIndex& tile)
{
	return IsValidTile(tile) && IsTileFlat(tile) && (IsTileType(tile, MP_CLEAR) || IsTileType(tile, MP_TREES) || IsWaterTile(tile) || IsBuoyTile(tile));
}

inline TileIndex GetFacingTile(ShipNode node)
{
	TileIndex neighbour_facing_tile = INVALID_TILE;
	switch (node->type) {
		case (SPTT_WATER):
			// the facing tile is just  adjacent to the canal tile
			return TileAddByDiagDir(node->tile, node->dir);

		case (SPTT_LOCK):
			// the facing tile is at the end of the lock, i.e. two tiles from its centre
			TileIndex mid = TileAddByDiagDir(node->tile, node->dir);
			if (!IsValidTile(mid)) {
				return INVALID_TILE;
			} else {
				return TileAddByDiagDir(mid, node->dir);
			}

		default:
			NOT_REACHED();
	}
}

#endif /* BUILD_PLANNER */
