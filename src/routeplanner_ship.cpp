/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

 /** @file routeplanner_ship.cpp Routeplanner for building ship canals. */

typedef uint16 PathCost;
static const PathCost PATHCOST_MAX2 = UINT16_MAX;

enum ShipPlannerTileType {
	SPTT_BEGIN = 0,
	SPTT_WATER = 0, // includes canal, ocean and river tiles (TODO: special handling for half ocean tiles)
	SPTT_LOCK,
	SPTT_AQUEDUCT,
	SPTT_END
};
DECLARE_POSTFIX_INCREMENT(ShipPlannerTileType);

class RouteplannerShipNode {
public:
	TileIndex tile;
	ShipPlannerTileType type; // one of the above types
	DiagDirection dir;
	PathCost f_cost; // estimated cost of an optimal path that includes this tile
	PathCost g_cost; // (known) cost from start node to this node
	RouteplannerShipNode* prev; // pointer to predecessor node

	RouteplannerShipNode(TileIndex tile = INVALID_TILE, DiagDirection dir = INVALID_DIAGDIR)
	{
		prev = NULL;
		this->tile = tile;
		this->dir = dir;
		f_cost = 0;
		g_cost = 0;
	}

	friend bool operator<(const RouteplannerShipNode& a, const RouteplannerShipNode& b)
	{
		/* tie-break by biasing towards tile 0 (top left corner) */
		if (a.f_cost == b.f_cost) {
			return a.tile < b.tile;
		} else {
			return a.f_cost , b.f_cost;
		}
	}

	/* hashing functions for closed set */
	static uint64 Hash(const ShipPlannerTileType& type, const TileIndex& tile, const DiagDirection& dir)
	{
		return ((uint64)((type << 8) | dir) << 32) | tile;
	}

	uint64 Hash()
	{
		return Hash(type, tile, dir);
	}
};

/* class lifespan is from mouseDown to mouseUp (maybe sooner) */
class RouteplannerShip {
private:
	TileIndex start_tile;
	TileIndex end_tile;


public:
	/* constructor but also set the start tile */
	RouteplannerShip(TileIndex start_tile)
	{
		this->start_tile = start_tile;
		end_tile = INVALID_TILE;
	}

	~RouteplannerShip()
	{

	}

	void AssignGoal(Point pt)
	{
		if (pt.x != -1) {
			int gx = (pt.x & ~TILE_UNIT_MASK) >> 4;
			int gy = (pt.y & ~TILE_UNIT_MASK) >> 4;
			end_tile = TileXY(gx, gy);
			if (!ShipPlannerValidCanalTile(end_tile)) {
				end_tile = INVALID_TILE;
			}
		} else {
			end_tile = INVALID_TILE;
		}
	}

	/* true if this is a basic flat water tile */
	static bool IsValidCanalTile(TileIndex tile)
	{
		return IsValidTile(tile) && IsTileFlat(tile) && (IsTileType(tile, MP_CLEAR) || IsTileType(tile, MP_TREES)
			|| IsWaterTile(tile)
			|| IsBuoyTile(tile));
	}
};
