/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

 /** @file routeplanner_ship.cpp Routeplanner for building ship canals. */

#include "routeplanner.h"

typedef uint16 PathCost;
static const PathCost PATHCOST_MAX2 = UINT16_MAX;

/*
planner_tileindex_set PathHighlightSet;
TileIndex ship_planner_start_tile;
TileIndex ship_planner_end_tile;
ShipNode best_node;

ShipNodeQueue OpenQueue; // Open set of nodes known but yet to be expanded
ShipNodeSet OpenSet; // same as OpenQueue but used to check for duplicate nodes
ShipNodeSet ClosedSet; // Nodes that have been visited and expanded - uint32 is a hash of tile, direction (if relevant) and type
*/

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
	RouteplannerShipNode best_node;
	/*
	typedef std::priority_queue<ShipNode, std::vector<ShipNode>, CompareShipNodes> ShipNodeQueue;
	typedef std::unordered_map<uint64, ShipNode> ShipNodeSet;
	typedef std::unordered_set<TileIndex> planner_tileindex_set;
	extern planner_tileindex_set PathHighlightSet; // highlight the finished path for the various planner tools
	*/

public:
	/* constructor but also set the start tile */
	RouteplannerShip(TileIndex start_tile)
	{
		this->start_tile = start_tile;
		end_tile = INVALID_TILE;
		/* create a node for each of the four directions */
		for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
			ShipNode first_node = newShipNode(tile);
			first_node->type = SPTT_WATER;
			first_node->dir = dir;
			// Put node_start in the OPEN list with f(node_start) = h(node_start) (initialization)
			OpenQueue.push(first_node);
			OpenSet.insert({ HashShipNode(first_node), first_node });
		}
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

	void TryBuildFinalPath()
	{
		// if they click too quickly, we assume they just clicked on a single tile, so behave just like the regular canal tool
		if (end_tile == ship_planner_start_tile) {
			if (!IsWaterTile(end_tile))	DoCommandP(end_tile, start_tile, WATER_CLASS_CANAL, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_BUILD_CANALS), CcPlaySound_SPLAT_WATER);
			break;
		}
		// check all four directions, find the cheapest node
		ShipNodeSet::iterator itr;
		PathCost cheapest = PATHCOST_MAX;
		ShipNode best_node = nullptr;
		for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
			if ((itr = ClosedSet.find(HashShipNode(SPTT_WATER, ship_planner_end_tile, dir))) != ClosedSet.end() &&
				itr->second->g_cost < cheapest) {
				best_node = itr->second;
				cheapest = best_node->g_cost;
			}
		}
		// build the path if it exists
		// TODO: reduce DoCommandP count by finding all tiles in a straight line
		if (best_node != nullptr) {
			TileIndex start_drag = INVALID_TILE;
			for (ShipNode temp = best_node; temp != nullptr; temp = temp->prev) {
				switch (temp->type) {
					case SPTT_WATER:
						// don't build if on water tile
						if (!(IsWaterTile(temp->tile) || IsBuoyTile(temp->tile))) {
							if (start_drag == INVALID_TILE) {
								start_drag = temp->tile;
							}
							// don't send command now if the next tile is still in a straight line from here
							if (temp->prev == nullptr || temp->prev->type != SPTT_WATER ||
								IsWaterTile(temp->prev->tile) || IsBuoyTile(temp->prev->tile) ||
								DiagdirBetweenTiles(start_drag, temp->prev->tile) == INVALID_DIAGDIR) {
								DoCommandP(start_drag, temp->tile, WATER_CLASS_CANAL, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_BUILD_CANALS), CcPlaySound_SPLAT_WATER);
								start_drag = INVALID_TILE;
							}
						}
						break;

					case SPTT_LOCK:
						// don't build if there's already a lock here
						if (!(IsTileType(temp->tile, MP_WATER) && IsLock(temp->tile) && GetLockPart(temp->tile) == LOCK_PART_MIDDLE)) {
							DoCommandP(temp->tile, 0, 0, CMD_BUILD_LOCK | CMD_MSG(STR_ERROR_CAN_T_BUILD_LOCKS), CcBuildDocks);
						}
						break;

					default:
						break;
				}
			}
		}
		// and then clean up afterwards
		this->Reset();
	}

	void Reset()
	{
		OpenQueue = ShipNodeQueue();
		OpenSet = ShipNodeSet();
		ClosedSet = ShipNodeSet();
		ship_planner_start_tile = INVALID_TILE;
		ship_planner_end_tile = INVALID_TILE;
	}

	void DoPathSearch()
	{
		// exit if either tiles aren't defined
		if (ship_planner_start_tile == INVALID_TILE || ship_planner_end_tile == INVALID_TILE) {
			UpdatePathSet();
			return;
		}
		// or the goal has already been found
		// (we have to check all four directions I guess)
		ShipNodeSet::iterator itr;
		PathCost cheapest = PATHCOST_MAX;
		best_node = nullptr;
		for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
			if ((itr = ClosedSet.find(HashShipNode(SPTT_WATER, ship_planner_end_tile, dir))) != ClosedSet.end() &&
				itr->second->g_cost < cheapest) {
				best_node = itr->second;
				cheapest = best_node->g_cost;
			}
		}
		if (cheapest != PATHCOST_MAX) {
			UpdatePathSet(best_node);
			return;
		}
		// if the goal tile has changed since the last execution, update the heuristic value for all nodes in the queue
		// TODO: actually check if the goal tile has changed
		ShipNodeQueue new_queue = ShipNodeQueue();
		while (!OpenQueue.empty()) {
			ShipNode temp_node = OpenQueue.top();
			OpenQueue.pop();
			temp_node->f_cost = temp_node->g_cost + ShipHeuristic(GetFacingTile(temp_node), ship_planner_end_tile);
			new_queue.push(temp_node);
		}
		OpenQueue = new_queue;

		// Do the A* thingo
		// while the OPEN list is not empty
		// time limit is arbitrarily set - only calculate for 25ms per tick
		using namespace std::chrono;
		high_resolution_clock::time_point until = high_resolution_clock::now() + (milliseconds)25;
		uint16 steps = 0;
		while (!OpenQueue.empty()) {
			/* only check clock every 100 nodes */
			if (steps++ % 100 == 99 && high_resolution_clock::now() > until) {
				break;
			}
			// Take from the open list the node node_current with the lowest
				// f(node_current) = g(node_current) + h(node_current)
			ShipNode node_current = OpenQueue.top();
			OpenQueue.pop();
			OpenSet.erase(HashShipNode(node_current));
			// if node_current is node_goal we have found the solution; don't break - instead keep searching

			// Generate each state node_successor that come after node_current
			// since node_current already has a direction, we only choose directions for canal neighbours
			// offset the "facing" tile for the current node - in the case that it's a LOCK or AQUEDUCT
			TileIndex neighbour_facing_tile = GetFacingTile(node_current);

			// check this tile is within the map, otherwise write off node_current and move on
			if (!IsValidTile(neighbour_facing_tile)) {
				// repeating this one line is better than indenting the following 200 lines :^)
				ClosedSet.insert({ HashShipNode(node_current), node_current });
				continue;
			}

			// try each tile type for the new neighbour
			for (ShipPlannerTileType successor_tiletype = SPTT_BEGIN; successor_tiletype < SPTT_END; successor_tiletype++) {
				TileIndex successor_tile;
				PathCost tentative_cost;
				switch (successor_tiletype) {
					// deal with canals/plain water (including buoys)
					case SPTT_WATER:
						// check it is valid for placement
						successor_tile = neighbour_facing_tile;
						// this check is different to IsValidTile()
						if (!ShipPlannerValidCanalTile(successor_tile)) continue;
						tentative_cost = node_current->g_cost + 2; // magic number oops
						break;

						// deal with locks for vertical movement :)
					case SPTT_LOCK: {
						/* check pre-lock tile */
						if (!IsTileFlat(neighbour_facing_tile)) continue;

						/* check slope tile */
						successor_tile = TileAddByDiagDir(neighbour_facing_tile, node_current->dir);
						if (!IsValidTile(successor_tile)) continue;

						DiagDirection slope_dir = GetInclinedSlopeDirection(GetTileSlope(successor_tile));
						if (!IsValidDiagDirection(slope_dir) || DiagDirToAxis(node_current->dir) != DiagDirToAxis(slope_dir)) continue;

						/* check post-lock tile */
						TileIndex post_tile = TileAddByDiagDir(successor_tile, node_current->dir);
						if (!IsValidTile(post_tile) || !IsTileFlat(post_tile)) continue;

						/* check if a lock already exists here */
						if (IsTileType(successor_tile, MP_WATER) && IsLock(successor_tile) ||
							// or if the tile types are valid for a new lock
							ShipPlannerValidLockTile(neighbour_facing_tile) && // check pre tile
							(IsTileType(successor_tile, MP_CLEAR) || IsTileType(successor_tile, MP_TREES) || IsCoastTile(successor_tile) || IsWaterTile(successor_tile)) && // TODO: check ownership
							ShipPlannerValidLockTile(post_tile)) { // check post tile
						/* hardcoded value, calculated by getting the fastest ship (hovercraft 112kph) and seeing fast it can travel vs travelling through a lock */
							tentative_cost = node_current->g_cost + 20;
						} else {
							continue;
						}
						break;
					}

					default:
						continue;
				}

				for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
					// if the neighbour type isn't a canal then we check the direction matches node_current
					// we also check we're not going back the way we just came
					if (dir == ReverseDiagDir(node_current->dir) || successor_tiletype != SPTT_WATER && dir != node_current->dir) {
						continue;
					}
					PathCost successor_current_cost = tentative_cost;

					// ships go faster on diagonals, i.e. "turning" is faster
					if (dir != node_current->dir) {
						successor_current_cost--;
					}

					ShipNode node_successor;
					// if node_successor is in the OPEN list
					if ((itr = OpenSet.find(HashShipNode(successor_tiletype, successor_tile, dir))) != OpenSet.end()) {
						node_successor = itr->second;
						// if g(node_successor) <= successor_current_cost continue (to line 20)
						if (node_successor->g_cost <= successor_current_cost) {
							continue;
						}
						node_successor->g_cost = successor_current_cost;
						node_successor->f_cost = node_successor->g_cost + ShipHeuristic(GetFacingTile(node_successor), ship_planner_end_tile);
						// we also have to move the node further to the front, uh hm
						OpenQueue.push(node_successor); // lazy insert, hopefully this works?
					// else if node_successor is in the CLOSED list
					} else if ((itr = ClosedSet.find(HashShipNode(successor_tiletype, successor_tile, dir))) != ClosedSet.end()) {
						node_successor = itr->second;
						// if g(node_successor) <= successor_current_cost continue (to line 20)
						if (node_successor->g_cost <= successor_current_cost) {
							continue;
						}
						// Move node_successor from the CLOSED list to the OPEN list
						node_successor->g_cost = successor_current_cost;
						node_successor->f_cost = node_successor->g_cost + ShipHeuristic(GetFacingTile(node_successor), ship_planner_end_tile);
						OpenQueue.push(node_successor);
						OpenSet.insert({ itr->first, itr->second });
						ClosedSet.erase(itr);
					} else {
						// Add node_successor to the OPEN list
						// Which means it has to be created
						node_successor = newShipNode(successor_tile);
						node_successor->type = successor_tiletype;
						node_successor->dir = dir;
						node_successor->g_cost = successor_current_cost;
						// Set h(node_successor) to be the heuristic distance to node_goal
						node_successor->f_cost = node_successor->g_cost + ShipHeuristic(GetFacingTile(node_successor), ship_planner_end_tile);
						// Add it to OpenQueue
						OpenQueue.push(node_successor);
						OpenSet.insert({ HashShipNode(node_successor), node_successor });
					}
					// Set the parent of node_successor to node_current
					node_successor->prev = node_current;
				}
			}
			// Add node_current to the CLOSED list
			ClosedSet.insert({ HashShipNode(node_current), node_current });
		}
		DEBUG(misc, 4, "%d nodes evaluated during tick.", steps);
		// if (node_current != node_goal) exit with error(the OPEN list is empty)
		// in our case, do nothing I guess
	}

	/* true if this is a basic flat water tile */
	static bool IsValidCanalTile(TileIndex tile)
	{
		return IsValidTile(tile) && IsTileFlat(tile) && (IsTileType(tile, MP_CLEAR) || IsTileType(tile, MP_TREES)
			|| IsWaterTile(tile)
			|| IsBuoyTile(tile));
	}


	static bool ShipPlannerValidLockTile(const TileIndex& tile)
	{
		return (IsTileType(tile, MP_CLEAR) || IsTileType(tile, MP_TREES) || IsWaterTile(tile) && !IsCanal(tile));
	}

	static TileIndex GetFacingTile(ShipNode node)
	{
		TileIndex neighbour_facing_tile = INVALID_TILE;
		switch (node->type) {
			case (SPTT_WATER):
				// the facing tile is just  adjacent to the canal tile
				return TileAddByDiagDir(node->tile, node->dir);

			case (SPTT_LOCK): {
				// the facing tile is at the end of the lock, i.e. two tiles from its centre
				TileIndex mid = TileAddByDiagDir(node->tile, node->dir);
				if (!IsValidTile(mid)) {
					return INVALID_TILE;
				} else {
					return TileAddByDiagDir(mid, node->dir);
				}
			}

			default:
				NOT_REACHED();
		}
	}

	// update the selected tiles for path highlighting
		// todo: change to unordered map (associate tile highlight with each tile)
		// also only change if the set actually changes
	void UpdatePathSet(ShipNode end = nullptr)
	{
		planner_tileindex_set::iterator it = PathHighlightSet.begin();
		while (it != PathHighlightSet.end()) {
			MarkTileDirtyByTile(*it);
			it = PathHighlightSet.erase(it);
		}
		while (end != nullptr) {
			PathHighlightSet.insert(end->tile);
			end = end->prev;
		}
	}

	bool IsPathTile(TileIndex tile)
	{
		return (PathHighlightSet.find(tile) != PathHighlightSet.end());
	}

	bool IsEndTileHighlight(const TileInfo *ti)
	{
		return ((ship_planner_start_tile == INVALID_TILE && ti->x == _thd.pos.x && ti->y == _thd.pos.y) ||
			ti->tile == ship_planner_start_tile ||
			ti->tile == ship_planner_end_tile);
	}

	bool DrawAsRed(TileIndex tile)
	{
		// XOR conditional expression, please don't ask how this works
		// TODO: Change IsTileFlat to a more precise validity check for candidate tiles (eg. checking tile type as well as slope)
		return !IsValidCanalTile(tile) || (ship_planner_start_tile == INVALID_TILE) != (ship_planner_end_tile == INVALID_TILE);
	}
};
