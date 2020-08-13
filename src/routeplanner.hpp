/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

 /** @file routeplanner.hpp Abstract class for using the routeplanner tools. */

#include <chrono>
#include "debug.h"

class RoutePlanner {
protected:
	typedef uint16 PathCost;
	static const PathCost PATHCOST_MAX2 = UINT16_MAX;

public:
	RoutePlanner()
	{

	}

	~RoutePlanner()
	{

	}

	virtual bool IsInPathHighlightSet(TileIndex) = 0;

	void DoPathSearch()
	{
		/* check start/end is defined */
		/* check goal hasn't already been found */
		/* if goal tile has moved, reorder elements in OpenSet */

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
			// Add node_current to the CLOSED list
			ClosedSet.insert({ HashShipNode(node_current), node_current });
		}
		DEBUG(misc, 4, "%d nodes evaluated during tick.", steps);
		// if (node_current != node_goal) exit with error(the OPEN list is empty)
		// in our case, do nothing I guess
	}

protected:


};
