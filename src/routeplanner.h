/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

 /** @file routeplanner.h */

/* class lifespan is from mouseDown to mouseUp (maybe sooner) */
class RouteplannerShip {

public:
	/* constructor but also set the start tile */
	RouteplannerShip() {}
	RouteplannerShip(TileIndex start_tile);
	~RouteplannerShip();
	void AssignGoal(Point pt);
	void TryBuildFinalPath();
	void Reset();
	void DoPathSearch();
	static bool IsValidCanalTile(TileIndex tile);
	bool IsPathTile(TileIndex tile);
	bool IsEndTileHighlight(const TileInfo* ti);
	bool DrawAsRed(TileIndex tile);
};

/* global var for the single Routeplanner class,
 * shared use between dock_gui.cpp and viewport.cpp
 * TODO: see if this can be eliminated */
extern RouteplannerShip routeplanner_ship;
