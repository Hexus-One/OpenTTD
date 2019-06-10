/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file dock_gui.cpp GUI to create amazing water objects. */

#include "stdafx.h"
#include "terraform_gui.h"
#include "window_gui.h"
#include "station_gui.h"
#include "command_func.h"
#include "water.h"
#include "window_func.h"
#include "vehicle_func.h"
#include "sound_func.h"
#include "viewport_func.h"
#include "gfx_func.h"
#include "company_func.h"
#include "slope_func.h"
#include "tilehighlight_func.h"
#include "company_base.h"
#include "hotkeys.h"
#include "gui.h"
#include "zoom_func.h"

#include "widgets/dock_widget.h"

#include "table/sprites.h"
#include "table/strings.h"

#include "safeguards.h"

#include <queue>
#include <unordered_map>

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

planner_tileindex_set PathHighlightSet;

ShipNodeQueue OpenQueue; // Open set of nodes known but yet to be expanded
ShipNodeSet OpenSet; // same as OpenQueue but used to check for duplicate nodes
ShipNodeSet ClosedSet; // Nodes that have been visited and expanded - uint32 is a hash of tile, direction (if relevant) and type

// create a key from a node, for use in ClosedSet
uint64 HashShipNode(const DiagDirection& dir, const ShipPlannerTileType& type, const TileIndex& tile)
{
	return ((uint64)((dir << 8) | type) << 32) | tile;
}

uint64 HashShipNode(const ShipNode& node)
{
	return HashShipNode(node->dir, node->type, node->tile);
}

TileIndex ship_planner_start_tile;
TileIndex ship_planner_end_tile;

// check whether a canal can be built on this tile
bool ShipPlannerValidCanalTile(const TileIndex& tile)
{
	return IsTileFlat(tile) && (IsTileType(tile, MP_CLEAR) || IsTileType(tile, MP_TREES) || IsWaterTile(tile));
}

static void ShowBuildDockStationPicker(Window *parent);
static void ShowBuildDocksDepotPicker(Window *parent);

static Axis _ship_depot_direction;

void CcBuildDocks(const CommandCost &result, TileIndex tile, uint32 p1, uint32 p2, uint32 cmd)
{
	if (result.Failed()) return;

	if (_settings_client.sound.confirm) SndPlayTileFx(SND_02_SPLAT_WATER, tile);
	if (!_settings_client.gui.persistent_buildingtools) ResetObjectToPlace();
}

void CcPlaySound_SPLAT_WATER(const CommandCost &result, TileIndex tile, uint32 p1, uint32 p2, uint32 cmd)
{
	if (result.Succeeded() && _settings_client.sound.confirm) SndPlayTileFx(SND_02_SPLAT_WATER, tile);
}


/**
 * Gets the other end of the aqueduct, if possible.
 * @param      tile_from The begin tile for the aqueduct.
 * @param[out] tile_to   The tile till where to show a selection for the aqueduct.
 * @return The other end of the aqueduct, or otherwise a tile in line with the aqueduct to cause the right error message.
 */
static TileIndex GetOtherAqueductEnd(TileIndex tile_from, TileIndex *tile_to = nullptr)
{
	int z;
	DiagDirection dir = GetInclinedSlopeDirection(GetTileSlope(tile_from, &z));

	/* If the direction isn't right, just return the next tile so the command
	 * complains about the wrong slope instead of the ends not matching up.
	 * Make sure the coordinate is always a valid tile within the map, so we
	 * don't go "off" the map. That would cause the wrong error message. */
	if (!IsValidDiagDirection(dir)) return TILE_ADDXY(tile_from, TileX(tile_from) > 2 ? -1 : 1, 0);

	/* Direction the aqueduct is built to. */
	TileIndexDiff offset = TileOffsByDiagDir(ReverseDiagDir(dir));
	/* The maximum length of the aqueduct. */
	int max_length = min(_settings_game.construction.max_bridge_length, DistanceFromEdgeDir(tile_from, ReverseDiagDir(dir)) - 1);

	TileIndex endtile = tile_from;
	for (int length = 0; IsValidTile(endtile) && TileX(endtile) != 0 && TileY(endtile) != 0; length++) {
		endtile = TILE_ADD(endtile, offset);

		if (length > max_length) break;

		if (GetTileMaxZ(endtile) > z) {
			if (tile_to != nullptr) *tile_to = endtile;
			break;
		}
	}

	return endtile;
}

/** Toolbar window for constructing water infrastructure. */
struct BuildDocksToolbarWindow : Window {
	DockToolbarWidgets last_clicked_widget; ///< Contains the last widget that has been clicked on this toolbar.

	BuildDocksToolbarWindow(WindowDesc *desc, WindowNumber window_number) : Window(desc)
	{
		this->last_clicked_widget = WID_DT_INVALID;
		this->InitNested(window_number);
		this->OnInvalidateData();

		ship_planner_start_tile = INVALID_TILE;
		ship_planner_end_tile = INVALID_TILE;

		if (_settings_client.gui.link_terraform_toolbar) ShowTerraformToolbar(this);
	}

	~BuildDocksToolbarWindow()
	{
		if (_game_mode == GM_NORMAL && this->IsWidgetLowered(WID_DT_STATION)) SetViewportCatchmentStation(nullptr, true);
		if (_settings_client.gui.link_terraform_toolbar) DeleteWindowById(WC_SCEN_LAND_GEN, 0, false);
	}

	/**
	 * Some data on this window has become invalid.
	 * @param data Information about the changed data.
	 * @param gui_scope Whether the call is done from GUI scope. You may not do everything when not in GUI scope. See #InvalidateWindowData() for details.
	 */
	void OnInvalidateData(int data = 0, bool gui_scope = true) override
	{
		if (!gui_scope) return;

		bool can_build = CanBuildVehicleInfrastructure(VEH_SHIP);
		this->SetWidgetsDisabledState(!can_build,
			WID_DT_DEPOT,
			WID_DT_STATION,
			WID_DT_BUOY,
			WIDGET_LIST_END);
		if (!can_build) {
			DeleteWindowById(WC_BUILD_STATION, TRANSPORT_WATER);
			DeleteWindowById(WC_BUILD_DEPOT, TRANSPORT_WATER);
		}
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		if (widget == WID_DT_SHIP_PLANNER) {
			// a bit of funky maths to shrink the cargo-flow icon
			ZoomLevel temp_zoom;
			switch (_gui_zoom) {
			case ZOOM_LVL_NORMAL:
				temp_zoom = ZOOM_LVL_OUT_2X;
				break;
			case ZOOM_LVL_OUT_2X:
				temp_zoom = ZOOM_LVL_OUT_4X;
				break;
			case ZOOM_LVL_OUT_4X:
				temp_zoom = ZOOM_LVL_OUT_8X;
				break;
			}
			Dimension d = GetSpriteSize(SPR_IMG_CARGOFLOW, (Point *)0, temp_zoom);
			uint offset = this->IsWidgetLowered(WID_DT_SHIP_PLANNER) ? 1 : 0;
			DrawSprite(SPR_IMG_CARGOFLOW, PAL_NONE, (r.left + r.right - d.width) / 2 + offset, (r.top + r.bottom - d.height) / 2 + offset, (const SubSprite *)0, temp_zoom);
		}
	}

	void OnClick(Point pt, int widget, int click_count) override
	{
		switch (widget) {
			case WID_DT_CANAL: // Build canal button
				HandlePlacePushButton(this, WID_DT_CANAL, SPR_CURSOR_CANAL, HT_RECT);
				break;

			case WID_DT_LOCK: // Build lock button
				HandlePlacePushButton(this, WID_DT_LOCK, SPR_CURSOR_LOCK, HT_SPECIAL);
				break;

			case WID_DT_DEMOLISH: // Demolish aka dynamite button
				HandlePlacePushButton(this, WID_DT_DEMOLISH, ANIMCURSOR_DEMOLISH, HT_RECT | HT_DIAGONAL);
				break;

			case WID_DT_DEPOT: // Build depot button
				if (!CanBuildVehicleInfrastructure(VEH_SHIP)) return;
				if (HandlePlacePushButton(this, WID_DT_DEPOT, SPR_CURSOR_SHIP_DEPOT, HT_RECT)) ShowBuildDocksDepotPicker(this);
				break;

			case WID_DT_STATION: // Build station button
				if (!CanBuildVehicleInfrastructure(VEH_SHIP)) return;
				if (HandlePlacePushButton(this, WID_DT_STATION, SPR_CURSOR_DOCK, HT_SPECIAL)) ShowBuildDockStationPicker(this);
				break;

			case WID_DT_BUOY: // Build buoy button
				if (!CanBuildVehicleInfrastructure(VEH_SHIP)) return;
				HandlePlacePushButton(this, WID_DT_BUOY, SPR_CURSOR_BUOY, HT_RECT);
				break;

			case WID_DT_RIVER: // Build river button (in scenario editor)
				if (_game_mode != GM_EDITOR) return;
				HandlePlacePushButton(this, WID_DT_RIVER, SPR_CURSOR_RIVER, HT_RECT);
				break;

			case WID_DT_BUILD_AQUEDUCT: // Build aqueduct button
				HandlePlacePushButton(this, WID_DT_BUILD_AQUEDUCT, SPR_CURSOR_AQUEDUCT, HT_SPECIAL);
				break;

			case WID_DT_SHIP_PLANNER: // Ship planner button
				HandlePlacePushButton(this, WID_DT_SHIP_PLANNER, SPR_CURSOR_CANAL, HT_START_END);
				break;

			default: return;
		}
		this->last_clicked_widget = (DockToolbarWidgets)widget;
	}

	void OnPlaceObject(Point pt, TileIndex tile) override
	{
		switch (this->last_clicked_widget) {
			case WID_DT_CANAL: // Build canal button
				VpStartPlaceSizing(tile, (_game_mode == GM_EDITOR) ? VPM_X_AND_Y : VPM_X_OR_Y, DDSP_CREATE_WATER);
				break;

			case WID_DT_LOCK: // Build lock button
				DoCommandP(tile, 0, 0, CMD_BUILD_LOCK | CMD_MSG(STR_ERROR_CAN_T_BUILD_LOCKS), CcBuildDocks);
				break;

			case WID_DT_DEMOLISH: // Demolish aka dynamite button
				PlaceProc_DemolishArea(tile);
				break;

			case WID_DT_DEPOT: // Build depot button
				DoCommandP(tile, _ship_depot_direction, 0, CMD_BUILD_SHIP_DEPOT | CMD_MSG(STR_ERROR_CAN_T_BUILD_SHIP_DEPOT), CcBuildDocks);
				break;

			case WID_DT_STATION: { // Build station button
				uint32 p2 = (uint32)INVALID_STATION << 16; // no station to join

				/* tile is always the land tile, so need to evaluate _thd.pos */
				CommandContainer cmdcont = { tile, _ctrl_pressed, p2, CMD_BUILD_DOCK | CMD_MSG(STR_ERROR_CAN_T_BUILD_DOCK_HERE), CcBuildDocks, "" };

				/* Determine the watery part of the dock. */
				DiagDirection dir = GetInclinedSlopeDirection(GetTileSlope(tile));
				TileIndex tile_to = (dir != INVALID_DIAGDIR ? TileAddByDiagDir(tile, ReverseDiagDir(dir)) : tile);

				ShowSelectStationIfNeeded(cmdcont, TileArea(tile, tile_to));
				break;
			}

			case WID_DT_BUOY: // Build buoy button
				DoCommandP(tile, 0, 0, CMD_BUILD_BUOY | CMD_MSG(STR_ERROR_CAN_T_POSITION_BUOY_HERE), CcBuildDocks);
				break;

			case WID_DT_RIVER: // Build river button (in scenario editor)
				VpStartPlaceSizing(tile, VPM_X_AND_Y, DDSP_CREATE_RIVER);
				break;

			case WID_DT_BUILD_AQUEDUCT: // Build aqueduct button
				DoCommandP(tile, GetOtherAqueductEnd(tile), TRANSPORT_WATER << 15, CMD_BUILD_BRIDGE | CMD_MSG(STR_ERROR_CAN_T_BUILD_AQUEDUCT_HERE), CcBuildBridge);
				break;

			case WID_DT_SHIP_PLANNER: // Ship planner button
				// check if this is a valid tile
				if (ShipPlannerValidCanalTile(tile)) {
					ship_planner_start_tile = tile;
					// create the first node :O
					ShipNode first_node = newShipNode(tile);
					// Put node_start in the OPEN list with f(node_start) = h(node_start) (initialization)
					OpenQueue.push(first_node);
					OpenSet.insert({ HashShipNode(first_node), first_node });

					VpStartPlaceSizing(tile, VPM_X_AND_Y, DDSP_SHIP_PLANNER);
				}
				break;

			default: NOT_REACHED();
		}
	}

	void OnPlaceDrag(ViewportPlaceMethod select_method, ViewportDragDropSelectionProcess select_proc, Point pt) override
	{
		VpSelectTilesWithMethod(pt.x, pt.y, select_method);
		// reassign the goal tile for ship planner
		if (this->IsWidgetLowered(WID_DT_SHIP_PLANNER)) {
			if (pt.x != -1) {
				int gx = (pt.x & ~TILE_UNIT_MASK) >> 4;
				int gy = (pt.y & ~TILE_UNIT_MASK) >> 4;
				ship_planner_end_tile = TileXY(gx, gy);
				if (!ShipPlannerValidCanalTile(ship_planner_end_tile)) {
					ship_planner_end_tile = INVALID_TILE;
				}
			}
		}
	}

	void OnPlaceMouseUp(ViewportPlaceMethod select_method, ViewportDragDropSelectionProcess select_proc, Point pt, TileIndex start_tile, TileIndex end_tile) override
	{
		if (pt.x != -1) {
			switch (select_proc) {
				case DDSP_DEMOLISH_AREA:
					GUIPlaceProcDragXY(select_proc, start_tile, end_tile);
					break;
				case DDSP_CREATE_WATER:
					DoCommandP(end_tile, start_tile, (_game_mode == GM_EDITOR && _ctrl_pressed) ? WATER_CLASS_SEA : WATER_CLASS_CANAL, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_BUILD_CANALS), CcPlaySound_SPLAT_WATER);
					break;
				case DDSP_CREATE_RIVER:
					DoCommandP(end_tile, start_tile, WATER_CLASS_RIVER, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_PLACE_RIVERS), CcPlaySound_SPLAT_WATER);
					break;
				case DDSP_SHIP_PLANNER: {
					// sometimes the drag function doesn't execute between mouseDown and Up - usually only when the user clicks too quickly.
					// if they click too quickly, we assume they just clicked on a single tile, so behave just like the regular canal tool
					int gx = (pt.x & ~TILE_UNIT_MASK) >> 4;
					int gy = (pt.y & ~TILE_UNIT_MASK) >> 4;
					if (TileXY(gx, gy) == ship_planner_start_tile) {
						DoCommandP(end_tile, start_tile, WATER_CLASS_CANAL, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_BUILD_CANALS), CcPlaySound_SPLAT_WATER);
					} else {
						// build the path if it exists
						ShipNodeSet::iterator itr;
						if ((itr = ClosedSet.find(HashShipNode(DIAGDIR_BEGIN, SPTT_CANAL, ship_planner_end_tile))) != ClosedSet.end()) {
							for (ShipNode temp = itr->second; temp != NULL; temp = temp->prev) {
								DoCommandP(temp->tile, temp->tile, WATER_CLASS_CANAL, CMD_BUILD_CANAL | CMD_MSG(STR_ERROR_CAN_T_BUILD_CANALS), CcPlaySound_SPLAT_WATER);
							}
						}
					}
					// and then clean up afterwards
					OpenQueue = ShipNodeQueue();
					OpenSet = ShipNodeSet();
					ClosedSet = ShipNodeSet();

					ship_planner_start_tile = INVALID_TILE;
					ship_planner_end_tile = INVALID_TILE;
					break;
				}

				default: break;
			}
		}
	}

	void OnPlaceObjectAbort() override
	{
		if (_game_mode != GM_EDITOR && this->IsWidgetLowered(WID_DT_STATION)) SetViewportCatchmentStation(nullptr, true);

		this->RaiseButtons();

		DeleteWindowById(WC_BUILD_STATION, TRANSPORT_WATER);
		DeleteWindowById(WC_BUILD_DEPOT, TRANSPORT_WATER);
		DeleteWindowById(WC_SELECT_STATION, 0);
		DeleteWindowByClass(WC_BUILD_BRIDGE);
	}

	void OnPlacePresize(Point pt, TileIndex tile_from) override
	{
		TileIndex tile_to = tile_from;

		if (this->last_clicked_widget == WID_DT_BUILD_AQUEDUCT) {
			GetOtherAqueductEnd(tile_from, &tile_to);
		} else {
			DiagDirection dir = GetInclinedSlopeDirection(GetTileSlope(tile_from));
			if (IsValidDiagDirection(dir)) {
				/* Locks and docks always select the tile "down" the slope. */
				tile_to = TileAddByDiagDir(tile_from, ReverseDiagDir(dir));
				/* Locks also select the tile "up" the slope. */
				if (this->last_clicked_widget == WID_DT_LOCK) tile_from = TileAddByDiagDir(tile_from, dir);
			}
		}

		VpSetPresizeRange(tile_from, tile_to);
	}

	// update the selected tiles for path highlighting
	// todo: change to unordered map (associate tile highlight with each tile)
	// also only change if the set actually changes
	// also: implement markDirty for the tiles
	void UpdatePathSet(ShipNode end = NULL)
	{
		PathHighlightSet = planner_tileindex_set();
		while (end != NULL) {
			PathHighlightSet.insert(end->tile);
			end = end->prev;
		}
	}

	void OnRealtimeTick(uint delta_ms) override
	{
		// exit if both tiles aren't defined,
		if (ship_planner_start_tile == INVALID_TILE || ship_planner_end_tile == INVALID_TILE) {
			UpdatePathSet();
			return;
		}
		// or the goal has already been found
		ShipNodeSet::iterator itr;
		if ((itr = ClosedSet.find(HashShipNode(DIAGDIR_BEGIN, SPTT_CANAL, ship_planner_end_tile))) != ClosedSet.end()) {
			UpdatePathSet(itr->second);
			return;
		}
		// Do the A* thingo
		// while the OPEN list is not empty
		// TODO: Add time limit so as not to lag the whole thing
		while (!OpenQueue.empty()) {
			// Take from the open list the node node_current with the lowest
				// f(node_current) = g(node_current) + h(node_current)
			ShipNode node_current = OpenQueue.top();
			OpenQueue.pop();
			OpenSet.erase(HashShipNode(node_current));
			// if node_current is node_goal we have found the solution; break
			if (node_current->tile == ship_planner_end_tile && node_current->type == SPTT_CANAL) {
				ClosedSet.insert({ HashShipNode(node_current), node_current });
				UpdatePathSet(node_current);
				break;
			}
			// Generate each state node_successor that come after node_current
			// For each direction, try each tile type
			for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
				// for now we only deal with canals
				// check it is valid for placement
				TileIndex successor_tile = TileAddByDiagDir(node_current->tile, dir);
				if (!ShipPlannerValidCanalTile(successor_tile)) {
					continue;
				}
				// Set successor_current_cost = g(node_current) + w(node_current, node_successor)
				PathCost successor_current_cost = node_current->g_cost + 1; // magic number oops
				// if node_successor is in the OPEN list
				ShipNode node_successor;
				if ((itr = OpenSet.find(HashShipNode(DIAGDIR_BEGIN, SPTT_CANAL, successor_tile))) != OpenSet.end()) {
					node_successor = itr->second;
					// if g(node_successor) <= successor_current_cost continue (to line 20)
					if (node_successor->g_cost <= successor_current_cost) {
						continue;
					}
				// else if node_successor is in the CLOSED list
				} else if ((itr = ClosedSet.find(HashShipNode(DIAGDIR_BEGIN, SPTT_CANAL, successor_tile))) != ClosedSet.end()) {
					node_successor = itr->second;
					// if g(node_successor) <= successor_current_cost continue (to line 20)
					if (node_successor->g_cost <= successor_current_cost) {
						continue;
					}
					// Move node_successor from the CLOSED list to the OPEN list
					OpenQueue.push(node_successor);
					OpenSet.insert({ itr->first, itr->second });
					ClosedSet.erase(itr);
				} else {
					// Add node_successor to the OPEN list
					// Which means it has to be created
					node_successor = newShipNode(successor_tile);
					node_successor->g_cost = successor_current_cost;
					node_successor->type = SPTT_CANAL;
					// Set h(node_successor) to be the heuristic distance to node_goal
					node_successor->f_cost = node_successor->g_cost + DistanceMax(successor_tile, ship_planner_end_tile);
					// Add it to OpenQueue
					OpenQueue.push(node_successor);
					OpenSet.insert({ HashShipNode(node_successor), node_successor });
				}
				// Set g(node_successor) = successor_current_cost
				node_successor->g_cost = successor_current_cost;
				node_successor->f_cost = node_successor->g_cost + DistanceMax(successor_tile, ship_planner_end_tile);
				// Set the parent of node_successor to node_current
				node_successor->prev = node_current;
			}
			// Add node_current to the CLOSED list
			ClosedSet.insert({ HashShipNode(node_current), node_current });
		}
		// if (node_current != node_goal) exit with error(the OPEN list is empty)
		// in our case, do nothing I guess
	}

	static HotkeyList hotkeys;
};

/**
 * Handler for global hotkeys of the BuildDocksToolbarWindow.
 * @param hotkey Hotkey
 * @return ES_HANDLED if hotkey was accepted.
 */
static EventState DockToolbarGlobalHotkeys(int hotkey)
{
	if (_game_mode != GM_NORMAL) return ES_NOT_HANDLED;
	Window *w = ShowBuildDocksToolbar();
	if (w == nullptr) return ES_NOT_HANDLED;
	return w->OnHotkey(hotkey);
}

const uint16 _dockstoolbar_aqueduct_keys[] = {'B', '8', 0};

static Hotkey dockstoolbar_hotkeys[] = {
	Hotkey('1', "canal", WID_DT_CANAL),
	Hotkey('2', "lock", WID_DT_LOCK),
	Hotkey('3', "demolish", WID_DT_DEMOLISH),
	Hotkey('4', "depot", WID_DT_DEPOT),
	Hotkey('5', "dock", WID_DT_STATION),
	Hotkey('6', "buoy", WID_DT_BUOY),
	Hotkey('7', "river", WID_DT_RIVER),
	Hotkey(_dockstoolbar_aqueduct_keys, "aqueduct", WID_DT_BUILD_AQUEDUCT),
	Hotkey('9', "ship_planner", WID_DT_SHIP_PLANNER),
	HOTKEY_LIST_END
};
HotkeyList BuildDocksToolbarWindow::hotkeys("dockstoolbar", dockstoolbar_hotkeys, DockToolbarGlobalHotkeys);

/**
 * Nested widget parts of docks toolbar, game version.
 * Position of #WID_DT_RIVER widget has changed.
 */
static const NWidgetPart _nested_build_docks_toolbar_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN), SetDataTip(STR_WATERWAYS_TOOLBAR_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_STICKYBOX, COLOUR_DARK_GREEN),
	EndContainer(),
	NWidget(NWID_HORIZONTAL_LTR),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_CANAL), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_CANAL, STR_WATERWAYS_TOOLBAR_BUILD_CANALS_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_LOCK), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_LOCK, STR_WATERWAYS_TOOLBAR_BUILD_LOCKS_TOOLTIP),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN), SetMinimalSize(5, 22), SetFill(1, 1), EndContainer(),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_DEMOLISH), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_DYNAMITE, STR_TOOLTIP_DEMOLISH_BUILDINGS_ETC),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_DEPOT), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_SHIP_DEPOT, STR_WATERWAYS_TOOLBAR_BUILD_DEPOT_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_STATION), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_SHIP_DOCK, STR_WATERWAYS_TOOLBAR_BUILD_DOCK_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_BUOY), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUOY, STR_WATERWAYS_TOOLBAR_BUOY_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_BUILD_AQUEDUCT), SetMinimalSize(23, 22), SetFill(0, 1), SetDataTip(SPR_IMG_AQUEDUCT, STR_WATERWAYS_TOOLBAR_BUILD_AQUEDUCT_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_SHIP_PLANNER), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_CANAL, STR_WATERWAYS_TOOLBAR_SHIP_PLANNER_TOOLTIP),
	EndContainer(),
};

static WindowDesc _build_docks_toolbar_desc(
	WDP_ALIGN_TOOLBAR, "toolbar_water", 0, 0,
	WC_BUILD_TOOLBAR, WC_NONE,
	WDF_CONSTRUCTION,
	_nested_build_docks_toolbar_widgets, lengthof(_nested_build_docks_toolbar_widgets),
	&BuildDocksToolbarWindow::hotkeys
);

/**
 * Open the build water toolbar window
 *
 * If the terraform toolbar is linked to the toolbar, that window is also opened.
 *
 * @return newly opened water toolbar, or nullptr if the toolbar could not be opened.
 */
Window *ShowBuildDocksToolbar()
{
	if (!Company::IsValidID(_local_company)) return nullptr;

	DeleteWindowByClass(WC_BUILD_TOOLBAR);
	return AllocateWindowDescFront<BuildDocksToolbarWindow>(&_build_docks_toolbar_desc, TRANSPORT_WATER);
}

/**
 * Nested widget parts of docks toolbar, scenario editor version.
 * Positions of #WID_DT_DEPOT, #WID_DT_STATION, and #WID_DT_BUOY widgets have changed.
 */
static const NWidgetPart _nested_build_docks_scen_toolbar_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN), SetDataTip(STR_WATERWAYS_TOOLBAR_CAPTION_SE, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
		NWidget(WWT_STICKYBOX, COLOUR_DARK_GREEN),
	EndContainer(),
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_CANAL), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_CANAL, STR_WATERWAYS_TOOLBAR_CREATE_LAKE_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_LOCK), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_LOCK, STR_WATERWAYS_TOOLBAR_BUILD_LOCKS_TOOLTIP),
		NWidget(WWT_PANEL, COLOUR_DARK_GREEN), SetMinimalSize(5, 22), SetFill(1, 1), EndContainer(),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_DEMOLISH), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_DYNAMITE, STR_TOOLTIP_DEMOLISH_BUILDINGS_ETC),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_RIVER), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_BUILD_RIVER, STR_WATERWAYS_TOOLBAR_CREATE_RIVER_TOOLTIP),
		NWidget(WWT_IMGBTN, COLOUR_DARK_GREEN, WID_DT_BUILD_AQUEDUCT), SetMinimalSize(22, 22), SetFill(0, 1), SetDataTip(SPR_IMG_AQUEDUCT, STR_WATERWAYS_TOOLBAR_BUILD_AQUEDUCT_TOOLTIP),
	EndContainer(),
};

/** Window definition for the build docks in scenario editor window. */
static WindowDesc _build_docks_scen_toolbar_desc(
	WDP_AUTO, "toolbar_water_scen", 0, 0,
	WC_SCEN_BUILD_TOOLBAR, WC_NONE,
	WDF_CONSTRUCTION,
	_nested_build_docks_scen_toolbar_widgets, lengthof(_nested_build_docks_scen_toolbar_widgets)
);

/**
 * Open the build water toolbar window for the scenario editor.
 *
 * @return newly opened water toolbar, or nullptr if the toolbar could not be opened.
 */
Window *ShowBuildDocksScenToolbar()
{
	return AllocateWindowDescFront<BuildDocksToolbarWindow>(&_build_docks_scen_toolbar_desc, TRANSPORT_WATER);
}

/** Widget numbers of the build-dock GUI. */
enum BuildDockStationWidgets {
	BDSW_BACKGROUND, ///< Background panel.
	BDSW_LT_OFF,     ///< 'Off' button of coverage high light.
	BDSW_LT_ON,      ///< 'On' button of coverage high light.
	BDSW_INFO,       ///< 'Coverage highlight' label.
};

struct BuildDocksStationWindow : public PickerWindowBase {
public:
	BuildDocksStationWindow(WindowDesc *desc, Window *parent) : PickerWindowBase(desc, parent)
	{
		this->InitNested(TRANSPORT_WATER);
		this->LowerWidget(_settings_client.gui.station_show_coverage + BDSW_LT_OFF);
	}

	virtual ~BuildDocksStationWindow()
	{
		DeleteWindowById(WC_SELECT_STATION, 0);
	}

	void OnPaint() override
	{
		int rad = (_settings_game.station.modified_catchment) ? CA_DOCK : CA_UNMODIFIED;

		this->DrawWidgets();

		if (_settings_client.gui.station_show_coverage) {
			SetTileSelectBigSize(-rad, -rad, 2 * rad, 2 * rad);
		} else {
			SetTileSelectSize(1, 1);
		}

		/* strings such as 'Size' and 'Coverage Area' */
		int top = this->GetWidget<NWidgetBase>(BDSW_LT_OFF)->pos_y + this->GetWidget<NWidgetBase>(BDSW_LT_OFF)->current_y + WD_PAR_VSEP_NORMAL;
		NWidgetBase *back_nwi = this->GetWidget<NWidgetBase>(BDSW_BACKGROUND);
		int right  = back_nwi->pos_x + back_nwi->current_x;
		int bottom = back_nwi->pos_y + back_nwi->current_y;
		top = DrawStationCoverageAreaText(back_nwi->pos_x + WD_FRAMERECT_LEFT, right - WD_FRAMERECT_RIGHT, top, SCT_ALL, rad, false) + WD_PAR_VSEP_NORMAL;
		top = DrawStationCoverageAreaText(back_nwi->pos_x + WD_FRAMERECT_LEFT, right - WD_FRAMERECT_RIGHT, top, SCT_ALL, rad, true) + WD_PAR_VSEP_NORMAL;
		/* Resize background if the window is too small.
		 * Never make the window smaller to avoid oscillating if the size change affects the acceptance.
		 * (This is the case, if making the window bigger moves the mouse into the window.) */
		if (top > bottom) {
			ResizeWindow(this, 0, top - bottom, false);
		}
	}

	void OnClick(Point pt, int widget, int click_count) override
	{
		switch (widget) {
			case BDSW_LT_OFF:
			case BDSW_LT_ON:
				this->RaiseWidget(_settings_client.gui.station_show_coverage + BDSW_LT_OFF);
				_settings_client.gui.station_show_coverage = (widget != BDSW_LT_OFF);
				this->LowerWidget(_settings_client.gui.station_show_coverage + BDSW_LT_OFF);
				if (_settings_client.sound.click_beep) SndPlayFx(SND_15_BEEP);
				this->SetDirty();
				break;
		}
	}

	void OnRealtimeTick(uint delta_ms) override
	{
		CheckRedrawStationCoverage(this);
	}
};

/** Nested widget parts of a build dock station window. */
static const NWidgetPart _nested_build_dock_station_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN), SetDataTip(STR_STATION_BUILD_DOCK_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_DARK_GREEN, BDSW_BACKGROUND),
		NWidget(NWID_SPACER), SetMinimalSize(0, 3),
		NWidget(WWT_LABEL, COLOUR_DARK_GREEN, BDSW_INFO), SetMinimalSize(148, 14), SetDataTip(STR_STATION_BUILD_COVERAGE_AREA_TITLE, STR_NULL),
		NWidget(NWID_HORIZONTAL), SetPIP(14, 0, 14),
			NWidget(WWT_TEXTBTN, COLOUR_GREY, BDSW_LT_OFF), SetMinimalSize(40, 12), SetFill(1, 0), SetDataTip(STR_STATION_BUILD_COVERAGE_OFF, STR_STATION_BUILD_COVERAGE_AREA_OFF_TOOLTIP),
			NWidget(WWT_TEXTBTN, COLOUR_GREY, BDSW_LT_ON), SetMinimalSize(40, 12), SetFill(1, 0), SetDataTip(STR_STATION_BUILD_COVERAGE_ON, STR_STATION_BUILD_COVERAGE_AREA_ON_TOOLTIP),
		EndContainer(),
		NWidget(NWID_SPACER), SetMinimalSize(0, 20), SetResize(0, 1),
	EndContainer(),
};

static WindowDesc _build_dock_station_desc(
	WDP_AUTO, nullptr, 0, 0,
	WC_BUILD_STATION, WC_BUILD_TOOLBAR,
	WDF_CONSTRUCTION,
	_nested_build_dock_station_widgets, lengthof(_nested_build_dock_station_widgets)
);

static void ShowBuildDockStationPicker(Window *parent)
{
	new BuildDocksStationWindow(&_build_dock_station_desc, parent);
}

struct BuildDocksDepotWindow : public PickerWindowBase {
private:
	static void UpdateDocksDirection()
	{
		if (_ship_depot_direction != AXIS_X) {
			SetTileSelectSize(1, 2);
		} else {
			SetTileSelectSize(2, 1);
		}
	}

public:
	BuildDocksDepotWindow(WindowDesc *desc, Window *parent) : PickerWindowBase(desc, parent)
	{
		this->InitNested(TRANSPORT_WATER);
		this->LowerWidget(_ship_depot_direction + WID_BDD_X);
		UpdateDocksDirection();
	}

	void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize) override
	{
		switch (widget) {
			case WID_BDD_X:
			case WID_BDD_Y:
				size->width  = ScaleGUITrad(96) + 2;
				size->height = ScaleGUITrad(64) + 2;
				break;
		}
	}

	void OnPaint() override
	{
		this->DrawWidgets();

		int x1 = ScaleGUITrad(63) + 1;
		int x2 = ScaleGUITrad(31) + 1;
		int y1 = ScaleGUITrad(17) + 1;
		int y2 = ScaleGUITrad(33) + 1;

		DrawShipDepotSprite(this->GetWidget<NWidgetBase>(WID_BDD_X)->pos_x + x1, this->GetWidget<NWidgetBase>(WID_BDD_X)->pos_y + y1, AXIS_X, DEPOT_PART_NORTH);
		DrawShipDepotSprite(this->GetWidget<NWidgetBase>(WID_BDD_X)->pos_x + x2, this->GetWidget<NWidgetBase>(WID_BDD_X)->pos_y + y2, AXIS_X, DEPOT_PART_SOUTH);
		DrawShipDepotSprite(this->GetWidget<NWidgetBase>(WID_BDD_Y)->pos_x + x2, this->GetWidget<NWidgetBase>(WID_BDD_Y)->pos_y + y1, AXIS_Y, DEPOT_PART_NORTH);
		DrawShipDepotSprite(this->GetWidget<NWidgetBase>(WID_BDD_Y)->pos_x + x1, this->GetWidget<NWidgetBase>(WID_BDD_Y)->pos_y + y2, AXIS_Y, DEPOT_PART_SOUTH);
	}

	void OnClick(Point pt, int widget, int click_count) override
	{
		switch (widget) {
			case WID_BDD_X:
			case WID_BDD_Y:
				this->RaiseWidget(_ship_depot_direction + WID_BDD_X);
				_ship_depot_direction = (widget == WID_BDD_X ? AXIS_X : AXIS_Y);
				this->LowerWidget(_ship_depot_direction + WID_BDD_X);
				if (_settings_client.sound.click_beep) SndPlayFx(SND_15_BEEP);
				UpdateDocksDirection();
				this->SetDirty();
				break;
		}
	}
};

static const NWidgetPart _nested_build_docks_depot_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_DARK_GREEN),
		NWidget(WWT_CAPTION, COLOUR_DARK_GREEN), SetDataTip(STR_DEPOT_BUILD_SHIP_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_DARK_GREEN, WID_BDD_BACKGROUND),
		NWidget(NWID_SPACER), SetMinimalSize(0, 3),
		NWidget(NWID_HORIZONTAL_LTR),
			NWidget(NWID_SPACER), SetMinimalSize(3, 0),
			NWidget(WWT_PANEL, COLOUR_GREY, WID_BDD_X), SetMinimalSize(98, 66), SetDataTip(0x0, STR_DEPOT_BUILD_SHIP_ORIENTATION_TOOLTIP),
			EndContainer(),
			NWidget(NWID_SPACER), SetMinimalSize(2, 0),
			NWidget(WWT_PANEL, COLOUR_GREY, WID_BDD_Y), SetMinimalSize(98, 66), SetDataTip(0x0, STR_DEPOT_BUILD_SHIP_ORIENTATION_TOOLTIP),
			EndContainer(),
			NWidget(NWID_SPACER), SetMinimalSize(3, 0),
		EndContainer(),
		NWidget(NWID_SPACER), SetMinimalSize(0, 3),
	EndContainer(),
};

static WindowDesc _build_docks_depot_desc(
	WDP_AUTO, nullptr, 0, 0,
	WC_BUILD_DEPOT, WC_BUILD_TOOLBAR,
	WDF_CONSTRUCTION,
	_nested_build_docks_depot_widgets, lengthof(_nested_build_docks_depot_widgets)
);


static void ShowBuildDocksDepotPicker(Window *parent)
{
	new BuildDocksDepotWindow(&_build_docks_depot_desc, parent);
}


void InitializeDockGui()
{
	_ship_depot_direction = AXIS_X;
}
