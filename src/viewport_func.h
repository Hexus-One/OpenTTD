/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file viewport_func.h Functions related to (drawing on) viewports. */

#ifndef VIEWPORT_FUNC_H
#define VIEWPORT_FUNC_H

#include "gfx_type.h"
#include "viewport_type.h"
#include "window_type.h"
#include "tile_map.h"
#include "station_type.h"

extern TileIndex ship_planner_start_tile;
extern TileIndex ship_planner_end_tile;

enum ShipPlannerTileType {
	SPTT_WATER, // includes both ocean and river tiles (TODO: special handling for half ocean tiles)
	SPTT_CANAL,
	SPTT_LOCK,
	SPTT_AQUEDUCT
};

typedef struct _ship_node {
	_ship_node *prev; // pointer to predecessor node
	TileIndex tile;
	uint16 f_cost; // estimated cost of an optimal path that includes this tile
	uint16 g_cost; // (known) cost from start node to this node
	DiagDirection dir; // only used for locks and aqueducts - since canals can attach in any direction
	ShipPlannerTileType type; // one of the above types
} *ShipNode;

inline ShipNode newShipNode(ShipNode prev = NULL, TileIndex tile = INVALID_TILE)
{
	ShipNode new_node = new _ship_node();
	new_node->prev = prev;
	new_node->tile = tile;
	new_node->f_cost = 0;
	new_node->g_cost = 0;
	return new_node;
}

static const int TILE_HEIGHT_STEP = 50; ///< One Z unit tile height difference is displayed as 50m.

void SetSelectionRed(bool);

void DeleteWindowViewport(Window *w);
void InitializeWindowViewport(Window *w, int x, int y, int width, int height, uint32 follow_flags, ZoomLevel zoom);
ViewPort *IsPtInWindowViewport(const Window *w, int x, int y);
Point TranslateXYToTileCoord(const ViewPort *vp, int x, int y, bool clamp_to_map = true);
Point GetTileBelowCursor();
void UpdateViewportPosition(Window *w);

void MarkAllViewportsDirty(int left, int top, int right, int bottom);

bool DoZoomInOutWindow(ZoomStateChange how, Window *w);
void ZoomInOrOutToCursorWindow(bool in, Window * w);
Point GetTileZoomCenterWindow(bool in, Window * w);
void FixTitleGameZoom();
void HandleZoomMessage(Window *w, const ViewPort *vp, byte widget_zoom_in, byte widget_zoom_out);

/**
 * Zoom a viewport as far as possible in the given direction.
 * @param how Zooming direction.
 * @param w   Window owning the viewport.
 * @pre \a how should not be #ZOOM_NONE.
 */
static inline void MaxZoomInOut(ZoomStateChange how, Window *w)
{
	while (DoZoomInOutWindow(how, w)) {};
}

void OffsetGroundSprite(int x, int y);

void DrawGroundSprite(SpriteID image, PaletteID pal, const SubSprite *sub = nullptr, int extra_offs_x = 0, int extra_offs_y = 0);
void DrawGroundSpriteAt(SpriteID image, PaletteID pal, int32 x, int32 y, int z, const SubSprite *sub = nullptr, int extra_offs_x = 0, int extra_offs_y = 0);
void AddSortableSpriteToDraw(SpriteID image, PaletteID pal, int x, int y, int w, int h, int dz, int z, bool transparent = false, int bb_offset_x = 0, int bb_offset_y = 0, int bb_offset_z = 0, const SubSprite *sub = nullptr);
void AddChildSpriteScreen(SpriteID image, PaletteID pal, int x, int y, bool transparent = false, const SubSprite *sub = nullptr, bool scale = true);
void ViewportAddString(const DrawPixelInfo *dpi, ZoomLevel small_from, const ViewportSign *sign, StringID string_normal, StringID string_small, StringID string_small_shadow, uint64 params_1, uint64 params_2 = 0, Colours colour = INVALID_COLOUR);


void StartSpriteCombine();
void EndSpriteCombine();

bool HandleViewportClicked(const ViewPort *vp, int x, int y);
void SetRedErrorSquare(TileIndex tile);
void SetTileSelectSize(int w, int h);
void SetTileSelectBigSize(int ox, int oy, int sx, int sy);

void ViewportDoDraw(const ViewPort *vp, int left, int top, int right, int bottom);

bool ScrollWindowToTile(TileIndex tile, Window *w, bool instant = false);
bool ScrollWindowTo(int x, int y, int z, Window *w, bool instant = false);

void RebuildViewportOverlay(Window *w);

bool ScrollMainWindowToTile(TileIndex tile, bool instant = false);
bool ScrollMainWindowTo(int x, int y, int z = -1, bool instant = false);

void UpdateAllVirtCoords();
void ClearAllCachedNames();

extern Point _tile_fract_coords;

void MarkTileDirtyByTile(TileIndex tile, int bridge_level_offset, int tile_height_override);

/**
 * Mark a tile given by its index dirty for repaint.
 * @param tile The tile to mark dirty.
 * @param bridge_level_offset Height of bridge on tile to also mark dirty. (Height level relative to north corner.)
 * @ingroup dirty
 */
static inline void MarkTileDirtyByTile(TileIndex tile, int bridge_level_offset = 0)
{
	MarkTileDirtyByTile(tile, bridge_level_offset, TileHeight(tile));
}

Point GetViewportStationMiddle(const ViewPort *vp, const Station *st);

struct Station;
struct Town;

void SetViewportCatchmentStation(const Station *st, bool sel);
void SetViewportCatchmentTown(const Town *t, bool sel);

#endif /* VIEWPORT_FUNC_H */
