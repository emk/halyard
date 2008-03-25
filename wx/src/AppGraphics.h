// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef AppGraphics_H
#define AppGraphics_H

#include "AppConfig.h"

// We get our graphics from XPM files on non-Windows systems.
#if CONFIG_USE_XPMS

typedef char *xpm_graphic_t[];

extern xpm_graphic_t ic_5L_xpm;
extern xpm_graphic_t ic_listener_xpm;
extern xpm_graphic_t ic_timecoder_xpm;
extern xpm_graphic_t tb_borders_xpm;
extern xpm_graphic_t tb_grid_xpm;
extern xpm_graphic_t tb_reload_xpm;
extern xpm_graphic_t tb_xy_xpm;

#endif // CONFIG_USE_XPMS

#endif // AppGraphics_H
