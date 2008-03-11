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

#include "AppGraphics.h"

// Include the XPMs, making sure to turn off their 'static' declarations.
// The XPM files can be regenerated using the 'make-xpms' script in this
// directory, which should work correctly on a Linux system with NetPBM
// installed.
#if CONFIG_USE_XPMS
#	define static	
#	include "ic_application.xpm"
#	include "ic_listener.xpm"
#	include "ic_timecoder.xpm"
#	include "tb_borders.xpm"
#	include "tb_grid.xpm"
#	include "tb_reload.xpm"
#	include "tb_xy.xpm"
#endif // CONFIG_USE_XPMS
