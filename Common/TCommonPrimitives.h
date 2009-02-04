// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef TCommonPrimitives_H
#define TCommonPrimitives_H

#include "TPrimitives.h"

BEGIN_NAMESPACE_HALYARD

//////////
/// Register all of the Common primitives with the gPrimitiveManager
/// object, so our scripts have some useful commands available.
///
extern void RegisterCommonPrimitives();

//////////
/// Update '_Graphic_X' and '_Graphic_Y' variables to hold
/// leftmost and bottommost edges of the most recently drawn graphic.
///
/// \param bounds  The graphic's rectangle, already offset from the origin.
///
extern void UpdateSpecialVariablesForGraphic(const TRect &bounds);

//////////
/// Update '_INCR_X' and '_INCR_Y' variables to hold
/// leftmost and bottommost edges of the most recently drawn graphic.
///
/// \param bottomLeft  The text's bottom left point, already offset from the
///                   origin.
///
extern void UpdateSpecialVariablesForText(const TPoint &bottomLeft);

END_NAMESPACE_HALYARD

#endif // TCommonPrimitives_H
