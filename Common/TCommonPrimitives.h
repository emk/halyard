// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

BEGIN_NAMESPACE_FIVEL

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

//////////
///  The 5L graphics primitives all draw relative to an "origin", which
///  is normally in the upper left corner of the screen, but which can
///  be moved to another location.  The origin is typically reset at the
///  begining of each card, but this is up to the interpreter.
///
///  This code was refactored out of the Card class and made global.
///
class Origin
{
	TPoint mOrigin;

public:
	//////////
	/// Adjust the global rect to local coordinates based on origin.
	///
	/// \param r  (in/out) The rectangle to adjust.
	///
	void AdjustRect(TRect *r);

	//////////
	/// Adjust the global point to local coordinates based on origin
	///
	/// \param pt  (in/out) The point to adjust.
	///
	void AdjustPoint(TPoint *pt);

	//////////
	/// Adjust the local point to global coordinates based on origin
	///
	/// \param pt  (in/out) The point to unadjust.
	///
	void UnadjustPoint(TPoint *pt);

	//////////
	/// Gets the card's local coordinate system.
	///
	/// \return  the current origin
	///
	TPoint GetOrigin();

	//////////
	/// Sets the card's local coordinate system.
	///
	/// \param loc  set the origin to this location
	///
	void SetOrigin(TPoint &loc);

	//////////
	/// Sets the card's local coordinate system.
	///
	/// \param inX  X-coordinate
	/// \param inY  Y-coordinate
	///
	void SetOrigin(int16 inX, int16 inY);

	//////////
	/// Offsets the card's local coordinate system by the amount given.
	///
	/// \param delta  amount to offset the origin
	///
	void OffsetOrigin(TPoint &delta);
};

//////////
/// Our global origin object.
///
extern Origin gOrigin;

END_NAMESPACE_FIVEL

#endif // TCommonPrimitives_H
