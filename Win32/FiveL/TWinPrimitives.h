// -*- Mode: C++; tab-width: 4; -*-

#ifndef TWinPrimitives_H
#define TWinPrimitives_H

BEGIN_NAMESPACE_FIVEL

//////////
// Register all of the Windows primitives with the gPrimitiveManager
// object, so our scripts have some useful commands available.
//
extern void RegisterWindowsPrimitives();

//////////
//  The 5L graphics primitives all draw relative to an "origin", which
//  is normally in the upper left corner of the screen, but which can
//  be moved to another location.  The origin is typically reset at the
//  begining of each card, but this is up to the interpreter.
//
//  This code was refactored out of the Card class and made global.
//
class Origin
{
	TPoint mOrigin;

public:
	//////////
	// Adjust the global rect to local coordinates based on origin.
	//
	// [in/out] r - The rectangle to adjust.
	//
	void AdjustRect(TRect *r);

	//////////
	// Adjust the global point to local coordinates based on origin
	//
	// [in/out] pt - The point to adjust.
	//
	void AdjustPoint(TPoint *pt);

	//////////
	// Sets the card's local coordinate system.
	//
	// [in] loc - set the origin to this location
	//
	void SetOrigin(TPoint &loc);

	//////////
	// Sets the card's local coordinate system.
	//
	// [in] inX - X-coordinate
	// [in] inY - Y-coordinate
	//
	void SetOrigin(int16 inX, int16 inY);

	//////////
	// Offsets the card's local coordinate system by the amount given.
	//
	// [in] delta - amount to offset the origin
	//
	void OffsetOrigin(TPoint &delta);
};

//////////
// Our global origin object.
//
extern Origin gOrigin;

END_NAMESPACE_FIVEL

#endif // TWinPrimitives_H
