// -*- Mode: C++; tab-width: 4; -*-

// Needed for RegisterCommonPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"
#include "TVariable.h"

USING_NAMESPACE_FIVEL

Origin FIVEL_NS gOrigin;


//=========================================================================
//  RegisterCommonPrimitives
//=========================================================================
//  Install our portable primitive functions.

void FIVEL_NS RegisterCommonPrimitives()
{
	// No Common primitives yet.
}


//=========================================================================
//  Support Methods
//=========================================================================

void FIVEL_NS UpdateSpecialVariablesForGraphic(const TRect &bounds)
{
	gVariableManager.SetLong("_Graphic_X", (short) bounds.Right());
	gVariableManager.SetLong("_Graphic_Y", (short) bounds.Bottom());
}


//=========================================================================
//  Origin Methods
//=========================================================================

void Origin::AdjustRect(TRect *r)
{
	r->Offset(mOrigin);
}

void Origin::AdjustPoint(TPoint *pt)
{
	pt->Offset(mOrigin);
}

TPoint Origin::GetOrigin()
{
	return mOrigin;
}

void Origin::SetOrigin(TPoint &loc)
{
    mOrigin = loc;
	gVariableManager.SetLong("_originx", mOrigin.X());
	gVariableManager.SetLong("_originy", mOrigin.Y());
}

void Origin::SetOrigin(int16 inX, int16 inY)
{
	TPoint newOrigin(inX, inY);
	SetOrigin(newOrigin);
}

void Origin::OffsetOrigin(TPoint &delta)
{
	TPoint newOrigin(mOrigin);
	newOrigin.Offset(delta);
	SetOrigin(newOrigin);
}


//=========================================================================
//  Implementation of Common Primitives
//=========================================================================
//  No Common primitives yet.
