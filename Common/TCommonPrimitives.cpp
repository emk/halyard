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
	TPoint p(bounds.Right(), bounds.Bottom());
	gOrigin.UnadjustPoint(&p);
	gVariableManager.SetLong("_Graphic_X", (int32) p.X());
	gVariableManager.SetLong("_Graphic_Y", (int32) p.Y());
}

void FIVEL_NS UpdateSpecialVariablesForText(const TPoint &bottomLeft)
{
	TPoint p = bottomLeft;
	gOrigin.UnadjustPoint(&p);
	gVariableManager.SetLong("_INCR_X", (int32) p.X());
	gVariableManager.SetLong("_INCR_Y", (int32) p.Y());
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

void Origin::UnadjustPoint(TPoint *pt)
{
	pt->OffsetX(-mOrigin.X());
	pt->OffsetY(-mOrigin.Y());
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
