// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterCommonPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"
#include "TVariable.h"
#include "TLogger.h"

// Needed to implement the primitives.
#include <string>

USING_NAMESPACE_FIVEL

Origin FIVEL_NS gOrigin;


//=========================================================================
//  RegisterCommonPrimitives
//=========================================================================
//  Install our portable primitive functions.

void FIVEL_NS RegisterCommonPrimitives()
{
	REGISTER_5L_PRIMITIVE(Log);
	REGISTER_5L_PRIMITIVE(Get);
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
	TRect orig = *r;
	r->Offset(mOrigin);

	// We log this here because it's too annoying to integrate directly
	// into TArgumentList.
	if (!(orig == *r))
		gDebugLog.Log("Adjusting: (rect %d %d %d %d) to (rect %d %d %d %d)",
					  orig.Left(), orig.Top(), orig.Right(), orig.Bottom(),
					  r->Left(), r->Top(), r->Right(), r->Bottom());
}

void Origin::AdjustPoint(TPoint *pt)
{
	TPoint orig = *pt;
	pt->Offset(mOrigin);

	// We log this here because it's too annoying to integrate directly
	// into TArgumentList.
	if (!(orig == *pt))
		gDebugLog.Log("Adjusting: (pt %d %d) to (pt %d %d)",
					  orig.X(), orig.Y(), pt->X(), pt->Y());
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

//-------------------------------------------------------------------------
// (LOG STRING STRING)
//-------------------------------------------------------------------------
// Logs the second argument to the file specified by the first.
// Available logs: debug, 5L, MissingMedia.

DEFINE_5L_PRIMITIVE(Log)
{
	std::string log_name, msg;
	inArgs >> log_name >> msg;
	log_name = ::MakeStringLowercase(log_name);

	if (log_name == "5l")
		gLog.Log("%s", msg.c_str());
	else if (log_name == "debug")
		gDebugLog.Log("%s", msg.c_str());
	else if (log_name == "missingmedia")
		gMissingMediaLog.Log("%s", msg.c_str());
	else
		gDebugLog.Caution("No such log file: %s", log_name.c_str());
}

//-------------------------------------------------------------------------
// (Get VARIABLE)
//-------------------------------------------------------------------------
// Returns the value stored in the variable, represented as a string.

DEFINE_5L_PRIMITIVE(Get)
{
	TString vname;
   	inArgs >> vname;
   	::SetPrimitiveResult(gVariableManager.GetString(vname.GetString()));
}
