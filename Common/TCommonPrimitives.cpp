// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterCommonPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"
#include "TVariable.h"
#include "TLogger.h"
#include "TDateUtil.h"
#include "TStyleSheet.h"

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
	REGISTER_5L_PRIMITIVE(HavePrimitive);
	REGISTER_5L_PRIMITIVE(Log);
	REGISTER_5L_PRIMITIVE(Origin);
	REGISTER_5L_PRIMITIVE(ResetOrigin);
	REGISTER_5L_PRIMITIVE(Set);
	REGISTER_5L_PRIMITIVE(Get);
	REGISTER_5L_PRIMITIVE(DefStyle);
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
// (HavePrimitive name:STRING)
//-------------------------------------------------------------------------
// Check to see whether the specified 5L primitive exists.  Helpful in
// writing code which runs under different versions of the engine.

DEFINE_5L_PRIMITIVE(HavePrimitive)
{
	std::string name;
	inArgs >> name;
	::SetPrimitiveResult(gPrimitiveManager.DoesPrimitiveExist(name));
}


//-------------------------------------------------------------------------
// (Log file:STRING msg:STRING [level:STRING = "log"])
//-------------------------------------------------------------------------
// Logs the second argument to the file specified by the first.
// Available logs: debug, 5L, MissingMedia.  Available log levels:
// fatalerror, error, caution, log.

DEFINE_5L_PRIMITIVE(Log)
{
	// Logging this primitive call would be redundant and ugly.
	::SkipPrimitiveLogging();

	std::string log_name, msg, level;
	level = "log";
	inArgs >> log_name >> msg;
	if (inArgs.HasMoreArguments())
		inArgs >> level;
	log_name = ::MakeStringLowercase(log_name);
	level = ::MakeStringLowercase(level);

	// Figure out which log file to use.
	TLogger *log = &gLog;
	if (log_name == "5l")
		log = &gLog;
	else if (log_name == "debug")
		log = &gDebugLog;
	else if (log_name == "missingmedia")
		log = &gMissingMediaLog;
	else
		gLog.Caution("No such log file: %s", log_name.c_str());

	// Report the problem using the appropriate log level.
	if (level == "log")
		log->Log("%s", msg.c_str());
	else if (level == "caution")
		log->Caution("%s", msg.c_str());
	else if (level == "error")
		log->Error("%s", msg.c_str());
	else if (level == "fatalerror")
		log->FatalError("%s", msg.c_str());
	else
	{
		gLog.Error("Unknown logging level: %s", level.c_str());
		gLog.FatalError("%s", msg.c_str());
	}
}


//-------------------------------------------------------------------------
// (ORIGIN DX DY)
//-------------------------------------------------------------------------
// Move the local coordinates for this particular card (or macro) by 
// the delta values given. This change is an offset from whatever the 
// current coordinates are. There is no way to set the absolute 
// coordinates for a macro or card!

DEFINE_5L_PRIMITIVE(Origin)
{
    TPoint   delta;

    inArgs >> delta;

    gOrigin.OffsetOrigin(delta);
    
	TPoint origin = gOrigin.GetOrigin();
    gDebugLog.Log("Origin set to <X Y> %d %d", origin.X(), origin.Y());
}


//-------------------------------------------------------------------------
// (ResetOrigin [DX DY])
//-------------------------------------------------------------------------
// Reset the origin or set it to something new.

DEFINE_5L_PRIMITIVE(ResetOrigin)
{
	TPoint		newOrigin(0, 0);

	if (inArgs.HasMoreArguments())
		inArgs >> newOrigin;

	gOrigin.SetOrigin(newOrigin);
}


//-------------------------------------------------------------------------
// (Set VARIABLE NEWVALUE [Flag])
//-------------------------------------------------------------------------
// Sets the variable to the given value.

DEFINE_5L_PRIMITIVE(Set)
{
    TString     vname;
    TString		value;
    TString		flag; 
	uint32      date_value;
	int32		date_type;

    inArgs >> vname >> value;
    
	if (inArgs.HasMoreArguments())
    {
    	inArgs >> flag;
    	flag.MakeLower();

		if (flag.Equal("longdate"))
    		date_type = DT_LONGDATE;
    	else if (flag.Equal("date"))
    		date_type = DT_DATE;
    	else if (flag.Equal("time"))
    		date_type = DT_TIME;
    	else if (flag.Equal("year"))
    		date_type = DT_YEAR;
    	else if (flag.Equal("month"))
    		date_type = DT_MONTH;
    	else if (flag.Equal("longmonth"))
    		date_type = DT_LONGMONTH;
    	else if (flag.Equal("day"))
    		date_type = DT_DAY;
    	else if (flag.Equal("longday"))
    		date_type = DT_LONGDAY;
    	else
    		gLog.Caution("Bad flag to set command <%s>.", flag.GetString());

		date_value = (uint32) value;

    	gVariableManager.SetDate(vname.GetString(), date_value, date_type);
	}
	else
	{
		gVariableManager.SetString(vname.GetString(), value.GetString());
	}
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


//-------------------------------------------------------------------------
// (DefStyle NAME ...)
//-------------------------------------------------------------------------
// Create a stylesheet with the given name.

DEFINE_5L_PRIMITIVE(DefStyle)
{
	gStyleSheetManager.AddStyleSheet(inArgs);
}
