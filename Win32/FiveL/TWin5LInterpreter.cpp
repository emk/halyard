// -*- Mode: C++; tab-width: 4; -*-

#include "stdafx.h"

#include "TLogger.h"
#include "TWin5LInterpreter.h"
#include "TStyleSheet.h"
#include "TParser.h"
#include "Header.h"

#include "Card.h"
#include "Macro.h"

BEGIN_NAMESPACE_FIVEL

// TODO - Make Win5L build again.
// TODO - Break dependencies of LTouchZone on Card.
// TODO - Move Win5L dependencies on TIndex(File) into here.


//=========================================================================
// TWin5LInterpreter Methods 
//=========================================================================

TWin5LInterpreter::TWin5LInterpreter()
{
	// Register our top-level forms.
	TParser::RegisterIndexManager("card", &gCardManager);
	TParser::RegisterIndexManager("macrodef", &gMacroManager);
	TParser::RegisterIndexManager("header", &gHeaderManager);
	TParser::RegisterIndexManager("defstyle", &gStyleSheetManager);
}

TWin5LInterpreter::~TWin5LInterpreter()
{
	CleanupIndexes();
}

void TWin5LInterpreter::CleanupIndexes()
{
	gCardManager.Pause();		
	gCardManager.RemoveAll();
	gMacroManager.RemoveAll();
	gHeaderManager.RemoveAll();
	gStyleSheetManager.RemoveAll();
	gIndexFileManager.RemoveAll();
}

void TWin5LInterpreter::Idle()
{
	gCardManager.Idle();
}

void TWin5LInterpreter::Pause()
{
	gCardManager.Pause();
}

void TWin5LInterpreter::WakeUp()
{
	gCardManager.WakeUp();
}

bool TWin5LInterpreter::Paused()
{
	return gCardManager.Paused();
}

void TWin5LInterpreter::Nap(int32 inTime)
{
	gCardManager.Nap(inTime);
}

bool TWin5LInterpreter::Napping()
{
	return gCardManager.Napping();
}

void TWin5LInterpreter::KillNap()
{
	gCardManager.KillNap();
}

void TWin5LInterpreter::JumpToCardByName(const char *inName)
{
	gCardManager.JumpToCardByName(inName);
}

const char *TWin5LInterpreter::CurCardName()
{
	return gCardManager.CurCardName();
}

const char *TWin5LInterpreter::PrevCardName()
{
	return gCardManager.PrevCardName();
}

//=========================================================================
// TWin5LCardCallback Methods
//=========================================================================

void TWin5LCardCallback::Run()
{
	gCardManager.JumpToCardByName(mCardName);
}


//=========================================================================
// TWin5LTouchZoneCallback Methods
//=========================================================================

TWin5LTouchZoneCallback::TWin5LTouchZoneCallback(const TString &inCommand)
	: mHaveSetCommand(false),
	  mRegularCommand(inCommand)
{
	// Nothing else to do.
}

TWin5LTouchZoneCallback::TWin5LTouchZoneCallback(const TString &inSetCommand,
												 const TString &inCommand)
	: mHaveSetCommand(true),
	  mSetCommand(inSetCommand),
	  mRegularCommand(inCommand)
{
	// Nothing else to do.	
}

void TWin5LTouchZoneCallback::Run()
{
	if (mHaveSetCommand)
    {
		gDebugLog.Log("TouchZone hit: set command <%s>",
					  mSetCommand.GetString());
    	gCardManager.OneCommand(mSetCommand);
    }
    
	gDebugLog.Log("TouchZone hit: regular command <%s>",
				  mRegularCommand.GetString());
    gCardManager.OneCommand(mRegularCommand);
}
