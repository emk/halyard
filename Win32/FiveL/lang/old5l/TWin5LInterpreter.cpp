// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "stdafx.h"

#include "TLogger.h"
#include "TException.h"
#include "TWin5LInterpreter.h"
#include "lang/old5l/TParser.h"
#include "Header.h"
#include "lang/old5l/T5LPrimitives.h"
#include "TWin5LPrimitives.h"

#include "Globals.h"
#include "Card.h"
#include "Macro.h"
#include "Config.h"

USING_NAMESPACE_FIVEL

#if defined USE_BUNDLE
	LFileBundle			FIVEL_NS gFileManager;
#else
	LFileList           FIVEL_NS gFileManager;
#endif


//=========================================================================
// TWin5LInterpreter Methods 
//=========================================================================

TWin5LInterpreter::TWin5LInterpreter(const TString &inStartScript)
	: mKilled(false)
{
	// Read the startup script.
	if (!gIndexFileManager.NewIndex(inStartScript))
	{
		CleanupIndexes();
		throw TException(__FILE__, __LINE__,
						 "Error reading startup script");
	}
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
	gIndexFileManager.RemoveAll();
}

void TWin5LInterpreter::Run(SystemIdleProc inIdleProc)
{
	while (!mKilled)
	{
		(*inIdleProc)();
		gCardManager.Idle();
	}
}

void TWin5LInterpreter::KillInterpreter()
{
	mKilled = true;
	// XXX - The interpreter will keep running until it reaches the end of
	// the current card.  This is nasty, but it's actually the old
	// behavior.
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

void TWin5LInterpreter::Timeout(const char *inName, int32 inTime)
{
	gCardManager.Timeout(inName, inTime);
}

void TWin5LInterpreter::Nap(int32 inTime)
{
	gCardManager.Nap(inTime);
}

bool TWin5LInterpreter::Napping()
{
	return gCardManager.Napping();
}

void TWin5LInterpreter::KillCurrentCard()
{
	// TODO - Implement?
	throw TException(__FILE__, __LINE__, "Feature not implemented on Windows");
}

void TWin5LInterpreter::KillNap()
{
	gCardManager.KillNap();
}

void TWin5LInterpreter::JumpToCardByName(const char *inName)
{
	gCardManager.JumpToCardByName(inName);
}

std::string TWin5LInterpreter::CurCardName()
{
	return gCardManager.CurCardName();
}

std::string TWin5LInterpreter::PrevCardName()
{
	return gCardManager.PrevCardName();
}


//=========================================================================
// TWin5LCallback Methods
//=========================================================================

TWin5LCallback::TWin5LCallback(const TString &inCommand)
	: mCommand(inCommand)
{
	// Nothing else to do.
}

void TWin5LCallback::Run()
{
    gCardManager.OneCommand(mCommand);
}

std::string TWin5LCallback::PrintableRepresentation()
{
	return mCommand;
}

TCallback *TWin5LCallback::MakeCallback(const TString &inCmd)
{
	return new TWin5LCallback(inCmd);
}


//=========================================================================
// TWin5LInterpreterManager Methods
//=========================================================================

TWin5LInterpreterManager::TWin5LInterpreterManager(
	TInterpreter::SystemIdleProc inIdleProc)
	: TInterpreterManager(inIdleProc),
	  mDefPaletteProcessor("defpalette"),
	  mDefStyleProcessor("defstyle"),
	  mHeaderProcessor("header")
{
	// Initialize the file I/O system.
#if defined USE_BUNDLE
	if (not gFileManager.Init())
		throw TException(__FILE__, __LINE__,
						 "Could not initialize file manager");
#endif

	// Register our 5L-only interpreter primitives.
	Register5LPrimitives();
	RegisterWindows5LPrimitives();

	// Install our callback creator.
	TStream::SetCallbackMaker(&TWin5LCallback::MakeCallback);

	// Register our top-level forms.
	TParser::RegisterTlfProcessor("card", &gCardManager);
	TParser::RegisterTlfProcessor("macrodef", &gMacroManager);
	TParser::RegisterTlfProcessor("header", &mHeaderProcessor);
	TParser::RegisterTlfProcessor("defstyle", &mDefStyleProcessor);
	TParser::RegisterTlfProcessor("defpalette", &mDefPaletteProcessor);
}

TInterpreter *TWin5LInterpreterManager::MakeInterpreter()
{
	return new TWin5LInterpreter(gConfigManager.CurScript());
}
