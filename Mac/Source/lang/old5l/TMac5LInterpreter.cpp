// -*- Mode: C++; tab-width: 4; -*-

#include "TLogger.h"
#include "TException.h"
#include "TMac5LInterpreter.h"
#include "lang/old5l/TParser.h"
#include "TStyleSheet.h"
#include "TParser.h"
#include "CHeader.h"
#include "lang/old5l/T5LPrimitives.h"
#include "TMac5LPrimitives.h"

#include "CCard.h"
#include "CMacroManager.h"
#include "CModule.h"

USING_NAMESPACE_FIVEL


//=========================================================================
// TMac5LInterpreter Methods 
//=========================================================================

TMac5LInterpreter::TMac5LInterpreter(const TString &inStartScript)
	: mKilled(false)
{
	// Read the startup script.
	if (!gIndexFileManager.NewIndex(inStartScript))
	{
		CleanupIndexes();
		throw TException(__FILE__, __LINE__, "Error reading startup script");
	}
}

TMac5LInterpreter::~TMac5LInterpreter()
{
	CleanupIndexes();
}

void TMac5LInterpreter::CleanupIndexes()
{
	gCardManager.CurCardKill();		
	gCardManager.RemoveAll();
	gMacroManager.RemoveAll();
	gHeaderManager.RemoveAll();
	gStyleSheetManager.RemoveAll();
	gIndexFileManager.RemoveAll();
}

void TMac5LInterpreter::Run(SystemIdleProc inIdleProc)
{
	while (!mKilled)
	{
		(*inIdleProc)();
		gCardManager.CurCardSpendTime();
	}
}

void TMac5LInterpreter::KillInterpreter()
{
	mKilled = true;
	// XXX - The interpreter will keep running until it reaches the end of
	// the current card.  This is nasty, but it's actually the old
	// behavior.
}

void TMac5LInterpreter::Pause()
{
	CCard *card = gCardManager.GetCurCard();
	ASSERT(card);
	card->Pause();
}

void TMac5LInterpreter::WakeUp()
{
	gCardManager.CurCardWakeUp();
}

bool TMac5LInterpreter::Paused()
{
	return gCardManager.CurCardPaused();
}

void TMac5LInterpreter::Timeout(const char *inName, int32 inTime)
{
	CCard *card = gCardManager.GetCurCard();
	ASSERT(card);
	card->Timeout(inTime, inName);
}

void TMac5LInterpreter::Nap(int32 inTime)
{
	CCard *card = gCardManager.GetCurCard();
	ASSERT(card);
	card->Nap(inTime);
}

bool TMac5LInterpreter::Napping()
{
	return gCardManager.CurCardNapping();
}

void TMac5LInterpreter::KillNap()
{
	// TODO - Implement?
	throw TException(__FILE__, __LINE__, "Feature not implemented on Mac");
}

void TMac5LInterpreter::KillCurrentCard()
{
	gCardManager.CurCardKill();
}

void TMac5LInterpreter::JumpToCardByName(const char *inName)
{
	gCardManager.JumpToCardByName(inName, false);
}

std::string TMac5LInterpreter::CurCardName()
{
	return gCardManager.CurCardName();
}

std::string TMac5LInterpreter::PrevCardName()
{
	return gCardManager.PrevCardName();
}


//=========================================================================
// TMac5LCallback Methods
//=========================================================================

TMac5LCallback::TMac5LCallback(const TString &inCommand)
	: mCommand(inCommand)
{
	// Nothing else to do.
}

std::string TMac5LCallback::PrintableRepresentation()
{
	return mCommand;
}

void TMac5LCallback::Run()
{
	if (gCardManager.CardManagerReady())
		gCardManager.DoOneCommand(mCommand);
	else
		// XXX - Race condition with leftover keybinds during a redoscript.
		// The stale keybinds are still around (and will become valid in
		// a second), but we haven't actually begun executing our first
		// card.  This is silly legacy cruft.
		gLog.Error("Can't run callback yet.  Please wait a moment.");
}

TCallback *TMac5LCallback::MakeCallback(const TString &inCmd)
{
	return new TMac5LCallback(inCmd);
}


//=========================================================================
// TWin5LInterpreterManager Methods
//=========================================================================

TMac5LInterpreterManager::TMac5LInterpreterManager(
	TInterpreter::SystemIdleProc inIdleProc)
	: TInterpreterManager(inIdleProc),
	  mDefPaletteProcessor("defpalette"),
	  mDefStyleProcessor("defstyle"),
	  mHeaderProcessor("header")
{
	// Register our 5L-only interpreter primitives.
	Register5LPrimitives();
	RegisterMacintosh5LPrimitives();

	// Install our callback creator.
	TStream::SetCallbackMaker(&TMac5LCallback::MakeCallback);

	// Register our top-level forms.
	TParser::RegisterTlfProcessor("card", &gCardManager);
	TParser::RegisterTlfProcessor("macrodef", &gMacroManager);
	TParser::RegisterTlfProcessor("header", &mHeaderProcessor);
	TParser::RegisterTlfProcessor("defstyle", &mDefStyleProcessor);
	TParser::RegisterTlfProcessor("defpalette", &mDefPaletteProcessor);
}

TInterpreter *TMac5LInterpreterManager::MakeInterpreter()
{
	// Set the default module.
	if (!(gModMan->HaveModules() && gModMan->LoadModule(-1)))
		gLog.FatalError("Mac5L was unable to open a script.  Please make sure "
						"Mac5L is in a directory with a \"Mac5L.config\" file "
						"and other program data files.");

	// Create a new interpreter.
	return new TMac5LInterpreter(gModMan->GetCurScript());
}
