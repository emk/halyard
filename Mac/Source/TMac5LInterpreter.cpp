// -*- Mode: C++; tab-width: 4; -*-

#include "TLogger.h"
#include "TException.h"
#include "TMac5LInterpreter.h"
#include "TStyleSheet.h"
#include "TParser.h"
#include "CHeader.h"

#include "CCard.h"
#include "CMacroManager.h"

USING_NAMESPACE_FIVEL


//=========================================================================
// TMac5LInterpreter Methods 
//=========================================================================

TMac5LInterpreter::TMac5LInterpreter(const TString &inStartScript)
{
	// Install our callback creator.
	TStream::SetCallbackMaker(&TMac5LCallback::MakeCallback);

	// Register our top-level forms.
	TParser::RegisterIndexManager("card", &gCardManager);
	TParser::RegisterIndexManager("macrodef", &gMacroManager);
	TParser::RegisterIndexManager("header", &gHeaderManager);
	TParser::RegisterIndexManager("defstyle", &gStyleSheetManager);

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

void TMac5LInterpreter::Idle()
{
	gCardManager.CurCardSpendTime();
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

void TMac5LInterpreter::DoReDoScript(const char *inCardName)
{
#ifdef DEBUG
	gCardManager.DoReDoScript(TString(inCardName));
#endif
}

void TMac5LInterpreter::JumpToCardByName(const char *inName)
{
	gCardManager.JumpToCardByName(inName, false);
}

const char *TMac5LInterpreter::CurCardName()
{
	return gCardManager.CurCardName();
}

const char *TMac5LInterpreter::PrevCardName()
{
	return gCardManager.PrevCardName();
}

void TMac5LInterpreter::ReloadScript(const char *inGotoCardName)
{
	// XXX - Implement.
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
