// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TWin5LInterpreter_h_)
#define _TWin5LInterpreter_h_

#include "TCommon.h"
#include "TInterpreter.h"
#include "TString.h"

BEGIN_NAMESPACE_FIVEL

//////////
// This class implements the Windows version of the 5L language.
//
class TWin5LInterpreter : public TInterpreter
{
public:
	TWin5LInterpreter(const TString &inStartScript);
	virtual ~TWin5LInterpreter();

	// This will become private once we refactor ReloadScript into this class.
	// TODO - Rename to PurgeScriptData, or something like that.
	void CleanupIndexes();

	// These methods are documented in our parent class.
	virtual void Idle();
	virtual void Pause();
	virtual void WakeUp();
	virtual bool Paused();
	virtual void Nap(int32 inTime);
	virtual bool Napping();
	virtual void KillNap();
	virtual void JumpToCardByName(const char *inName);
	virtual const char *CurCardName();
	virtual const char *PrevCardName();
	virtual void ReloadScript(const char *inGotoCardName);
};

//////////
// When invoked, this callback jumps to the specified card.  This
// callback remains valid across a ReloadScript.
//
class TWin5LCardCallback : public TCallback
{
	TString mCardName;

public:
	//////////
	// Create a new callback.
	//
	// [in] inCardName - The card to jump to when the callback is run.
	//
	TWin5LCardCallback(const TString &inCardName)
		: mCardName(inCardName) {}

	//////////
	// Jump to the specified card, if it exists.  Otherwise, log an
	// error.
	//
	virtual void Run();
};

//////////
// When invoked, this callback (1) runs an optional (set ...) command, and
// (2) runs a regular 5L command.  This callback remains valid across a
// ReloadScript (but that doesn't matter much, since all the touchzones
// get cleared).
//
// This class has a pretty crockish design; it was refactored out of
// LTouchZone, and is expected to be refactored again in the immediate
// future.
//
class TWin5LTouchZoneCallback : public TCallback
{
	bool mHaveSetCommand;
	TString mSetCommand;
	TString mRegularCommand;

public:
	//////////
	// Create a new callback.
	//
	// [in] inCommand - The command to run.
	//
	TWin5LTouchZoneCallback(const TString &inCommand);

	//////////
	// Create a new callback (with a set command).
	//
	// [in] inSetCommand - The set command to run.
	// [in] inRegularCommand - The regular command to run.
	//
	TWin5LTouchZoneCallback(const TString &inSetCommand,
							const TString &inRegularCommand);

	//////////
	// Run the appropriate commands.
	//
	virtual void Run();
};

END_NAMESPACE_FIVEL

#endif // TWin5LInterpreter
