// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TInterpreter_h_)
#define _TInterpreter_h_

#include "TCommon.h"

BEGIN_NAMESPACE_FIVEL

//////////
// TInterpreter provides an abstract interface to a programming language
// interpreter used by the 5L engine.  In theory, it should be possible
// to change 5L's scripting language by providing a new implementation
// of this class.
//
class TInterpreter
{
public:
	//////////
	// Create a new TInterpreter object.  TInterpreter is a singleton
	// class; only one such object may be created (at any given time).
	// Call GetInstance (below) to return that instance.
	//
	TInterpreter();

	//////////
	// Our destructor is virtual to make sure the destructors of
	// our child classes get called.
	//
	virtual ~TInterpreter();

	//////////
	// Processing time for card manager.
	//
	virtual void Idle(void) = 0;

	//////////
	// Pause the CardManager.  Use WakeUp() to resume.
	//
	virtual void Pause(void) = 0;
	
	//////////
	// Wakeup from a Pause().  This might be called on cards
	// which aren't paused; check to be certain.
	//
	virtual void WakeUp(void) = 0;
	
	//////////
	// Is the CardManager paused?
	//
	// [out] return - true if paused, false otherwise
	//
	virtual bool Paused(void) = 0;

	//////////
	// Set the timeout timer. 
	//
	// [in] inName - name of Card to jump to after timeout.
	// [in] inTime - time in seconds (e.g. timeout in 30 sec)
	//
	virtual void Timeout(const char *inName, int32 inTime) = 0;

	//////////
	// Set the nap timer.
	//
	// [in] inTime - time in 1/10 seconds (e.g. 20 = timeout in 2 sec)
	//
	virtual void Nap(int32 inTime) = 0;        
	
	//////////
	// Is the CardManager napping?
	//
	// [out] return - true if napping, false otherwise
	//
	virtual bool Napping(void) = 0;
	
	//////////
	// Kill the nap timer.  This might be called on cards which
	// aren't napping; check to be certain.
	//
	virtual void KillNap(void) = 0;

	//////////
	// Permanently stop execution of the current card's script.
	//
	virtual void KillCurrentCard(void) = 0;

	//////////
	// Tell the CardManager to reload the current script
	// at the next call to Idle().
	//
	// [in] inCardName - name of the Card to jump to after reload.
	//
	virtual void DoReDoScript(const char *inCardName) = 0;

	//////////
	// Jump to a card given its name.
	//
	// [in] inName - name of the Card to jump to.
	//
	virtual void JumpToCardByName(const char *inName) = 0;
	
	//////////
	// Get name of current Card.
	//
	// [out] return - the name of the current card.
	//
	virtual const char *CurCardName(void) = 0;
        
	//////////
	// Get name of the previous Card.
	//
	// [out] return - the name of the previous card.
	//
	virtual const char *PrevCardName(void) = 0;
	
	//////////
	// Reload the currently running script, and jump to the
	// specified card name.  This operation has some odd semantics;
	// basically, it's intended to work like "Reload" in a web
	// browser.  Talk to one of the authoring staff to understand
	// the Zen of ReloadScript.
	//
	// [in] inGotoCardName - The card to jump to after reloading.
	//
	virtual void ReloadScript(const char *inGotoCardName) = 0;
        
	//////////
	// Do we have a single, global instance of this class?
	//
	static bool HaveInstance() { return (sInstance != NULL); }

	//////////
	// Return the single, global instance of this class.
	//
	static TInterpreter *GetInstance() { ASSERT(sInstance); return sInstance; }

private:
	static TInterpreter *sInstance;
};

//////////
// TCallback represents a "callback" function in the interpreter.  These
// functions may be called repeatedly.  Destroying the TInterpreter
// object invalidates all TCallbacks; calling ReDoScript may or may
// not invalidate any given callback.
//
class TCallback
{
	DISABLE_COPY_AND_ASSIGN(TCallback);

public:
	TCallback() {}
	virtual ~TCallback() {}

	//////////
	// Execute the callback.
	//
	virtual void Run() = 0;
};

END_NAMESPACE_FIVEL

#endif // TInterpreter
