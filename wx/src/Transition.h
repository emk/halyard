// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Transition_H
#define Transition_H

#include "TCommon.h"

#include <string>
#include <map>

class Transition;

//////////
// A set of display-related resources which are needed to perform a
// transition.  We stick these in a class so we don't need to pass
// them around as separate parameters everywhere.
//
class TransitionResources
{
	wxDC &mOutputDC;
	wxBitmap &mBeforeBmp;
	wxBitmap &mAfterBmp;
	wxBitmap &mScratchBmp;

public:
	//////////
	// Create a new set of TransitionResources.
	//
	// [in] inOutputDC - The DC to which we should draw our output.
	// [in] inBeforeBmp - What the screen looks like before our
	//        transition.
	// [in] inAfterBmp - What the screen looks like after our transition.
	// [in] inScratchBmp - A bitmap which we can use for building
	//        intermediate stages, if we wish.
	TransitionResources(wxDC &inOutputDC, wxBitmap &inBeforeBmp,
						wxBitmap &inAfterBmp, wxBitmap &inScratchBmp)
		: mOutputDC(inOutputDC), mBeforeBmp(inBeforeBmp),
		  mAfterBmp(inAfterBmp), mScratchBmp(inScratchBmp) {}
	
	wxDC &GetOutputDC() { return mOutputDC; }
	wxBitmap &GetBeforeBmp() { return mBeforeBmp; }
	wxBitmap &GetAfterBmp() { return mAfterBmp; }
	wxBitmap &GetScratchBmp() { return mScratchBmp; }
};

//////////
// This class holds all known transitions, and performs them upon request.
//
class TransitionManager
{
	typedef std::map<std::string,Transition*> TransitionMap;

	TransitionMap mTransitions;

	//////////
	// Register a transition with the transition manager.
	//
	void RegisterTransition(const std::string &inName,
							Transition *inTransition);

public:
	TransitionManager();
	~TransitionManager();

	//////////
	// Draw the intermediate steps of the specified transition.  Do
	// not display the first or last step; assume these are handled for
	// us by our caller.
	//
	// [in] inName - The transition to use.
	// [in] inMilliseconds - The amount of time the transition should
	//        ideally take.  The transition may take slightly more or
	//        less time.
	// [in] inResources - Resources to use for the transition.
	//
	void RunTransition(const std::string &inName, int inMilliseconds,
					   TransitionResources &inResources);
};

#endif // Transition_H
