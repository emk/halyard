// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/rawbmp.h>

#include "Transition.h"
#include "TLogger.h"


//=========================================================================
//  Transition
//=========================================================================

class Transition
{
protected:
	//////////
	// Show a single step of the transition.
	//
	// [in] inStep - a number > 0.0 and < 1.0, corresponding to the step
	//          to show.
	// [in] inResources - The resources to use.
	//
	virtual void ShowStep(double inStep, TransitionResources &inResources) = 0;

public:
    Transition();
    void RunTransition(int inMilliseconds, TransitionResources &inResources);
};

Transition::Transition()
{
    // Do nothing.
}

void Transition::RunTransition(int inMilliseconds,
							   TransitionResources &inResources)
{
	// TODO - Keep a running average of how long each step takes.
	for (double time = 0.2; time < 1.0; time += 0.2)
		ShowStep(time, inResources);
}


//=========================================================================
//  CrossFade
//=========================================================================

class CrossFade : public Transition
{
	void ShowStep(double inStep, TransitionResources &inResources);
};

void CrossFade::ShowStep(double inStep, TransitionResources &inResources)
{
	wxNativePixelData before_data(inResources.GetBeforeBmp());
	wxNativePixelData after_data(inResources.GetAfterBmp());
	wxNativePixelData scratch_data(inResources.GetScratchBmp());

	// Figure out our blending factors.  Use a 0-256 scale so
	// we can later divide by 256 using a fast '>> 8'.
	int alpha = 256 * inStep;
	int beta  = 256 - alpha;

	// Our outer loop.
	wxNativePixelData::Iterator before_row_start(before_data);
	wxNativePixelData::Iterator after_row_start(after_data);
	wxNativePixelData::Iterator scratch_row_start(scratch_data);
	for (int y = inResources.GetBeforeBmp().GetHeight(); y > 0; --y)
	{
		wxNativePixelData::Iterator before(before_row_start);
		wxNativePixelData::Iterator after(after_row_start);
		wxNativePixelData::Iterator scratch(scratch_row_start);

		// Our fast inner loop.
		for (int y = inResources.GetBeforeBmp().GetWidth(); y > 0; --y)
		{
			scratch.Red() = (after.Red() * alpha + before.Red() * beta) >> 8;
			scratch.Green() =
				(after.Green() * alpha + before.Green() * beta) >> 8;
			scratch.Blue() =
				(after.Blue() * alpha + before.Blue() * beta) >> 8;

			++before;
			++after;
			++scratch;
		}

		before_row_start.OffsetY(before_data, 1);
		after_row_start.OffsetY(after_data, 1);
		scratch_row_start.OffsetY(scratch_data, 1);
	}

	// Draw our scratch buffer to the screen.
	inResources.GetOutputDC().DrawBitmap(inResources.GetScratchBmp(), 0, 0,
										 false);
}


//=========================================================================
//  TransitionManager
//=========================================================================

TransitionManager::TransitionManager()
{
    RegisterTransition("crossfade", new CrossFade());
    //RegisterTransition("wipeleft",  new Transition());
    //RegisterTransition("wiperight", new Transition());
    //RegisterTransition("wipeup",    new Transition());
    //RegisterTransition("wipedown",  new Transition());
}

TransitionManager::~TransitionManager()
{
    TransitionMap::iterator i = mTransitions.begin();
    for (; i != mTransitions.end(); ++i)
        delete i->second;
}

void TransitionManager::RegisterTransition(const std::string &inName,
                                           Transition *inTransition)
{
    TransitionMap::iterator found = mTransitions.find(inName);
    if (found == mTransitions.end())
        mTransitions.insert(TransitionMap::value_type(inName, inTransition));
    else
    {
        gLog.Caution("Duplicate transition: %s", inName.c_str());
        delete inTransition;
    }    
}

void TransitionManager::RunTransition(const std::string &inName,
                                      int inMilliseconds,
									  TransitionResources &inResources)
{
    TransitionMap::iterator found = mTransitions.find(inName);
    if (found == mTransitions.end())
        gLog.Caution("Unknown transition: %s", inName.c_str());
    else
        found->second->RunTransition(inMilliseconds, inResources);
}
