// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef Transition_H
#define Transition_H

class Transition;

//////////
/// A set of display-related resources which are needed to perform a
/// transition.  We stick these in a class so we don't need to pass
/// them around as separate parameters everywhere.
///
class TransitionResources
{
	wxDC &mOutputDC;
	wxBitmap &mBeforeBmp;
	wxBitmap &mAfterBmp;
	wxBitmap &mScratchBmp;

public:
	//////////
	/// Create a new set of TransitionResources.
	///
	/// \param inOutputDC  The DC to which we should draw our output.
	/// \param inBeforeBmp  What the screen looks like before our
	///        transition.
	/// \param inAfterBmp  What the screen looks like after our transition.
	/// \param inScratchBmp  A bitmap which we can use for building
	///        intermediate stages, if we wish.
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
/// This class holds all known transitions, and performs them upon request.
///
class TransitionManager
{
	typedef std::map<std::string,Transition*> TransitionMap;

	TransitionMap mTransitions;

	//////////
	/// Register a transition with the transition manager.
	///
	void RegisterTransition(const std::string &inName,
							Transition *inTransition);

public:
	TransitionManager();
	~TransitionManager();

	//////////
	/// Draw the intermediate steps of the specified transition.  Do
	/// not display the first or last step; assume these are handled for
	/// us by our caller.
	///
	/// \param inName  The transition to use.
	/// \param inMilliseconds  The amount of time the transition should
	///        ideally take.  The transition may take slightly more or
	///        less time.
	/// \param inResources  Resources to use for the transition.
	///
	void RunTransition(const std::string &inName, int inMilliseconds,
					   TransitionResources &inResources);
};

#endif // Transition_H
