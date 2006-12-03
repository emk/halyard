// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"
#include "MovieElement.h"
#include "MovieWindow.h"

USING_NAMESPACE_FIVEL

MovieElement::MovieElement(Stage *inStage, const wxString &inName,
                           FIVEL_NS TCallbackPtr inDispatcher,
						   const wxRect &inBounds,
						   const wxString &inLocation,
						   long inWindowStyle,
						   MovieWindowStyle inMovieWindowStyle,
                           float inVolume)
    : Widget(inStage, inName, inDispatcher), mMovieWindow(NULL),
	  mEndPlaybackWasCalled(false), mHaveSentMediaErrorEvent(false),
      mHaveSentMediaTimeoutEvent(false)
{
    mMovieWindow = new MovieWindowNative(inStage, -1, inBounds.GetPosition(),
										 inBounds.GetSize(), inWindowStyle,
										 inMovieWindowStyle);
    mMovieWindow->SetVolume("all", inVolume);
	try {
        mMovieWindow->SetMovie(inLocation);
    } catch (std::exception &e) {
        // Log this exception.  We'll report it in our Idle() method,
        // because many movie errors occur after playback has started, so
        // it's more consistent to handle this one that way.
        gLog.Log("Movie error: %s for %s", e.what(),
                 inLocation.mb_str());
    }
	InitializeWidgetWindow(mMovieWindow);
}

bool MovieElement::HasVisibleRepresentation()
{
	if (mMovieWindow->GetMovieWindowStyle() & MOVIE_AUDIO_ONLY)
		return false;
	else
		return true;
}

bool MovieElement::WantsCursor() const {
    return mMovieWindow->WantsCursor();
}

bool MovieElement::HasReachedFrame(MovieFrame inFrame)
{
	if (mEndPlaybackWasCalled)
		return true;
	else if (inFrame == LAST_FRAME)
		return mMovieWindow->IsDone();
	else
		return mMovieWindow->IsDone() || (mMovieWindow->GetFrame() >= inFrame);
}

void MovieElement::Idle() {
    CheckWhetherMediaFinished();

    // Handle any errors.
    if (mMovieWindow->IsBroken()) {
        if (!mHaveSentMediaErrorEvent) {
            mHaveSentMediaErrorEvent = true;
            if (mMovieWindow->IsRemoteMovie())
                GetEventDispatcher()->DoEventMediaNetworkError();
            else
                GetEventDispatcher()->DoEventMediaLocalError();
        }
    }

    // Retrieve and display any captions.
    std::string caption;
    while (mMovieWindow->GetNextCaption(caption))
        GetEventDispatcher()->DoEventMediaCaption(caption);

    // See if our movie has timed out.
    if (mMovieWindow->HasTimedOut()) {
        if (!mHaveSentMediaTimeoutEvent) {
            mHaveSentMediaTimeoutEvent = true;
            GetEventDispatcher()->DoEventMediaNetworkTimeout();
        }
    }
}

bool MovieElement::IsLooping()
{
    return mMovieWindow->IsLooping();
}

void MovieElement::EndPlayback()
{
	mMovieWindow->Pause();
	mEndPlaybackWasCalled = true;
}

void MovieElement::Pause()
{
	mMovieWindow->Pause();
}

void MovieElement::Resume()
{
	mMovieWindow->Resume();
}

void MovieElement::SetVolume(const std::string &inChannel, double inVolume)
{
    mMovieWindow->SetVolume(inChannel, inVolume);
}

void MovieElement::SetTimeout(unsigned int timeout) {
    mMovieWindow->SetTimeout(timeout);
}
