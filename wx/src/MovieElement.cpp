// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "AppHeaders.h"
#include "MovieElement.h"
#include "MovieWindow.h"

using namespace Halyard;

MovieElement::MovieElement(Stage *inStage, const wxString &inName,
                           Halyard::TCallbackPtr inDispatcher,
						   const wxRect &inBounds,
						   const wxString &inLocation,
						   long inWindowStyle,
						   MovieWindowStyle inMovieWindowStyle,
                           float inVolume)
    : Widget(inStage, inName, inDispatcher),
      mLocationInfo(inLocation), mMovieWindow(NULL),
	  mHaveSentMediaErrorEvent(false),
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
        std::string location(inLocation.mb_str());
        gLog.Info("halyard", "Movie error: %s for %s", e.what(),
                  location.c_str());
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

bool MovieElement::IsDone() {
    return mMovieWindow->IsDone();
}

MovieFrame MovieElement::CurrentFrame() {
    if (mMovieWindow->IsDone())
        return LAST_FRAME;
    else
        return mMovieWindow->GetFrame();
}

void MovieElement::Idle() {
    MediaElementIdle();

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

bool MovieElement::ApplyClippingToStage(wxRegion &ioRegion) {
    // If mMovieWindow has already loaded some actual movie data, then it
    // knows what to paint on the screen.  If not, then we actually want
    // Stage::PaintStage to draw *over* this element, as if it wasn't even
    // here.
    //
    // Our goal is to have a seamless transition from the stage background
    // color/graphic to the playing movie, without any intermediate
    // repaints caused by an movie widget with no movie data (and hence, no
    // clue about what it should actually paint).
    //
    // Yes, this is a particularly nasty attempt to hand-simulate a
    // transparent wxWindow.  And this is also why the Stage can't just use
    // wxCLIP_CHILDEN--sometimes, certain children *want* to be overdrawn.
    if (mMovieWindow->IsReadyToHandleOwnDrawing())
        return Widget::ApplyClippingToStage(ioRegion);
    else
        return false;
}

bool MovieElement::IsLooping()
{
    return mMovieWindow->IsLooping();
}

void MovieElement::EndPlayback()
{
    MediaElement::EndPlayback();
	mMovieWindow->Pause();
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
