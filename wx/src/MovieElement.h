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

#ifndef MovieElement_H
#define MovieElement_H

#include "Widget.h"
#include "MediaElement.h"
#include "MovieWindow.h"

//////////
/// A Widget subclass which implements some MovieWindow-specific features.
///
/// TODO - Make a typedef for Frame counts.
///
class MovieElement : public Widget, public MediaElement
{
    MovieWindow *mMovieWindow;
	bool mEndPlaybackWasCalled;
    bool mHaveSentMediaErrorEvent;
    bool mHaveSentMediaTimeoutEvent;

protected:
    virtual bool IsDone();
    virtual MovieFrame CurrentFrame();

public:
    MovieElement(Stage *inStage, const wxString &inName,
                 FIVEL_NS TCallbackPtr inDispatcher,
				 const wxRect &inBounds, const wxString &inLocation,
				 long inWindowStyle, MovieWindowStyle inMovieWindowStyle,
                 float inVolume = 1.0f);

	virtual bool HasVisibleRepresentation();
    virtual bool WantsCursor() const;

    virtual void Idle();
    virtual bool ApplyClippingToStage(wxRegion &ioRegion);

	// Note: Some of these methods may not be happy if the underlying movie
	// code does not like to be paused.
    virtual bool IsLooping();
	virtual void EndPlayback();
	virtual void Pause();
	virtual void Resume();
	virtual void SetVolume(const std::string &inChannel, double inVolume);
	virtual void SetTimeout(unsigned int timeout);
};

#endif // MovieElement_H
