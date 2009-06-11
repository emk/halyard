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

#ifndef MediaElement_H
#define MediaElement_H

#include "AppGlobals.h"

BEGIN_NAMESPACE_HALYARD
class CaptionList;
END_NAMESPACE_HALYARD

class MediaInfoPane;


//////////
/// An interface for elements which play media streams.  This is used as a
/// mixin class.
///
class MediaElement
{
    bool mEndPlaybackWasCalled;
    bool mHaveSentMediaFinishedEvent;
    shared_ptr<Halyard::CaptionList> mCaptions;

    bool mHasPlaybackTimer;
    MovieFrame mTriggerPlaybackTimerAt;

    Element *GetThisAsElement();

protected:
    //////////
    /// Do any idle-time processing needed by a media element, and send any
    /// events that we need to send.
    ///
    /// If the media is finished, send a media-finished event to this
    /// element.  This will need to be called by the element's Idle()
    /// method.
    ///
    virtual void MediaElementIdle();

    //////////
    /// Return true if playback has finished, according to the underlying
    /// media layer.  Note that this function is only intended to be
    /// called by MediaElement methods, and then only after they've
    /// checked mEndPlaybackWasCalled.
    ///
    virtual bool IsDone() = 0;
    
    //////////
    /// Return the current frame of the MediaElement.  May return
    /// LAST_FRAME if the movie is done (or broken) and we can't calculate
    /// a more specific end value.  You generally shouldn't bother calling
    /// this if IsDone returns true.
    ///
    virtual MovieFrame CurrentFrame() = 0;

public:
    MediaElement();
	virtual ~MediaElement() {}

    //////////
    /// Attach a caption file to this movie.
    ///
    void AttachCaptionFile(const std::string &inCaptionFile);

    //////////
    /// Return true if the movie has reached the specified frame (or the
    /// movie is done).  If inFrame is LAST_FRAME, then return true only
    /// when the movie reaches the end.
    ///
    bool HasReachedFrame(MovieFrame inFrame);

    //////////
    /// Returns true if the media is looping.
    ///
    virtual bool IsLooping() = 0;

	//////////
	/// Cease playback, and make sure all future calls to HasReachedFrame()
	/// return true.  If you override this, be sure /// to call
	/// MediaElement::EndPlayback().
	///
	virtual void EndPlayback();

    //////////
    /// Pause the media stream.
    ///
    /// Note: This method may not be happy if the underlying movie layer
    /// doesn't like to be paused.  In particular, streaming network
    /// QuickTime is chancy.
    ///
    virtual void Pause() = 0;

    //////////
    /// Resume playing the media stream after a pause.
    ///
    virtual void Resume() = 0;

    //////////
    /// Set the volume of this media element.
    ///
    /// \param inChannel  The channel to change.  Typical values include
    ///                  "left", "right" and "all", although future media
    ///                  frameworks may define more channels.
    /// \param inVolume  The volume.  0.0 is silent, 1.0 is the media's
    ///                 normal volume.
    ///
	virtual void SetVolume(const std::string &inChannel, double inVolume) = 0;


    //////////
    /// Set up a playback timer which we be triggered at the specified
    /// time.  LAST_FRAME may be used here.  When the timer is triggered,
    /// it will send an event to the EventDispatcher for this element.
    /// Each playback timer will trigger exactly once unless cleared (or
    /// set to trigger at an invalid frame).
    ///
    /// It is an error to overwrite an existing playback timer.
    ///
    virtual void SetPlaybackTimer(MovieFrame inFrame);

    //////////
    /// Clear any playback timer which may have been installed for this
    /// element.  Does nothing if no playback timer is active.
    ///
    virtual void ClearPlaybackTimer();

    //////////
    /// Return some sort of path or location information for this media
    /// clip (for display purposes only).  If none is available, return "".
    /// It's acceptable to return relative or abstract paths.
    ///
    virtual wxString GetLocationInfo() { return wxT(""); }

    //////////
    /// Write information about this clip to inMediaInfoPane.
    ///
    virtual void WriteInfoTo(MediaInfoPane *inMediaInfoPane);
};

#endif // MediaElement_H
