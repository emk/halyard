// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#ifndef MovieWindowQT_H
#define MovieWindowQT_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUICKTIME

class TQTMovie;

//////////
/// A wxWindow subclass which can display and manage a QuickTime movie.
/// TODO - Still frightfully incomplete, but enough to get by with.
///
class MovieWindowQT : public MovieWindow
{
    WXHWND mHWND;
    TQTMovie *mMovie;
    bool mCouldNotConstructMovie;
    bool mIsRemote;
    unsigned int mTimeout;

    enum {
        // By default, allow playback to stall for 40 seconds.
        DEFAULT_TIMEOUT = 40
    };

    //////////
    /// Get the CGraphPtr associated with this window.  This is either a
    /// real CGraphPtr on the Mac, or a reasonable immitation provided by
    /// QuickTime for Windows.
    ///
    void *GetMacPort();

    //////////
    /// Where should we draw this movie, relative to our CGrafPtr?  This
    /// varies depending on how the underlying graphics architecture works.
    ///
    wxPoint GetMoviePosRelativeToPort();

	//////////
	/// Delete any movie attached to this object.
	///
    void CleanUpMovie();

public:
    //////////
    /// Create a new MovieWindowQT.  The parameters of this function are
    /// identical to those of the MovieWindow constructor.
    ///
    MovieWindowQT(wxWindow *inParent, wxWindowID inID = -1,
				  const wxPoint &inPos = wxDefaultPosition,
				  const wxSize &inSize = wxDefaultSize,
				  long inWindowStyle = 0,
				  MovieWindowStyle inMovieWindowStyle = 0,
				  const wxString &inName = wxPanelNameStr);

    //////////
    /// Clean up the resources held by the MovieWindow.  Use Destroy()
    /// instead of calling this function directly.
    ///
    virtual ~MovieWindowQT();

    //////////
    /// Ask this widget to create and manage the specified movie.
    ///
    virtual void SetMovie(const wxString &inName);

    //////////
    /// Get the current frame of the movie (where there are 30 frames
	/// per second).  Returns 0 if there is no movie or the movie hasn't
	/// started.  This number *can* decrease, especially if the user
	/// is playing with the movie controller.
    ///
    virtual MovieFrame GetFrame();

    //////////
    /// Returns true if the movie is being streamed over a network.
    ///
    virtual bool IsRemoteMovie();
    
    //////////
    /// Returns true if this movie has loaded enough data to make
    /// intelligent decisions about what to paint on the screen.
    ///
    virtual bool IsReadyToHandleOwnDrawing() const;

    //////////
    /// Returns true if the movie had an error of some sort.
    ///
    virtual bool IsBroken();

    //////////
    /// Returns true if the movie has timed out.
    ///
    virtual bool HasTimedOut();

    //////////
    /// Get the next caption to be displayed, if any.  Captions should
    /// be displayed as soon as they become available.
    ///
    virtual bool GetNextCaption(std::string &outCaption);

    //////////
    /// Returns true if the movie is looping.
    ///
    virtual bool IsLooping();

    //////////
    /// Is the movie finished?  This function is fairly smart about broken
	/// movies, looping movies, etc.
    ///
    virtual bool IsDone();

	//////////
	/// Pause the movie
	/// Note: these methods may not be happy if the underlying movie code 
	/// does not like to be paused.
	///
	virtual void Pause();

	//////////
	/// Resume the movie
	///
	virtual void Resume();

    //////////
    /// Set the movie's volume.
    ///
	virtual void SetVolume(const std::string &inChannel, double inVolume);

    //////////
    /// Set the number of seconds which can pass without inactivity
    /// without causing a timeout.  This generally affects only networked
    /// movies.  (There's a fair bit of intelligence in how timeouts
    /// are calculated. Typically, any playback activity resets the timeout,
    /// and playing with the movie controller turns it off for good.)
    ///
    virtual void SetTimeout(unsigned int timeout);

	void OnEraseBackground(wxEraseEvent &inEvent);

	//////////
	/// Some Sorenson codecs need to receive this paint message, or they
	/// won't display video at all.  So just smile and nod, and do what
	/// they want.
	///
	/// TODO - I haven't tested whether this is still necessary under
	/// wxWindows, but it was in older applications using this QuickTime
	/// interface.)
	///
	void OnPaint(wxPaintEvent &inEvent);

	void OnIdle(wxIdleEvent &inEvent);

    void OnMouseMove(wxMouseEvent &inEvent);

	void OnActivate(wxActivateEvent &inEvent);

	void OnLeftDown(wxMouseEvent &inEvent);

	void OnKeyDown(wxKeyEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // CONFIG_HAVE_QUICKTIME
#endif // MovieWindowQT_H
