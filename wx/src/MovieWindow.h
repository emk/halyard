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

#ifndef MovieWindow_H
#define MovieWindow_H

#include "AppConfig.h"
#include "AppGlobals.h" // For MovieFrame.

typedef unsigned long MovieWindowStyle;

enum /* MovieWindowStyle */ {
	MOVIE_CONTROLLER = 1,
	MOVIE_LOOP = 2,
	MOVIE_AUDIO_ONLY = 4,
	MOVIE_INTERACTION = 8
};

//////////
/// A wxWindow subclass for displaying and managing a movie.  By itself, this
/// class does nothing but display a blank widget; you generally want to use
/// an appropriate subclass to actually show a movie.
///
class MovieWindow : public wxWindow
{
    MovieWindowStyle mMovieWindowStyle;

public:
    //////////
    /// Create a new MovieWindow.  The parameters of this function are
    /// identical to those of the wxWindow constructor, except for 
    /// 'inWindowStyle', which corresponds to 'style', and
    /// 'inMovieWindowStyle', which handles options specific to this
    /// class.
    ///
    MovieWindow(wxWindow *inParent, wxWindowID inID = -1,
				const wxPoint &inPos = wxDefaultPosition,
				const wxSize &inSize = wxDefaultSize,
				long inWindowStyle = 0,
				MovieWindowStyle inMovieWindowStyle = 0,
				const wxString &inName = wxPanelNameStr);

    //////////
    /// Clean up the resources held by the MovieWindow.  Use Destroy()
    /// instead of calling this function directly.
    ///
    virtual ~MovieWindow();

	//////////
	/// Get the style of this movie window.
	///
	MovieWindowStyle GetMovieWindowStyle() const { return mMovieWindowStyle; }

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
    /// Does this movie want a cursor?
    ///
    virtual bool WantsCursor() const;

    //////////
    /// Returns true if the movie is being streamed over a network.
    ///
    virtual bool IsRemoteMovie() { return false; }

    //////////
    /// Returns true if the movie had an error of some sort.
    ///
    virtual bool IsBroken() { return false; }

    //////////
    /// Returns true if the movie has timed out.
    ///
    virtual bool HasTimedOut() { return false; }

    //////////
    /// Returns true if the movie is looping.
    ///
    virtual bool IsLooping() { return false; }

    //////////
    /// Is the movie finished?  This function is fairly smart about broken
	/// movies, looping movies, etc.
    ///
    virtual bool IsDone();

	//////////
	/// Pause the movie.
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
    /// movies.
    ///
	virtual void SetTimeout(unsigned int timeout) {}
};

// Define MovieWindowNative to map to an appropriate movie window class.
// This is essentially a low-budget MovieWindow "factory" pattern.
#if CONFIG_HAVE_QUICKTIME
#   include "MovieWindowQT.h"
#   define MovieWindowNative MovieWindowQT
#else // !CONFIG_HAVE_QUICKTIME
#   define MovieWindowNative MovieWindow
#endif // !CONFIG_HAVE_QUICKTIME

#endif // MovieWindow_H
