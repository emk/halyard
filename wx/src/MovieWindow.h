// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MovieWindow_H
#define MovieWindow_H

#include "AppGlobals.h" // For MovieFrame.

typedef unsigned long MovieWindowStyle;

enum /* MovieWindowStyle */ {
	MOVIE_CONTROLLER = 1,
	MOVIE_LOOP = 2,
	MOVIE_AUDIO_ONLY = 4
};

//////////
// A wxWindow subclass for displaying and managing a movie.  By itself, this
// class does nothing but display a blank widget; you generally want to use
// an appropriate subclass to actually show a movie.
//
class MovieWindow : public wxWindow
{
    MovieWindowStyle mMovieWindowStyle;

public:
    //////////
    // Create a new MovieWindow.  The parameters of this function are
    // identical to those of the wxWindow constructor, except for 
    // 'inWindowStyle', which corresponds to 'style', and
    // 'inMovieWindowStyle', which handles options specific to this
    // class.
    //
    MovieWindow(wxWindow *inParent, wxWindowID inID = -1,
				const wxPoint &inPos = wxDefaultPosition,
				const wxSize &inSize = wxDefaultSize,
				long inWindowStyle = 0,
				MovieWindowStyle inMovieWindowStyle = 0,
				const wxString &inName = wxPanelNameStr);

    //////////
    // Clean up the resources held by the MovieWindow.  Use Destroy()
    // instead of calling this function directly.
    //
    virtual ~MovieWindow();

	//////////
	// Get the style of this movie window.
	//
	MovieWindowStyle GetMovieWindowStyle();

    //////////
    // Ask this widget to create and manage the specified movie.
    //
    virtual void SetMovie(const wxString &inName);

    //////////
    // Get the current frame of the movie (where there are 30 frames
	// per second).  Returns 0 if there is no movie or the movie hasn't
	// started.  This number *can* decrease, especially if the user
	// is playing with the movie controller.
    //
    virtual MovieFrame GetFrame();

    //////////
    // Is the movie finished?  This function is fairly smart about broken
	// movies, looping movies, etc.
    //
    virtual bool IsDone();
};

// Define MovieWindowNative to map to an appropriate movie window class.
// This is essentially a low-budget MovieWindow "factory" pattern.
#ifdef FIVEL_NO_MOVIES
#   define MovieWindowNative MovieWindow
#else // !defined FIVEL_NO_MOVIES
#   include "MovieWindowQT.h"
#   define MovieWindowNative MovieWindowQT
#endif // !defined FIVEL_NO_MOVIES

#endif // MovieWindow_H
