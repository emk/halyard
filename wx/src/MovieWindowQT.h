// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MovieWindowQT_H
#define MovieWindowQT_H

class TQTMovie;

//////////
// A wxWindow subclass which can display and manage a QuickTime movie.
// TODO - Still frightfully incomplete, but enough to get by with.
//
class MovieWindowQT : public MovieWindow
{
    WXHWND mHWND;
    TQTMovie *mMovie;

	//////////
	// Delete any movie attached to this object.
	//
    void CleanUpMovie();

public:
    //////////
    // Create a new MovieWindowQT.  The parameters of this function are
    // identical to those of the MovieWindow constructor.
    //
    MovieWindowQT(wxWindow *inParent, wxWindowID inID = -1,
				  const wxPoint &inPos = wxDefaultPosition,
				  const wxSize &inSize = wxDefaultSize,
				  long inWindowStyle = 0,
				  MovieWindowStyle inMovieWindowStyle = 0,
				  const wxString &inName = wxPanelNameStr);

    //////////
    // Clean up the resources held by the MovieWindow.  Use Destroy()
    // instead of calling this function directly.
    //
    virtual ~MovieWindowQT();

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

	void OnEraseBackground(wxEraseEvent &inEvent);

	//////////
	// Some Sorenson codecs need to receive this paint message, or they
	// won't display video at all.  So just smile and nod, and do what
	// they want.
	//
	// TODO - I haven't tested whether this is still necessary under
	// wxWindows, but it was in older applications using this QuickTime
	// interface.)
	//
	void OnPaint(wxPaintEvent &inEvent);

	void OnIdle(wxIdleEvent &inEvent);

	void OnActivate(wxActivateEvent &inEvent);

	void OnLeftDown(wxMouseEvent &inEvent);

	void OnKeyDown(wxKeyEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // MovieWindowQT_H
