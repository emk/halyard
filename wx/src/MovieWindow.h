// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MovieWindow_H
#define MovieWindow_H

class TQTMovie;

//////////
// A wxWindow subclass which can display and manage a QuickTime movie.
// TODO - Still frightfully incomplete, but enough to get by with.
//
class MovieWindow : public wxWindow
{
    WXHWND mHWND;
    TQTMovie *mMovie;
    long mMovieWindowStyle;

    void CleanUpMovie();

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
				long inMovieWindowStyle = 0,
				const wxString &inName = wxPanelNameStr);

    //////////
    // Clean up the resources held by the MovieWindow.  Use Destroy()
    // instead of calling this function directly.
    //
    virtual ~MovieWindow();

    //////////
    // Ask this widget to create and manage the specified movie.
    //
    void SetMovie(const wxString &inName);

    //////////
    // Get the current frame of the movie (where there are 30 frames
	// per second).  Returns 0 if there is no movie or the movie hasn't
	// started.  This number *can* decrease, especially if the user
	// is playing with the movie controller.
    //
    int GetFrame();

	//////////
	// QuickTime needs to receive raw Windows events to implement
	// interaction (the controller, Flash, etc.).  We could painstakingly
	// reconstruct these events by hand from the wxEvents we receive,
	// or we can simply intercept the raw Windows events as they are fed
	// to us.  We choose the later course of action.
	//
	// TODO - PORTABILITY - We'll need a similar hack on the Macintosh,
	// which may require modifying the wxWindows source code.
	//
	//virtual long MSWWindowProc(WXUINT message, WXWPARAM wParam,
	//						   WXLPARAM lParam);

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

    DECLARE_EVENT_TABLE();
};

#endif // MovieWindow_H
