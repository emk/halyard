// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TQTMovie.h"
#include "AppGlobals.h"
#include "FiveLApp.h"
#include "MovieWindow.h"

BEGIN_EVENT_TABLE(MovieWindow, wxWindow)
	EVT_ERASE_BACKGROUND(MovieWindow::OnEraseBackground)
    EVT_PAINT(MovieWindow::OnPaint)
    EVT_IDLE(MovieWindow::OnIdle)
    EVT_ACTIVATE(MovieWindow::OnActivate)
    EVT_LEFT_DOWN(MovieWindow::OnLeftDown)
	//EVT_KEY_DOWN(MovieWindow::OnKeyDown) - Incomplete!
END_EVENT_TABLE()

MovieWindow::MovieWindow(wxWindow *inParent, wxWindowID inID,
						 const wxPoint &inPos,
						 const wxSize &inSize,
						 long inWindowStyle,
						 MovieWindowStyle inMovieWindowStyle,
						 const wxString &inName)
    : wxWindow(inParent, inID, inPos, inSize, inWindowStyle, inName),
      mMovie(NULL), mMovieWindowStyle(inMovieWindowStyle)
{
    // Set a more-appropriate default background color for a movie.
    SetBackgroundColour(MOVIE_WINDOW_COLOR);

    // Prepare this window for use with QuickTime.
    // TODO - PORTING - This code is Windows-specific.
    mHWND = GetHWND();
    TQTMovie::RegisterWindowForMovies((HWND) mHWND);

	// If this is an audio-only movie, hide the widget.
	if (mMovieWindowStyle & MOVIE_AUDIO_ONLY)
		Hide();

	wxLogTrace(TRACE_STAGE_DRAWING, "Created movie window.");
}

MovieWindow::~MovieWindow()
{
    CleanUpMovie();

    // Detach this window from QuickTime.
    // TODO - PORTING - This code is Windows-specific.
    TQTMovie::UnregisterWindowForMovies((HWND) mHWND);

	wxLogTrace(TRACE_STAGE_DRAWING, "Destroyed movie window.");
}

void MovieWindow::CleanUpMovie()
{
    if (mMovie)
    {
		delete mMovie;
		mMovie = NULL;
    }
}

void MovieWindow::SetMovie(const wxString &inName)
{
    ASSERT(inName != "");

    // Detach any old movie, and attach the new one.
    CleanUpMovie();
    mMovie = new TQTMovie(TQTMovie::GetPortFromHWND((HWND) mHWND),
						  (const char *) inName);

	// Figure out what playback options we want.
	TQTMovie::PlaybackOptions opt = TQTMovie::kDefaultOptions;
	if (mMovieWindowStyle & MOVIE_CONTROLLER)
		opt |= TQTMovie::kEnableMovieController;
	if (mMovieWindowStyle & MOVIE_AUDIO_ONLY)
		opt |= TQTMovie::kAudioOnly;
	if (mMovieWindowStyle & MOVIE_LOOP)
		opt |= TQTMovie::kLoopMovie;

    // Set the movie to play as soon as it can.
    // TODO - We'll change this to better integrate with pre-rolling.
    Point p;
    p.h = p.v = 0;
    mMovie->StartWhenReady(opt, p);
}

MovieFrame MovieWindow::GetFrame()
{
	if (!mMovie || !mMovie->IsStarted())
		return 0;
	else
	{
		// TODO - I have no idea if this calculation is correct.  See
		// the QuickTime documentation.  But it appears to produce
		// correct results...
		TimeScale scale = mMovie->GetTimeScale();
		TimeValue time = mMovie->GetMovieTime();
		return time * FRAMES_PER_SECOND / scale;
	}
}

bool MovieWindow::IsDone()
{
	if (!mMovie)
		return false;
	else
		return mMovie->IsDone();
}

void MovieWindow::OnEraseBackground(wxEraseEvent &inEvent)
{
	if (mMovie && mMovie->IsStarted())
	{
		wxLogTrace(TRACE_STAGE_DRAWING,
				   "Ignoring request to erase movie window.");
	}
	else
	{
		wxLogTrace(TRACE_STAGE_DRAWING,
				   "Erasing background of movie window.");
		inEvent.Skip();
	}
}

void MovieWindow::OnPaint(wxPaintEvent &inEvent)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Asked to repaint movie window.");
    if (mMovie)
	{
		wxLogTrace(TRACE_STAGE_DRAWING, "Passing repaint event to movie.");
		mMovie->Redraw((HWND) mHWND);
	}

	// Let wxWindows handle this paint event properly.  It won't paint
	// anything, but it will do a bunch of other magic with a PaintDC
	// and other stuff, none of which I wish to figure out right now.
	inEvent.Skip();
}

void MovieWindow::OnIdle(wxIdleEvent &inEvent)
{
	if (mMovie)
		mMovie->Idle();
}

void MovieWindow::OnActivate(wxActivateEvent &inEvent)
{
	if (mMovie)
	{
		if (inEvent.GetActive())
			wxLogTrace(TRACE_STAGE_DRAWING, "Activate movie window.");
		else
			wxLogTrace(TRACE_STAGE_DRAWING, "Deactivate movie window.");
		mMovie->Activate((HWND) mHWND, inEvent.GetActive());
	}
}

void MovieWindow::OnLeftDown(wxMouseEvent &inEvent)
{
	// TODO - Figure out why double-clicking is broken.
	if (mMovie)
	{
		// Get usable event.when and event.modifiers values.  Note that
		// these aren't strictly accurate, merely usable.
		EventRecord event;
		mMovie->FillOutEvent((HWND) mHWND, WM_LBUTTONDOWN, (WPARAM) MK_LBUTTON,
							 (LPARAM) 0 /* We'll do the mouse. */, &event);

		// Get the click location and convert it to a Mac point.
		wxPoint p = inEvent.GetPosition();
		Point mac_point;
		mac_point.h = p.x;
		mac_point.v = p.y;

		// Pass the event to our movie.
		// TODO - Fix 'when' and 'modifiers' parameters.
		mMovie->Click((HWND) mHWND, mac_point, event.when, event.modifiers);
	}
}

void MovieWindow::OnKeyDown(wxKeyEvent &inEvent)
{
	// TODO - Only pass appropriate keys to QuickTime, and Skip() the
	// rest of the events.  Figure out why SPACE isn't working.  And
	// re-enable this handler in our event table.
	if (mMovie)
	{
		EventRecord event;
		mMovie->FillOutEvent((HWND) mHWND, WM_KEYDOWN,
							 (WPARAM) inEvent.GetRawKeyCode(),
							 (LPARAM) inEvent.GetRawKeyFlags(), &event);
		mMovie->Key((HWND) mHWND, event.message & charCodeMask,
					event.modifiers);
	}
}
