// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TQTMovie.h"
#include "FiveLApp.h"
#include "MovieWindow.h"

BEGIN_EVENT_TABLE(MovieWindow, wxWindow)
	//EVT_ERASE_BACKGROUND(MovieWindow::OnEraseBackground)
    EVT_PAINT(MovieWindow::OnPaint)
    EVT_IDLE(MovieWindow::OnIdle)
END_EVENT_TABLE()

MovieWindow::MovieWindow(wxWindow *inParent, wxWindowID inID,
						 const wxPoint &inPos,
						 const wxSize &inSize,
						 long inWindowStyle,
						 long inMovieWindowStyle,
						 const wxString &inName)
    : wxWindow(inParent, inID, inPos, inSize, inWindowStyle, inName),
      mMovie(NULL), mMovieWindowStyle(inMovieWindowStyle)
{
    // Set a more-appropriate default background color for a movie.
    SetBackgroundColour(*wxBLACK);

    // Prepare this window for use with QuickTime.
    // TODO - PORTING - This code is Windows-specific.
    mHWND = GetHWND();
    TQTMovie::RegisterWindowForMovies((HWND) mHWND);

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

    // Set the movie to play as soon as it can.
    // TODO - We'll change this to better integrate with pre-rolling.
    Point p;
    p.h = p.v = 0;
    mMovie->StartWhenReady(TQTMovie::kEnableMovieController |
						   TQTMovie::kEnableInteraction,
						   p);
}

int MovieWindow::GetFrame()
{
	if (!mMovie || !mMovie->IsStarted())
		return 0;
	else
	{
		// XXX - I have no idea if this calculation is correct.  See
		// the QuickTime documentation.
		TimeScale scale = mMovie->GetTimeScale();
		TimeValue time = mMovie->GetMovieTime();
		return time * 30 / scale;
	}
}

/*
long MovieWindow::MSWWindowProc(WXUINT message, WXWPARAM wParam,
								WXLPARAM lParam)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "MovieWindow::MSWWindowProc: %d %d %ld",
			   message, wParam, lParam);

	bool handled = false;
	if (mMovie)
	{
		// TODO - In theory, we should pay attention to the return
		// value of this function, but see the documentation in TQTMovie.h.
		handled = mMovie->HandleMovieEvent((HWND) mHWND, (UINT) message,
										   (WPARAM) wParam, (LPARAM) lParam);
	}

	// Pass the event along to be handled normally.
	//if (!handled)
	return wxWindow::MSWWindowProc(message, wParam, lParam);
	//else
	//return 0;
}
*/

void MovieWindow::OnEraseBackground(wxEraseEvent &inEvent)
{
	if (mMovie && mMovie->IsStarted())
	{
		inEvent.Skip();
		wxLogTrace(TRACE_STAGE_DRAWING,
				   "Ignoring request to erase movie window.");
	}
	else
	{
		wxLogTrace(TRACE_STAGE_DRAWING,
				   "Erasing background of movie window.");
	}
}

void MovieWindow::OnPaint(wxPaintEvent &inEvent)
{
	//wxPaintDC dc; // We must always create one of these in OnPaint.
	wxLogTrace(TRACE_STAGE_DRAWING, "Asked to repaint movie window.");
    if (mMovie)
	{
		wxLogTrace(TRACE_STAGE_DRAWING, "Passing repaint event to movie.");
		mMovie->Redraw((HWND) mHWND);
	}
	//else
	//{
	inEvent.Skip();
	//}
}

void MovieWindow::OnIdle(wxIdleEvent &inEvent)
{
	if (mMovie)
		mMovie->Idle();
}
