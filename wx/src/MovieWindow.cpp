// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "AppGlobals.h"
#include "FiveLApp.h"
#include "MovieWindow.h"

MovieWindow::MovieWindow(wxWindow *inParent, wxWindowID inID,
						 const wxPoint &inPos,
						 const wxSize &inSize,
						 long inWindowStyle,
						 MovieWindowStyle inMovieWindowStyle,
						 const wxString &inName)
    : wxWindow(inParent, inID, inPos, inSize, inWindowStyle, inName),
      mMovieWindowStyle(inMovieWindowStyle)
{
    // Set a more-appropriate default background color for a movie.
    SetBackgroundColour(MOVIE_WINDOW_COLOR);

	// If this is an audio-only movie, hide the widget.
	if (mMovieWindowStyle & MOVIE_AUDIO_ONLY)
		Hide();

	wxLogTrace(TRACE_STAGE_DRAWING, "Created movie window.");
}

MovieWindow::~MovieWindow()
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Destroyed movie window.");
}

void MovieWindow::SetMovie(const wxString &inName)
{
	wxLogWarning("Movies not supported; skipping \"%s\".",
				 (const char *) inName);
}

MovieFrame MovieWindow::GetFrame()
{
	return 0;
}

bool MovieWindow::IsDone()
{
	return true;
}

void MovieWindow::Pause()
{
	return;
}

void MovieWindow::Resume()
{
	return;
}
