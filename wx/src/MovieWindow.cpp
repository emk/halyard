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

#include "TamaleHeaders.h"
#include "AppGlobals.h"
#include "FiveLApp.h"
#include "MovieWindow.h"

MovieWindow::MovieWindow(wxWindow *inParent, wxWindowID inID,
						 const wxPoint &inPos,
						 const wxSize &inSize,
						 long inWindowStyle,
						 MovieWindowStyle inMovieWindowStyle,
						 const wxString &inName)
    : wxWindow(), // Must use empty constructor; see below.
      mMovieWindowStyle(inMovieWindowStyle)
{
    // Turn off background repainting completely.  (Theoretically, if we do
    // this here, we don't need to override EVT_ERASE_BACKGROUND and throw
    // away the message.)  We must call this *before* Create(...), because
    // MSW will draw the window immediately upon creation.
    SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    // Set a more-appropriate default background color for a movie.
    SetBackgroundColour(MOVIE_WINDOW_COLOR);

	// If this is an audio-only movie, hide the widget.
	if (mMovieWindowStyle & MOVIE_AUDIO_ONLY)
		Hide();

    // Create() our actual window *after* all our options are set up.
    Create(inParent, inID, inPos, inSize, inWindowStyle, inName);
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

bool MovieWindow::WantsCursor() const {
    unsigned flags =
        GetMovieWindowStyle() & (MOVIE_CONTROLLER|MOVIE_INTERACTION);
    return (flags ? true : false);
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

void MovieWindow::SetVolume(const std::string &inChannel, double inVolume)
{
    return;
}
