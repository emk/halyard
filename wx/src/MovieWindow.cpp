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

#include "AppHeaders.h"
#include "AppGlobals.h"
#include "HalyardApp.h"
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
    // If this is an audio-only movie, hide the widget.
    if (mMovieWindowStyle & MOVIE_AUDIO_ONLY)
        Hide();

    // Ideally, we would Create() our actual window *after* all our options
    // are set up.  But that crashes hard on the Mac.
    Create(inParent, inID, inPos, inSize, inWindowStyle, inName);
    wxLogTrace(TRACE_STAGE_DRAWING, wxT("Created movie window."));

    // Turn off background repainting completely.  (Theoretically, if we do
    // this here, we don't need to override EVT_ERASE_BACKGROUND and throw
    // away the message.)  We must call this *before* Create(...), because
    // MSW will draw the window immediately upon creation.
    SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    // Set a more-appropriate default background color for a movie.
    SetBackgroundColour(MOVIE_WINDOW_COLOR);
}

MovieWindow::~MovieWindow() {
    wxLogTrace(TRACE_STAGE_DRAWING, wxT("Destroyed movie window."));
}

void MovieWindow::SetMovie(const wxString &inName) {
    // Here, we use wxString::c_str(), because we want to return either a
    // const char * or a const wchar_t *, depending on whether or not we
    // are in Unicode mode.  I can't find any documentation on wxLogWarning
    // in Unicode mode, but I Googled around and found the following code
    // which uses wxLogDebug with c_str:
    // http://wxcode.sourceforge.net/components/wxmswres/src/wxmswres.cpp
    wxLogWarning(wxT("Movies not supported; skipping \"%s\"."), inName.c_str());
}

MovieFrame MovieWindow::GetFrame() {
    return 0;
}

bool MovieWindow::WantsCursor() const {
    unsigned flags =
        GetMovieWindowStyle() & (MOVIE_CONTROLLER|MOVIE_INTERACTION);
    return (flags ? true : false);
}

bool MovieWindow::IsDone() {
    return true;
}

void MovieWindow::Pause() {
    return;
}

void MovieWindow::Resume() {
    return;
}

void MovieWindow::SetVolume(const std::string &inChannel, double inVolume) {
    return;
}
