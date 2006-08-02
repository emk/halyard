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

#include "AppGraphics.h"
#include "Stage.h"
#include "Timecoder.h"
#include "MovieWindow.h"

#define TEST_MOVIE "Media\\credits_v.mov"


//=========================================================================
// Timecoder Methods
//=========================================================================

BEGIN_EVENT_TABLE(Timecoder, ToolWindow)
	EVT_IDLE(Timecoder::OnIdle)
END_EVENT_TABLE()

Timecoder::Timecoder(StageFrame *inStageFrame)
    : ToolWindow(inStageFrame, TOOL_TIMECODER, "Timecoder",
				 wxICON(ic_timecoder))
{
    // Create a panel to hold our movie.
	// TODO - This is way too hard-coded.
    mMovieWindow =
		new MovieWindowNative(this, -1, wxDefaultPosition, wxSize(320, 256),
							  0, MOVIE_CONTROLLER);
    mMovieWindow->SetMovie(TEST_MOVIE);

    // Create a panel to hold our timecode value.
    wxPanel *tc_panel = new wxPanel(this);
    mTimecodeLabel =
		new wxStaticText(tc_panel, -1, "0:00:00", wxDefaultPosition,
						 wxSize(200, -1), wxALIGN_CENTRE | wxST_NO_AUTORESIZE);
    wxBoxSizer *tc_sizer = new wxBoxSizer(wxVERTICAL);
    tc_sizer->Add(mTimecodeLabel, 0 /* don't stretch */, wxGROW | wxALL, 3);
    tc_panel->SetSizer(tc_sizer);
    tc_sizer->SetSizeHints(tc_panel);

    // Lay out the window.
    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(mMovieWindow, 1 /* stretch */, wxGROW, 0);
    sizer->Add(tc_panel, 0 /* don't stretch */, wxGROW, 0);
    SetSizer(sizer);
    sizer->SetSizeHints(this);
}

void Timecoder::OnIdle(wxIdleEvent &inEvent)
{
	// Get the most recent frame count.
	MovieFrame frames = mMovieWindow->GetFrame();
	int seconds = frames / FRAMES_PER_SECOND;
	int minutes = seconds / 60;

	// Make a label.
	wxString label;
	label.Printf("%d:%02d:%02d",
				 minutes, seconds % 60, (int) frames % FRAMES_PER_SECOND);

	// If the label is different from what we're currently displaying,
	// update it.  We don't update the label unnecessarily, because
	// that would cause annoying flicker.
	if (label != mTimecodeLabel->GetLabel())
		mTimecodeLabel->SetLabel(label);
}
