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

#ifndef Timecoder_H
#define Timecoder_H

#include "ToolWindow.h"

class MovieWindow;


//////////
/// A Timecoder allows to scrub back and forth in a video clip, and shows
/// you the timecodes associated with specific points in the movie.  It's
/// commonly used when synchronizing audio and video.
///
class Timecoder : public ToolWindow
{
	MovieWindow *mMovieWindow;
	wxStaticText *mTimecodeLabel;

public:
	Timecoder(StageFrame *inStageFrame);

	void OnIdle(wxIdleEvent &inEvent);
	
    DECLARE_EVENT_TABLE();
};

#endif // Timecoder_H
