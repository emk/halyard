// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Timecoder_H
#define Timecoder_H

#include "ToolWindow.h"

class MovieWindow;


//////////
// A Timecoder allows to scrub back and forth in a video clip, and shows
// you the timecodes associated with specific points in the movie.  It's
// commonly used when synchronizing audio and video.
//
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
