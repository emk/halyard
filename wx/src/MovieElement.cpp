// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include "TCommon.h"
#include "MovieElement.h"
#include "MovieWindow.h"

MovieElement::MovieElement(Stage *inStage, const wxString &inName,
						   const wxRect &inBounds,
						   const wxString &inLocation,
						   long inWindowStyle,
						   MovieWindowStyle inMovieWindowStyle)
    : Widget(inStage, inName), mMovieWindow(NULL)
{
    mMovieWindow = new MovieWindowNative(inStage, -1, inBounds.GetPosition(),
										 inBounds.GetSize(), inWindowStyle,
										 inMovieWindowStyle);
	mMovieWindow->SetMovie(inLocation);
	InitializeWidgetWindow(mMovieWindow);
}

bool MovieElement::HasReachedFrame(MovieFrame inFrame)
{
	if (inFrame == LAST_FRAME)
		return mMovieWindow->IsDone();
	else
		return (mMovieWindow->GetFrame() >= inFrame);
}
