// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "MovieElement.h"
#include "MovieWindow.h"

MovieElement::MovieElement(Stage *inStage, const wxString &inName,
                           FIVEL_NS TCallbackPtr inDispatcher,
						   const wxRect &inBounds,
						   const wxString &inLocation,
						   long inWindowStyle,
						   MovieWindowStyle inMovieWindowStyle)
    : Widget(inStage, inName, inDispatcher), mMovieWindow(NULL),
	  mEndPlaybackWasCalled(false), mHaveSentMediaFinishedEvent(false)
{
    mMovieWindow = new MovieWindowNative(inStage, -1, inBounds.GetPosition(),
										 inBounds.GetSize(), inWindowStyle,
										 inMovieWindowStyle);
	mMovieWindow->SetMovie(inLocation);
	InitializeWidgetWindow(mMovieWindow);
}

bool MovieElement::HasVisibleRepresentation()
{
	if (mMovieWindow->GetMovieWindowStyle() & MOVIE_AUDIO_ONLY)
		return false;
	else
		return true;
}

bool MovieElement::HasReachedFrame(MovieFrame inFrame)
{
	if (mEndPlaybackWasCalled)
		return true;
	else if (inFrame == LAST_FRAME)
		return mMovieWindow->IsDone();
	else
		return mMovieWindow->IsDone() || (mMovieWindow->GetFrame() >= inFrame);
}

void MovieElement::Idle()
{
    if (!mHaveSentMediaFinishedEvent && HasReachedFrame(LAST_FRAME)) {
        mHaveSentMediaFinishedEvent = true;
        GetEventDispatcher()->DoEventMediaFinished();
    }
}

void MovieElement::EndPlayback()
{
	mMovieWindow->Pause();
	mEndPlaybackWasCalled = true;
}

void MovieElement::Pause()
{
	mMovieWindow->Pause();
}

void MovieElement::Resume()
{
	mMovieWindow->Resume();
}

void MovieElement::SetVolume(const std::string &inChannel, double inVolume)
{
    mMovieWindow->SetVolume(inChannel, inVolume);
}
