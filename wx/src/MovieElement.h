// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MovieElement_H
#define MovieElement_H

#include "Widget.h"
#include "MediaElement.h"
#include "MovieWindow.h"

//////////
// A Widget subclass which implements some MovieWindow-specific features.
//
// TODO - Make a typedef for Frame counts.
//
class MovieElement : public Widget, public IMediaElement
{
    MovieWindow *mMovieWindow;
	bool mEndPlaybackWasCalled;
    bool mHaveSentMediaFinishedEvent;

public:
    MovieElement(Stage *inStage, const wxString &inName,
                 FIVEL_NS TCallbackPtr inDispatcher,
				 const wxRect &inBounds, const wxString &inLocation,
				 long inWindowStyle, MovieWindowStyle inMovieWindowStyle);

	virtual bool HasVisibleRepresentation();

    bool HasReachedFrame(MovieFrame inFrame);

    virtual void Idle();

	// Note: these methods may not be happy if the underlying movie code 
	// does not like to be paused.
    virtual bool IsLooping();
	virtual void EndPlayback();
	virtual void Pause();
	virtual void Resume();
	virtual void SetVolume(const std::string &inChannel, double inVolume);
};

#endif // MovieElement_H
