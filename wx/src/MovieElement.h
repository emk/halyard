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

public:
    MovieElement(Stage *inStage, const wxString &inName,
				 const wxRect &inBounds, const wxString &inLocation,
				 long inWindowStyle, MovieWindowStyle inMovieWindowStyle);

	virtual bool HasVisibleRepresentation();

    bool HasReachedFrame(MovieFrame inFrame);

	// Note: these methods may not be happy if the underlying movie code 
	// does not like to be paused.
	virtual void EndPlayback();
	virtual void Pause();
	virtual void Resume();
};

#endif // MovieElement_H
