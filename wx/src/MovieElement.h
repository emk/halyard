// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef MovieElement_H
#define MovieElement_H

#include "Widget.h"
#include "MovieWindow.h"

//////////
// A Widget subclass which implements some MovieWindow-specific features.
//
// TODO - Make a typedef for Frame counts.
//
class MovieElement : public Widget
{
    MovieWindow *mMovieWindow;

public:
    MovieElement(Stage *inStage, const wxString &inName,
				 const wxRect &inBounds, const wxString &inLocation,
				 long inWindowStyle, MovieWindowStyle inMovieWindowStyle);

    bool HasReachedFrame(MovieFrame inFrame);
};

#endif // MovieElement_H
