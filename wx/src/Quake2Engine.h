// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Quake2Engine_H
#define Quake2Engine_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUAKE2

#include "Widget.h"
#include "wxquake2.h"

//////////
// A Widget subclass which contains a Quake 2 game.
//
class Quake2Element : public Widget
{
    wxQuake2Window *mQuake2Window;

public:
    //////////
    // Create a new Quake 2 window on the specified stage.
    //
    Quake2Element(Stage *inStage, const wxString &inName);

	//////////
	// Send a command to the Quake 2 window.
	//
	void ExecCommand(const wxString &inName);
};

#endif // CONFIG_HAVE_QUAKE2
#endif // Quake2Engine_H
