// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Quake2Engine_H
#define Quake2Engine_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUAKE2

#include "Widget.h"
#include "wxquake2.h"

//////////
// Our local "value-added" subclass of wxQuake2Window.  This class
// is a singleton, and once created, it remains permanently attached
// to the Stage object (although we may hide and show it).
//
class Quake2Engine : public wxQuake2Window
{
	static bool sHasBeenCreated;
    static Quake2Engine *sInstance;

    //////////
    // Create a new Quake 2 window on the specified stage.
    //
    Quake2Engine(Stage *inStage);

public:
	~Quake2Engine();

	static bool IsInitialized() { return sHasBeenCreated; }
	static void Initialize();
	static Quake2Engine *GetInstance() { ASSERT(sInstance); return sInstance; }
};

#endif // CONFIG_HAVE_QUAKE2
#endif // Quake2Engine_H
