// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include "TCommon.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "Quake2Engine.h"

#if CONFIG_HAVE_QUAKE2

// TODO - Do we want to allow the Quake 2 to refresh the screen while
// another window is displayed over it?

Quake2Engine::Quake2Engine(Stage *inStage)
    : wxQuake2Window(inStage)
{
	ASSERT(!sHasBeenCreated && !sInstance);
	sInstance = this;
	sHasBeenCreated = true;

	// TODO - Do something reasonable about stages which aren't 640x480.
}

Quake2Engine::~Quake2Engine()
{
	sInstance = NULL;
}

void Quake2Engine::Initialize()
{
	new Quake2Engine(wxGetApp().GetStage());
}

#endif CONFIG_HAVE_QUAKE2
