// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include "TCommon.h"
#include "Quake2Engine.h"

#if CONFIG_HAVE_QUAKE2

Quake2Element::Quake2Element(Stage *inStage, const wxString &inName)
    : Widget(inStage, inName), mQuake2Window(NULL)
{
	mQuake2Window = new wxQuake2Window(inStage);
	InitializeWidgetWindow(mQuake2Window);
}

void Quake2Element::ExecCommand(const wxString &inName)
{
	mQuake2Window->ExecCommand(inName);
}

#endif CONFIG_HAVE_QUAKE2
