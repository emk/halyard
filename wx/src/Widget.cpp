// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "Widget.h"

Widget::Widget(Stage *inStage, const wxString &inName, wxWindow *inWindow)
    : Element(inStage, inName), mWindow(inWindow)
{
    ASSERT(inWindow);
}

Widget::~Widget()
{
    // XXX - Is this actually safe to do?  We might be called when we are
    // removed from the stage, or when the stage is destroyed.
    mWindow->Destroy();
}

