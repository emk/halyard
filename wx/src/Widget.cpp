// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "Widget.h"

Widget::Widget(Stage *inStage, const wxString &inName)
	: Element(inStage, inName), mWindow(NULL)
{
	// Our subclass must call InitializeWidgetWindow before exiting its
	// constructor.
}


Widget::Widget(Stage *inStage, const wxString &inName, wxWindow *inWindow)
    : Element(inStage, inName), mWindow(NULL)
{
    InitializeWidgetWindow(inWindow);
}

Widget::~Widget()
{
    // XXX - Is this actually safe to do?  We might be called when we are
    // removed from the stage, or when the stage is destroyed.
    mWindow->Destroy();
}

void Widget::InitializeWidgetWindow(wxWindow *inWindow)
{
	ASSERT(mWindow == NULL);
	ASSERT(inWindow != NULL);
	mWindow = inWindow;
}

wxRect Widget::GetRect()
{
	ASSERT(mWindow != NULL);
	return mWindow->GetRect();
}

bool Widget::IsShown()
{
	ASSERT(mWindow != NULL);
	return mWindow->IsShown();
}
