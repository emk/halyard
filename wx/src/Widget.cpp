// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "TCommon.h"
#include "TLogger.h"
#include "Widget.h"

Widget::Widget(Stage *inStage, const wxString &inName,
               FIVEL_NS TCallbackPtr inDispatcher)
	: Element(inStage, inName, inDispatcher), mWindow(NULL)
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

void Widget::Show(bool inShow)
{
	// If we're not changing anything, quit now.
	if (inShow == IsShown())
		return;

	if (inShow && !HasVisibleRepresentation())
	{
		gLog.Error("Cannot show a window without a visible representation");
		return;
	}

	// Update the window's visibility, and notify the stage of
	// the change.
	if (inShow)
		mWindow->Show();
	else
		mWindow->Hide();
	GetStage()->NotifyElementsChanged();
}

bool Widget::IsShown()
{
	ASSERT(mWindow != NULL);
	return mWindow->IsShown();
}

void Widget::DrawElementBorder(wxDC &inDC)
{
	// Draw the border *outside* our rectangle.
	wxRect r = this->GetRect();
	r.Inflate(1);
	inDC.DrawRectangle(r.x, r.y, r.width, r.height);
}
