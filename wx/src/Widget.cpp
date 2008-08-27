// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "AppHeaders.h"

#include "TCommon.h"
#include "TLogger.h"
#include "Widget.h"

using namespace Halyard;

Widget::Widget(Stage *inStage, const wxString &inName,
               Halyard::TCallbackPtr inDispatcher)
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

Widget::~Widget() {
    // XXX - Is this actually safe to do?  We might be called when we are
    // removed from the stage, or when the stage is destroyed.
	//
	// We only destroy the window if it's been created.
	if (mWindow)
		mWindow->Destroy();
}

void Widget::InitializeWidgetWindow(wxWindow *inWindow) {
	ASSERT(mWindow == NULL);
	ASSERT(inWindow != NULL);
	mWindow = inWindow;
}

wxRect Widget::GetRect() {
	ASSERT(mWindow != NULL);
	return mWindow->GetRect();
}

void Widget::Show(bool inShow) {
    if (!HasVisibleRepresentation()) {
        // A Widget without a visible representation should never be shown
        // in the first place, so we can call our superclass's version of
        // this function and get behavior compatible with our old API.
        // Specifically, if we are hiding a hidden wigdet, nothing will
        // happen, and if we are trying to show a hidden widget, we'll get
        // an error.
        ASSERT(!IsShown());
        Element::Show(inShow);
        ASSERT(!IsShown());
    } else {
        // If we're not changing anything, quit now.
        if (inShow == IsShown())
            return;

        // Update the window's visibility, and notify the stage of
        // the change.
        if (inShow)
            mWindow->Show();
        else
            mWindow->Hide();
        GetStage()->NotifyElementsChanged();
    }
}

bool Widget::IsShown() {
	ASSERT(mWindow != NULL);
	return mWindow->IsShown();
}

void Widget::DrawElementBorder(wxDC &inDC) {
	// Draw the border *outside* our rectangle.
	wxRect r = this->GetRect();
	r.Inflate(1);
	inDC.DrawRectangle(r.x, r.y, r.width, r.height);
}

void Widget::SetFocus() {
    // This should work for most subclasses.
    mWindow->SetFocus();
}

bool Widget::ApplyClippingToStage(wxRegion &ioRegion) {
    ASSERT(IsShown());
    if (!ioRegion.Subtract(this->GetRect()))
        gLog.FatalError("Cannot update clipping region");
    return true;
}

#if wxUSE_ACCESSIBILITY

wxAccessible *Widget::GetAccessible() {
    return mWindow->GetAccessible();
}

#endif wxUSE_ACCESSIBILITY
