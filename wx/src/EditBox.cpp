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

#include "TamaleHeaders.h"
#include "EditBox.h"
#include "EventDispatcher.h"


//=========================================================================
//  CustomTextCtrl
//=========================================================================

class CustomTextCtrl : public wxTextCtrl {
    EditBox *mElement;

    void OnChar(wxKeyEvent &inEvent);
    void OnText(wxCommandEvent &inEvent);
    void OnTextEnter(wxCommandEvent &inEvent);

public:
    CustomTextCtrl(wxWindow *inParent, const wxString &inValue,
                   const wxRect &inRect, long inStyle,
                   EditBox *inElement);

    DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(CustomTextCtrl, wxTextCtrl)
    EVT_CHAR(CustomTextCtrl::OnChar)
    EVT_TEXT(-1, CustomTextCtrl::OnText)
    EVT_TEXT_ENTER(-1, CustomTextCtrl::OnTextEnter)
END_EVENT_TABLE()

CustomTextCtrl::CustomTextCtrl(wxWindow *inParent, const wxString &inValue,
                               const wxRect &inBounds, long inStyle,
                               EditBox *inElement)
    : wxTextCtrl(inParent, -1, inValue, inBounds.GetPosition(),
                 inBounds.GetSize(), inStyle),
      mElement(inElement)
{
    // Do nothing.
}

void CustomTextCtrl::OnChar(wxKeyEvent &inEvent) {
    if (!mElement->GetEventDispatcher()->DoEventChar(inEvent))
        inEvent.Skip();
}

void CustomTextCtrl::OnText(wxCommandEvent &inEvent) {
    mElement->GetEventDispatcher()->DoEventTextChanged(inEvent);
}

void CustomTextCtrl::OnTextEnter(wxCommandEvent &inEvent) {
    mElement->GetEventDispatcher()->DoEventTextEnter(inEvent);
}


//=========================================================================
//  EditBox Methods
//=========================================================================

EditBox::EditBox(Stage *inStage, const wxString &inName, 
                 Halyard::TCallbackPtr inDispatch,
                 const wxRect &inBounds, const wxString inText,
                 uint32 inSize, bool inIsMultiline, bool inEnterIsEvent)
    : Widget(inStage, inName, inDispatch)
{
    // Figure out what flags to use.  We always pass wxTE_PROCESS_TAB, because
    // we're inside a wxFrame, not a wxDialog, and the default tab handling is
    // therefore useless.  We'll let Scheme sort out what to do.
    long style = wxTE_PROCESS_TAB | wxSIMPLE_BORDER; /* ...or wxNO_BORDER. */
    if (inIsMultiline)
        style |= wxTE_MULTILINE;
    // TODO - Enter processing in multiline controls needs some work.  It's
    // on by default (not only do we not need to pass wxTE_PROCESS_ENTER,
    // but wxWidgets will actually crash if we do), but that's probably not
    // what we want.
    if (inEnterIsEvent && !inIsMultiline)
        style |= wxTE_PROCESS_ENTER;

    // Create the actual control.
    mControl = new CustomTextCtrl(inStage, inText, inBounds, style, this);

    // Set up various options.
    wxFont font(inSize, wxROMAN, wxNORMAL, wxNORMAL);
    mControl->SetFont(font);
    //mControl->SetForegroundColour(...);
    //mControl->SetBackgroundColour(...);

    InitializeWidgetWindow(mControl);
}

wxString EditBox::GetValue() const {
    return mControl->GetValue();
}

void EditBox::SetValue(const wxString &inValue) {
    mControl->SetValue(inValue);
}

long EditBox::TranslatePosition(long pos) {
    // OK, the "-1 means end" convention is a bit arbitrary, but it's
    // supported by many scripting languages for array access.
    if (pos == -1)
        return mControl->GetLastPosition();
    else
        return pos;
}

void EditBox::SetInsertionPoint(long pos) {
    mControl->SetInsertionPoint(TranslatePosition(pos));
}

void EditBox::SetSelection(long begin, long end) {
    mControl->SetSelection(TranslatePosition(begin), TranslatePosition(end));
}
