// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

    void OnTextEnter(wxCommandEvent &inEvent);

public:
    CustomTextCtrl(wxWindow *inParent, const wxString &inValue,
                   const wxRect &inRect, long inStyle,
                   EditBox *inElement);

    DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(CustomTextCtrl, wxTextCtrl)
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

void CustomTextCtrl::OnTextEnter(wxCommandEvent &inEvent) {
    mElement->GetEventDispatcher()->DoEventTextEnter(inEvent);
}


//=========================================================================
//  EditBox Methods
//=========================================================================

EditBox::EditBox(Stage *inStage, const wxString &inName, 
                 FIVEL_NS TCallbackPtr inDispatch,
                 const wxRect &inBounds, const wxString inText,
                 uint32 inSize, bool inIsMultiline, bool inEnterIsEvent)
    : Widget(inStage, inName, inDispatch)
{
    // Figure out what flags to use.
    long style = wxSIMPLE_BORDER; /* ...or wxNO_BORDER. */
    if (inIsMultiline)
        style |= wxTE_MULTILINE;
    if (inEnterIsEvent)
        style |= wxTE_PROCESS_ENTER;

    // Create the actual control.
    mControl = new CustomTextCtrl(inStage, inText, inBounds, style, this);

    // Set up various options.
    wxFont font(inSize, wxROMAN, wxNORMAL, wxNORMAL);
    mControl->SetFont(font);
    //mControl->SetForegroundColour(...);
    //mControl->SetBackgroundColour(...);
    mControl->SetFocus();

    InitializeWidgetWindow(mControl);
}

wxString EditBox::GetValue() const {
    return mControl->GetValue();
}
