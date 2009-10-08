// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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
#include "PopUpMenu.h"
#include "EventDispatcher.h"
#include "HalyardApp.h"

using namespace Halyard;


//=========================================================================
//  CustomChoice Methods
//=========================================================================

class CustomChoice : public wxChoice {
    PopUpMenu *mPopUpMenu;

    void OnChoice(wxCommandEvent &inEvent);

    DECLARE_EVENT_TABLE();

public:
    CustomChoice(wxWindow *inParent, const wxRect &inBounds,
                 PopUpMenu *inPopUpMenu);
};

BEGIN_EVENT_TABLE(CustomChoice, wxChoice)
    EVT_CHOICE(wxID_ANY, CustomChoice::OnChoice)
END_EVENT_TABLE()

CustomChoice::CustomChoice(wxWindow *inParent, const wxRect &inBounds,
                           PopUpMenu *inPopUpMenu)
    : wxChoice(inParent, wxID_ANY, inBounds.GetPosition(),
               inBounds.GetSize()),
      mPopUpMenu(inPopUpMenu)
{
}

void CustomChoice::OnChoice(wxCommandEvent &inEvent) {
    mPopUpMenu->GetEventDispatcher()->DoEventItemSelected(inEvent);
}


//=========================================================================
//  PopUpMenu Methods
//=========================================================================

PopUpMenu::PopUpMenu(const wxString &inName, 
                     Halyard::TCallbackPtr inDispatch,
                     const wxRect &inBounds)
    : Widget(inName, inDispatch)
{
    mChoice = new CustomChoice(wxGetApp().GetStage(), inBounds, this);
    InitializeWidgetWindow(mChoice);
}

void PopUpMenu::AddItem(const wxString &inValue) {
    mChoice->Append(inValue);
}

int PopUpMenu::GetSelection() const {
    return mChoice->GetSelection();
}
