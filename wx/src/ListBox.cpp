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
#include "ListBox.h"
#include "EventDispatcher.h"
#include "HalyardApp.h"

using namespace Halyard;

//=========================================================================
//  CustomListBox Methods
//=========================================================================

class CustomListBox : public wxListBox {
    ListBox *mListBox;

    void OnListBox(wxCommandEvent &inEvent);
    void OnListBoxDClick(wxCommandEvent &inEvent);

    DECLARE_EVENT_TABLE();

public:
    CustomListBox(wxWindow *inParent, const wxRect &inBounds,
                  ListBox *inListBox);
};

BEGIN_EVENT_TABLE(CustomListBox, wxListBox)
    EVT_LISTBOX(wxID_ANY, CustomListBox::OnListBox)
    EVT_LISTBOX_DCLICK(wxID_ANY, CustomListBox::OnListBoxDClick)
END_EVENT_TABLE()

CustomListBox::CustomListBox(wxWindow *inParent, const wxRect &inBounds,
                             ListBox *inListBox)
    : wxListBox(inParent, wxID_ANY, inBounds.GetPosition(),
                inBounds.GetSize()),
      mListBox(inListBox)
{
}

void CustomListBox::OnListBox(wxCommandEvent &inEvent) {
    mListBox->GetEventDispatcher()->DoEventItemSelected(inEvent);
}

void CustomListBox::OnListBoxDClick(wxCommandEvent &inEvent) {
    mListBox->GetEventDispatcher()->DoEventItemDoubleClick(inEvent);
}


//=========================================================================
//  ListBox Methods
//=========================================================================

ListBox::ListBox(const wxString &inName, 
                 Halyard::TCallbackPtr inDispatch,
                 const wxRect &inBounds)
    : Widget(inName, inDispatch)
{
    mListBox = new CustomListBox(wxGetApp().GetStage(), inBounds, this);
    InitializeWidgetWindow(mListBox);
}

void ListBox::AddItem(const wxString &inValue) {
    mListBox->Append(inValue);
}

void ListBox::GetSelection(wxArrayInt &outSelection) const {
    mListBox->GetSelections(outSelection);
}
