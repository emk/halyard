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

#include "AppGlobals.h"
#include "LocationBox.h"
#include "TInterpreter.h"

// For jumping to cards.
#include "HalyardApp.h"
#include "Stage.h"

using namespace Halyard;


//=========================================================================
//  LocationBox Methods
//=========================================================================

BEGIN_EVENT_TABLE(LocationBox, LOCATION_BOX_PARENT_CLASS)
    EVT_UPDATE_UI(HALYARD_LOCATION_BOX, LocationBox::UpdateUiLocationBox)
	EVT_CHAR(LocationBox::OnChar)

// We only process this event if we're a ComboBox.
#if CONFIG_LOCATION_BOX_IS_COMBO
	EVT_COMBOBOX(HALYARD_LOCATION_BOX, LocationBox::OnComboBoxSelected)
#endif

END_EVENT_TABLE()

#if CONFIG_LOCATION_BOX_IS_COMBO

LocationBox::LocationBox(wxToolBar *inParent)
	: wxComboBox(inParent, HALYARD_LOCATION_BOX, "",
				 wxDefaultPosition, wxSize(200, -1),
				 0, NULL, wxWANTS_CHARS|wxCB_DROPDOWN)
{
}

#else // !CONFIG_LOCATION_BOX_IS_COMBO

LocationBox::LocationBox(wxToolBar *inParent)
	: wxTextCtrl(inParent, HALYARD_LOCATION_BOX, "", wxDefaultPosition,
				 wxSize(200, -1), wxTE_PROCESS_ENTER)
{
}

#endif // !CONFIG_LOCATION_BOX_IS_COMBO

void LocationBox::NotifyEnterCard(const wxString &inName)
{
	SetValue(inName);
	RegisterCard(inName);
}

void LocationBox::RegisterCard(const wxString &inCardName)
{
#if CONFIG_LOCATION_BOX_IS_COMBO
	// Update our drop-down list of cards.
	// First, delete the card from the list
	int loc = FindString(inCardName);
	std::vector<wxString> vec;
	int i;

	if (loc != wxNOT_FOUND)
		Delete(loc);	

	// Now move all of the remaining cards into a vector
	for (i = GetCount() - 1; i >= 0; i--) {
		vec.push_back(GetString(i));
		Delete(i);
	}

	// Add our new card at the front of the list
	Append(inCardName);

	// Put back all of the remaining cards
	for (i = vec.size() - 1; i >= 0; i--) 
		Append(vec[i]);
#endif // CONFIG_LOCATION_BOX_IS_COMBO
}

void LocationBox::TryJump(const wxString &inCardName)
{
	Stage *stage = wxGetApp().GetStage();
    if (stage->CanJump())
	{
		// If the specified card exists, add it to our list.
		if (TInterpreter::GetInstance()->IsValidCard(inCardName))
			RegisterCard(inCardName);

		// Jump to the specified card, or display an error if it
		// does not exist.
		stage->TryJumpTo(inCardName);
	}
}

void LocationBox::UpdateUiLocationBox(wxUpdateUIEvent &inEvent)
{
	// We need to check for a stage because we may be called after the
	// Stage is unregistered.
	if (wxGetApp().HaveStage())
		inEvent.Enable(wxGetApp().GetStage()->CanJump());
}

void LocationBox::OnChar(wxKeyEvent &inEvent)
{
	if (inEvent.GetKeyCode() == WXK_RETURN)
		TryJump(GetValue());
	else
		inEvent.Skip();
}

#if CONFIG_LOCATION_BOX_IS_COMBO

void LocationBox::OnComboBoxSelected(wxCommandEvent &inEvent)
{
	TryJump(inEvent.GetString());
}

#endif // CONFIG_LOCATION_BOX_IS_COMBO

void LocationBox::Prompt()
{
	SetFocus();
}
