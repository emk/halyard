// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

//
// This class implements a browser-style "Location:" box which can be used
// to jump between cards.  We implement it slightly differently on different
// platforms because we occasionally need to abuse the semantics of the
// underlying native control (or to abuse the wxWindows window class wrapping
// that control).

#ifndef LocationBox_H
#define LocationBox_H

#include "AppConfig.h"

#if CONFIG_LOCATION_BOX_IS_COMBO
#	define LOCATION_BOX_PARENT_CLASS wxComboBox
#else
#	define LOCATION_BOX_PARENT_CLASS wxTextCtrl
#endif

/// The browser-like text-entry box on the StageFrame.
class LocationBox : public LOCATION_BOX_PARENT_CLASS
{
public:
	LocationBox(wxToolBar *inParent);
	
	void NotifyEnterCard(const wxString &inName);

	void RegisterCard(const wxString &inCardName);
	void TryJump(const wxString &inCardName);

	void UpdateUiLocationBox(wxUpdateUIEvent &inEvent);
	void OnChar(wxKeyEvent &inEvent);

#if CONFIG_LOCATION_BOX_IS_COMBO
	void OnComboBoxSelected(wxCommandEvent &inEvent);
#endif

	//////////
	/// Call this function to focus the location box and prepare for the
	/// user to enter a card name.
	///
	void Prompt();

	DECLARE_EVENT_TABLE();
};

#endif // LocationBox_H
