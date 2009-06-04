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

#ifndef StartupDlg_H
#define StartupDlg_H

#include "XrcDlg.h"
#include <wx/docview.h>

//////////
/// This is the dialog which pops up at startup time and asks you whether
/// you want to open an existing program or create a new one.
///
class StartupDlg : public XrcDlg
{
    DECLARE_EVENT_TABLE();
	
    wxFileHistory mRecentFiles;
	wxRadioButton *mRadioRecent;
	wxRadioButton *mRadioOpen;
	wxRadioButton *mRadioNew;
	wxListBox *mRecentList;

	void OnOK(wxCommandEvent &inEvent);

public:
	StartupDlg(wxWindow *inParent);
};

#endif // StartupDlg_H
