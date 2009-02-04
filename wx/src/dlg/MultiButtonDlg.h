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

#ifndef MultiButtonDlg_H
#define MultiButtonDlg_H

#include "XrcDlg.h"

//////////
/// This dialog shows the specified title, messages, and buttons.
///
class MultiButtonDlg : public XrcDlg
{
    DECLARE_EVENT_TABLE();
	
	wxStaticText *mMessage;
	wxButton *mButton1;
	wxButton *mButton2;
	wxButton *mButton3;

    void LabelOrHideButton(wxButton *inButton, const wxString &inLabel);
	void OnButton(wxCommandEvent &inEvent);

public:
	MultiButtonDlg(wxWindow *inParent, const wxString &inTitle,
                   const wxString &inMessage, const wxString &inButton1,
                   const wxString &inButton2 = "",
                   const wxString &inButton3 = "");
};

#endif // MultiButtonDlg_H
