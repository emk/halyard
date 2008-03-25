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
#include "MultiButtonDlg.h"

USING_NAMESPACE_FIVEL

BEGIN_EVENT_TABLE(MultiButtonDlg, XrcDlg)
	EVT_BUTTON(XRCID("ID_BUTTON1"), MultiButtonDlg::OnButton)
	EVT_BUTTON(XRCID("ID_BUTTON2"), MultiButtonDlg::OnButton)
	EVT_BUTTON(XRCID("ID_BUTTON3"), MultiButtonDlg::OnButton)
END_EVENT_TABLE()

MultiButtonDlg::MultiButtonDlg(wxWindow *inParent, const wxString &inTitle,
                               const wxString &inMessage,
                               const wxString &inButton1,
                               const wxString &inButton2,
                               const wxString &inButton3)
	: XrcDlg(inParent, "DLG_MULTIBUTTON")
{
    wxASSERT(inTitle != "");
    wxASSERT(inMessage != "");
    wxASSERT(inButton1 != "");

    Bind(mMessage, XRCID("ID_MESSAGE"));
	Bind(mButton1, XRCID("ID_BUTTON1"));
	Bind(mButton2, XRCID("ID_BUTTON2"));
	Bind(mButton3, XRCID("ID_BUTTON3"));

	// Fill in our various fields.
    SetTitle(inTitle);
    mMessage->SetLabel(inMessage);
    mButton1->SetLabel(inButton1);
    LabelOrHideButton(mButton2, inButton2);
    LabelOrHideButton(mButton3, inButton3);

    // XXX - For some dumb reason, this dialog has an overlay-large
    // minimum size. It's not currently a priority to figure out
    // what's going on.
}

void MultiButtonDlg::LabelOrHideButton(wxButton *inButton,
                                       const wxString &inLabel)
{
    if (inLabel != "")
        inButton->SetLabel(inLabel);
    else
        inButton->Hide();
}

void MultiButtonDlg::OnButton(wxCommandEvent &inEvent)
{
	Hide();

    int id = inEvent.GetId();
    int result = 0;
    if (id == XRCID("ID_BUTTON1"))
        result = 1;
    if (id == XRCID("ID_BUTTON2"))
        result = 2;
    if (id == XRCID("ID_BUTTON3"))
        result = 3;
    ASSERT(result);

	EndModal(result);
}
