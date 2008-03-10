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
#include "../FiveLApp.h"
#include "../StageFrame.h"
#include "StartupDlg.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(StartupDlg, XrcDlg)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_NEW"), StartupDlg::OnNew)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_OPEN"), StartupDlg::OnOpen)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_RECENT"), StartupDlg::OnRecent)
	EVT_BUTTON(wxID_OK, StartupDlg::OnOK)
	EVT_BUTTON(wxID_CANCEL, wxDialog::OnCancel)
END_EVENT_TABLE()

StartupDlg::StartupDlg(wxWindow *inParent)
	: XrcDlg(inParent, "DLG_STARTUP")
{
	Bind(mRadioNew, XRCID("DLG_STARTUP_NEW"));
	Bind(mRadioOpen, XRCID("DLG_STARTUP_OPEN"));
	Bind(mRadioRecent, XRCID("DLG_STARTUP_RECENT"));
	Bind(mRecentList, XRCID("DLG_STARTUP_LIST"));

    // XXX - HACK - We need to SetFocus here to prevent later
    // calls to SetFocus from selecting mRadioNew as the default
    // button.  Presumably this bug will get fixed in wxWindows.
    //
    // https://sourceforge.net/tracker/index.php?func=detail&aid=992497&group_id=9863&atid=109863
    mRadioOpen->SetFocus();
	mRadioOpen->SetValue(true);

    /// \todo Turn "new program" back on when it works better.
    mRadioNew->Disable();

	// TODO - Implement the recent documents list.
	mRadioRecent->Disable();
	mRecentList->Disable();
}

void StartupDlg::OnOK(wxCommandEvent &inEvent)
{
	Hide();

	BEGIN_EXCEPTION_TRAPPER()
		if (mRadioNew->GetValue())
			wxGetApp().GetStageFrame()->NewDocument();
		else if (mRadioOpen->GetValue())
			wxGetApp().GetStageFrame()->OpenDocument();
	END_EXCEPTION_TRAPPER(TException::ReportException)
	
	EndModal(wxID_OK);
}

