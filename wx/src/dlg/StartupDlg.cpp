// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/xrc/xmlres.h>

#include "TCommon.h"
#include "TException.h"

#include "../FiveLApp.h"
#include "../Stage.h"
#include "StartupDlg.h"

BEGIN_EVENT_TABLE(StartupDlg, wxDialog)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_NEW"), StartupDlg::OnNew)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_OPEN"), StartupDlg::OnOpen)
	//EVT_RADIOBUTTON(XRCID("DLG_STARTUP_RECENT"), StartupDlg::OnRecent)
	EVT_BUTTON(wxID_OK, StartupDlg::OnOK)
	EVT_BUTTON(wxID_CANCEL, wxDialog::OnCancel)
END_EVENT_TABLE()

StartupDlg::StartupDlg(wxWindow *inParent)
{
	wxTheXmlResource->LoadDialog(this, inParent, "DLG_STARTUP");
	Bind(mRadioNew, XRCID("DLG_STARTUP_NEW"));
	Bind(mRadioOpen, XRCID("DLG_STARTUP_OPEN"));
	Bind(mRadioRecent, XRCID("DLG_STARTUP_RECENT"));
	Bind(mRecentList, XRCID("DLG_STARTUP_LIST"));

	mRadioOpen->SetValue(true);

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
	END_EXCEPTION_TRAPPER()
	
	EndModal(wxID_OK);
}

