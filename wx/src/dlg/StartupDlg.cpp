// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "../FiveLApp.h"
#include "../StageFrame.h"
#include "StartupDlg.h"

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

