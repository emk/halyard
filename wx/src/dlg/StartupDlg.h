// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef StartupDlg_H
#define StartupDlg_H

#include "XrcDlg.h"

//////////
// This is the dialog which pops up at startup time and asks you whether
// you want to open an existing program or create a new one.
//
class StartupDlg : public XrcDlg
{
    DECLARE_EVENT_TABLE();
	
	wxRadioButton *mRadioRecent;
	wxRadioButton *mRadioOpen;
	wxRadioButton *mRadioNew;
	wxListBox *mRecentList;

	void OnOK(wxCommandEvent &inEvent);

public:
	StartupDlg(wxWindow *inParent);
};

#endif // StartupDlg_H
