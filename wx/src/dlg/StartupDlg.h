// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef StartupDlg_H
#define StartupDlg_H

//////////
// This is the dialog which pops up at startup time and asks you whether
// you want to open an existing program or create a new one.
//
class StartupDlg : public wxDialog
{
    DECLARE_EVENT_TABLE();
	
	wxRadioButton *mRadioRecent;
	wxRadioButton *mRadioOpen;
	wxRadioButton *mRadioNew;
	wxListBox *mRecentList;

	void OnOK(wxCommandEvent &inEvent);

protected:
	template <class T>
	Bind(T* &outVar, long inID)
	{ outVar = dynamic_cast<T*>(FindWindow(inID)); wxASSERT(outVar); }

public:
	StartupDlg(wxWindow *inParent);
};

#endif // StartupDlg_H

