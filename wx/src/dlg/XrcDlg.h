// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef XrcDlg_H
#define XrcDlg_H

//////////
// This abstract class displays a dialog defined in our XRC resources file.
//
class XrcDlg : public wxDialog
{
    DECLARE_EVENT_TABLE();
    
protected:
	XrcDlg(wxWindow *inParent, const wxString &inResourceName);

    template <class T>
	void Bind(T* &outVar, long inID)
	{ outVar = dynamic_cast<T*>(FindWindow(inID)); wxASSERT(outVar); }
};


#endif // XrcDlg_H
