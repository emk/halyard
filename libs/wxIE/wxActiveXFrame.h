#ifndef wxActiveXFrame_h
#define wxActiveXFrame_h

#include "wxactivex.h"

class wxActiveXFrame : public wxFrame
{
public:
    wxActiveX *X;
    wxTextCtrl *textLog;

    wxActiveXFrame(wxWindow *parent, wxString title);

    DECLARE_EVENT_TABLE()

    void OnGetTypeInfo(wxCommandEvent& event);
    void OnInvokeMethod(wxCommandEvent& event);
	void OnTest(wxCommandEvent& event);
    void OnActiveXEvent(wxActiveXEvent& event);
};


#endif