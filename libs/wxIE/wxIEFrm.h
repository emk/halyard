#include "IEHtmlWin.h"
#include "wx/gauge.h"

// Define a new frame type: this is going to be our main frame
class wxIEFrame : public wxFrame
{
public:
	wxIEHtmlWin	*m_ie;
	wxGauge *m_gauge;

    // ctor(s)
    wxIEFrame(const wxString& title, const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize);

    // event handlers (these functions should _not_ be virtual)
	void OnSize(wxSizeEvent& event);
    void OnQuit(wxCommandEvent& event);
    void OnAbout(wxCommandEvent& event);

    void OnEditMode(wxCommandEvent& event);
    void OnEditModeUI(wxUpdateUIEvent& event);
	void OnBrowse(wxCommandEvent& event);
	void OnOpen(wxCommandEvent& event);
    void OnTestHTML(wxCommandEvent& event);
    void OnTestSelect(wxCommandEvent& event);
    void OnTestHTMLSelect(wxCommandEvent& event);
    void OnTestGetText(wxCommandEvent& event);
    void OnTestHTMLGetText(wxCommandEvent& event);
    void OnTestHome(wxCommandEvent& event);
    void OnTestActiveX(wxCommandEvent& event);

private:
    // any class wishing to process wxWindows events must use this macro
    DECLARE_EVENT_TABLE()

    void OnMSHTMLStatusTextChangeX(wxActiveXEvent& event);
    void OnMSHTMLBeforeNavigate2X(wxActiveXEvent& event);
    void OnMSHTMLTitleChangeX(wxActiveXEvent& event);
	void OnMSHTMLNewWindow2X(wxActiveXEvent& event);
    void OnMSHTMLProgressChangeX(wxActiveXEvent& event);
};
