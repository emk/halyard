// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef HistoryText_H
#define HistoryText_H

//////////
// A HistoryText control is a wxTextCtrl. It implements a command history 
// along the lines of Bash. Each time you hit "RETURN", what you're 
// currently typing is added to the history, which can be navigated using 
// the arrow keys or CTRL-P and CTRL-N.
//
class HistoryTextCtrl : public wxTextCtrl
{
	std::vector<wxString> mHistoryItems;
	int mHistoryCurrent;

public:
	HistoryTextCtrl(wxWindow* parent, 
					wxWindowID id, 
					const wxString& value = "", 
					const wxPoint& pos = wxDefaultPosition, 
					const wxSize& size = wxDefaultSize, 
					long style = 0, 
					const wxValidator& validator = wxDefaultValidator,
					const wxString& name = wxTextCtrlNameStr);
		
	void OnKeyDown(wxKeyEvent &inEvent);
	void OnTextEnter(wxCommandEvent &inEvent);

	void SaveCurrHist();
	void DisplayCurrHist();
	void HistPrev();
	void HistNext();
	
	DECLARE_EVENT_TABLE();
};

#endif // HistoryText_H
