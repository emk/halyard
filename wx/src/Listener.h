// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Listener_H
#define Listener_H

class StageFrame;

//////////
// A Listener allows you type in commands, and see their results in a
// history window.  It's the traditional LISP term for any sort of
// interactive command line for a running application.  In other systems,
// it's also known as an "interactor" or "message box".
//
class Listener : public wxFrame
{
	StageFrame *mStageFrame;

	wxTextCtrl *mHistory;
	wxTextCtrl *mInput;

	wxFont mNormalFont;
	wxFont mBoldFont;

public:
	Listener(StageFrame *inStageFrame);

	void OnActivate(wxActivateEvent &inEvent);
	void UpdateUiInput(wxUpdateUIEvent &inEvent);
	void OnTextEnter(wxCommandEvent &inEvent);
	void OnClose(wxCloseEvent &inEvent);
	
    DECLARE_EVENT_TABLE();
};

#endif // Listener_H
