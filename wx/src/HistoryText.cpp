// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "HistoryText.h"

USING_NAMESPACE_FIVEL

BEGIN_EVENT_TABLE(HistoryTextCtrl, wxTextCtrl)
	EVT_KEY_DOWN(HistoryTextCtrl::OnKeyDown)
	EVT_TEXT_ENTER(wxID_ANY, HistoryTextCtrl::OnTextEnter)
END_EVENT_TABLE()

HistoryTextCtrl::HistoryTextCtrl(wxWindow* parent, 
								 wxWindowID id, 
								 const wxString& value, 
								 const wxPoint& pos, 
								 const wxSize& size, 
								 long style, 
								 const wxValidator& validator, 
								 const wxString& name)

	: wxTextCtrl(parent, id, value, pos, size, style, validator, name) 
{
	// Set up the initial command history. At first, the command history
	// consists of the initial value of the first command.
	mHistoryItems.push_back(value);
	mHistoryCurrent = 0;
}

// Handle the keystrokes used to navigate through the history
void HistoryTextCtrl::OnKeyDown(wxKeyEvent &inEvent)
{
	int code = inEvent.GetKeyCode();
	if (code == WXK_UP)
		HistPrev();	
	else if (code == WXK_DOWN)
		HistNext();
	else
		inEvent.Skip();
}

// Handle entering a value
void HistoryTextCtrl::OnTextEnter(wxCommandEvent &inEvent)
{
    if (inEvent.GetString() == "")
		inEvent.Skip();
    else
    {
		wxString input = inEvent.GetString();
		// Save the item in our command history.
		mHistoryItems[mHistoryItems.size() - 1] = input;

		// Prepare next item in command history
		mHistoryItems.push_back(wxString(""));
		mHistoryCurrent = mHistoryItems.size() - 1;

		inEvent.Skip();
	}
}
	

// History manipulation functions.
void HistoryTextCtrl::SaveCurrHist()
{
	mHistoryItems[mHistoryCurrent] = GetValue();
}

void HistoryTextCtrl::DisplayCurrHist()
{
	SetValue(mHistoryItems[mHistoryCurrent]);
	SetInsertionPointEnd();
}

void HistoryTextCtrl::HistPrev() 
{
	if (mHistoryCurrent > 0)
	{	
		// Note: This uses Bash's semantics, which are to save the current
		// value of the history item you are editing when you go to another
		// line. The probably more correct behavior would be to have two 
		// copies of the history vector, one which was the actual history,
		// and the other of which was your current working copy. When you 
		// hit enter, your current working copy would be replaced with the
		// historical copy. Feel free to implement this if it floats your
		// boat.
		SaveCurrHist();
		mHistoryCurrent--;
		DisplayCurrHist();
	}
}

void HistoryTextCtrl::HistNext() 
{
	if (mHistoryCurrent < mHistoryItems.size() - 1)
	{	
		SaveCurrHist();
		mHistoryCurrent++;
		DisplayCurrHist();
	}
}
