// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TInterpreter.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "Stage.h"
#include "Listener.h"

USING_NAMESPACE_FIVEL

BEGIN_EVENT_TABLE(Listener, ToolWindow)
    EVT_ACTIVATE(Listener::OnActivate)
    EVT_UPDATE_UI(FIVEL_LISTENER_TEXT_ENTRY, Listener::UpdateUiInput)
    EVT_TEXT_ENTER(FIVEL_LISTENER_TEXT_ENTRY, Listener::OnTextEnter)
END_EVENT_TABLE()

Listener::Listener(StageFrame *inStageFrame)
    : ToolWindow(inStageFrame, TOOL_LISTENER, "Listener", wxICON(ic_listener))
{
    mHistory = new wxTextCtrl(this, -1, "", wxDefaultPosition,
							  wxDefaultSize,
							  wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);
    mInput = new wxTextCtrl(this, FIVEL_LISTENER_TEXT_ENTRY, "",
							wxDefaultPosition, wxDefaultSize,
							wxTE_PROCESS_ENTER);

    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(mHistory, 1 /* stretch */, wxGROW, 0);
    sizer->Add(mInput, 0 /* don't stretch */, wxGROW, 0);
    SetSizer(sizer);
    sizer->SetSizeHints(this);

    // Make copies of the history widget's default font (to avoid the
    // reference counting system) and set the default weight of one to
    // bold.  We'll use these for the listener output.
    wxFont f = mHistory->GetFont();
    mNormalFont = wxFont(f.GetPointSize(), f.GetFamily(), f.GetStyle(),
						 f.GetWeight(), f.GetUnderlined(), f.GetFaceName(),
						 f.GetDefaultEncoding());
    mBoldFont = wxFont(f.GetPointSize(), f.GetFamily(), f.GetStyle(),
					   wxBOLD, f.GetUnderlined(), f.GetFaceName(),
					   f.GetDefaultEncoding());

    SetClientSize(640, 240);
    mInput->SetFocus();
}

void Listener::OnActivate(wxActivateEvent &inEvent)
{
    // When we're raised to be the top-most window, assume it's because
    // the user wanted to type something into the input box.
    // TODO - This doesn't work if the user raises the window by clicking
    // in the history, because we get the activate event and then the
    // click.
    mInput->SetFocus();
}

void Listener::UpdateUiInput(wxUpdateUIEvent &inEvent)
{
    inEvent.Enable(TInterpreter::HaveInstance());    
}

void Listener::OnTextEnter(wxCommandEvent &inEvent)
{
    if (inEvent.GetString() == "")
		inEvent.Skip();
    else
    {
		ASSERT(TInterpreter::HaveInstance());

		// Print the user's input.
		wxString input = inEvent.GetString();
		mHistory->SetDefaultStyle(wxTextAttr(*wxBLACK, wxNullColour,
											 mBoldFont));
		mHistory->AppendText(input + "\n");
		
		// Talk to the interpreter.
		std::string result;
		bool ok =
			TInterpreter::GetInstance()->Eval((const char *) input, result);
		
		// Print the interpreter's output.
		if (ok)
		{
			mHistory->SetDefaultStyle(wxTextAttr(*wxBLUE, wxNullColour,
												 mNormalFont));
			mHistory->AppendText("==> " + wxString(result.c_str()) + "\n\n");
		}
		else
		{
			mHistory->SetDefaultStyle(wxTextAttr(*wxRED, wxNullColour,
												 mNormalFont));
			mHistory->AppendText("ERROR: " + wxString(result.c_str()) +
								 "\n\n");
		}
		
		// Clear our input field.
		mInput->SetValue("");
    }
}
