// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "AppHeaders.h"

#include "TInterpreter.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "Stage.h"
#include "HistoryText.h"
#include "Listener.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(Listener, ToolWindow)
    EVT_ACTIVATE(Listener::OnActivate)
    EVT_UPDATE_UI(HALYARD_LISTENER_TEXT_ENTRY, Listener::UpdateUiInput)
    EVT_TEXT_ENTER(HALYARD_LISTENER_TEXT_ENTRY, Listener::OnTextEnter)
END_EVENT_TABLE()

Listener::Listener(StageFrame *inStageFrame)
    : ToolWindow(inStageFrame, TOOL_LISTENER, wxT("Listener"),
                 wxICON(ic_listener))
{
    mHistory = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition,
							  wxDefaultSize,
							  wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);
	// Use a history text control, so we can have a command history
    mInput = new HistoryTextCtrl(this, HALYARD_LISTENER_TEXT_ENTRY, wxT(""),
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
    if (inEvent.GetString() == wxT(""))
		inEvent.Skip();
    else
    {
		ASSERT(TInterpreter::HaveInstance());

		// Print the user's input.
		wxString input = inEvent.GetString();
		mHistory->SetDefaultStyle(wxTextAttr(*wxBLACK, wxNullColour,
											 mBoldFont));
		mHistory->AppendText(input + wxT("\n"));
	
		// Talk to the interpreter.
		std::string result;
		bool ok =
			TInterpreter::GetInstance()->Eval(std::string(input.mb_str()),
                                              result);
		
		// Print the interpreter's output.
		if (ok)
		{
			mHistory->SetDefaultStyle(wxTextAttr(*wxBLUE, wxNullColour,
												 mNormalFont));
			mHistory->AppendText(wxT("==> ") +
                                 wxString(result.c_str(), wxConvLocal) +
                                 wxT("\n\n"));
		}
		else
		{
			mHistory->SetDefaultStyle(wxTextAttr(*wxRED, wxNullColour,
												 mNormalFont));
			mHistory->AppendText(wxT("ERROR: ") +
                                 wxString(result.c_str(), wxConvLocal) +
								 wxT("\n\n"));
		}
		
		// Clear our input field.
		mInput->SetValue(wxT(""));
    }
}
