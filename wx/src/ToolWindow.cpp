// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "Stage.h"
#include "ToolWindow.h"

BEGIN_EVENT_TABLE(ToolWindow, wxFrame)
    EVT_CLOSE(ToolWindow::OnClose)
END_EVENT_TABLE()

ToolWindow::ToolWindow(StageFrame *inStageFrame, ToolWindowID inID,
		       const wxString &inTitle)
    : wxFrame(inStageFrame, -1, inTitle), mStageFrame(inStageFrame), mID(inID)
{
    // Child objects will need to set the window size manually.
}

void ToolWindow::OnClose(wxCloseEvent &inEvent)
{
    if (inEvent.CanVeto())
    {
		// Since we're allowed to ignore this close event, simply cancel
		// the close event and hide the window.  This preserves the
		// window's contents longer than they'd otherwise last.
		inEvent.Veto();
		Hide();
    }
    else
    {
		// Let the stage know we're closing, and schedule the window for
		// deletion.
		mStageFrame->DetachToolWindow(mID);
		Destroy();
    }
}
