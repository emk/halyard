// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"
#include "Stage.h"
#include "ToolWindow.h"

BEGIN_EVENT_TABLE(ToolWindow, wxFrame)
    EVT_CLOSE(ToolWindow::OnClose)
END_EVENT_TABLE()

ToolWindow::ToolWindow(StageFrame *inStageFrame, ToolWindowID inID,
		       const wxString &inTitle, const wxIcon &inIcon)
    : wxFrame(inStageFrame, -1, inTitle), mStageFrame(inStageFrame), mID(inID)
{
	SetIcon(inIcon);

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
