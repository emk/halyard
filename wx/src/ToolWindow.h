// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef ToolWindow_H
#define ToolWindow_H

#include "StageFrame.h"

//////////
// A ToolWindow is a unique, named window associated with a given
// StageFrame.  Examples include the Listener and the Timecoder.
// Some of the support code for ToolWindows is centralized to keep
// us from reinventing any more wheels than necessary, but feel free
// to tweak this class a bit to support more exotic types of ToolWindows
// when necessary.
//
// When closed, ToolWindows try to hide themselves (instead of destroying
// themselves, which is the default behavior).  This allows them to
// maintain their position and contents during an editing session without
// an undue amount of work.
//
class ToolWindow : public wxFrame
{
	StageFrame *mStageFrame;
	ToolWindowID mID;

protected:
	//////////
	// Create a new ToolWindow.  When you done with the constructor,
	// you'll want to assign an appropriate size to this window.
	//
	// [in] inStageFrame - The StageFrame for this tool
	// [in] inID - The ID of this ToolWindow.  See StageFrame for
	//             a list of ToolWindowID values.
	// [in] inTitle - The user-visible title of this window.
	//
	ToolWindow(StageFrame *inStageFrame, ToolWindowID inID,
			   const wxString &inTitle, const wxIcon &inIcon);

public:
	//////////
	// wxWindows event handler--do not call manually.
	//
	void OnClose(wxCloseEvent &inEvent);
	
    DECLARE_EVENT_TABLE();
};

#endif // ToolWindow_H
