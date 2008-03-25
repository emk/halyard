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

#ifndef ToolWindow_H
#define ToolWindow_H

#include "StageFrame.h"

//////////
/// A ToolWindow is a unique, named window associated with a given
/// StageFrame.  Examples include the Listener and the Timecoder.
/// Some of the support code for ToolWindows is centralized to keep
/// us from reinventing any more wheels than necessary, but feel free
/// to tweak this class a bit to support more exotic types of ToolWindows
/// when necessary.
///
/// When closed, ToolWindows try to hide themselves (instead of destroying
/// themselves, which is the default behavior).  This allows them to
/// maintain their position and contents during an editing session without
/// an undue amount of work.
///
class ToolWindow : public wxFrame
{
	StageFrame *mStageFrame;
	ToolWindowID mID;

protected:
	//////////
	/// Create a new ToolWindow.  When you done with the constructor,
	/// you'll want to assign an appropriate size to this window.
	///
	/// \param inStageFrame  The StageFrame for this tool
	/// \param inID  The ID of this ToolWindow.  See StageFrame for
	///             a list of ToolWindowID values.
	/// \param inTitle  The user-visible title of this window.
	///
	ToolWindow(StageFrame *inStageFrame, ToolWindowID inID,
			   const wxString &inTitle, const wxIcon &inIcon);

public:
	//////////
	/// wxWindows event handler--do not call manually.
	///
	void OnClose(wxCloseEvent &inEvent);
	
    DECLARE_EVENT_TABLE();
};

#endif // ToolWindow_H
