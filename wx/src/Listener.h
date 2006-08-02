// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#ifndef Listener_H
#define Listener_H

#include "ToolWindow.h"

//////////
/// A Listener allows you type in commands, and see their results in a
/// history window.  It's the traditional LISP term for any sort of
/// interactive command line for a running application.  In other systems,
/// it's also known as an "interactor" or "message box".
///
class Listener : public ToolWindow
{
	wxTextCtrl *mHistory;
	wxTextCtrl *mInput;

	wxFont mNormalFont;
	wxFont mBoldFont;

public:
	Listener(StageFrame *inStageFrame);

	void OnActivate(wxActivateEvent &inEvent);
	void UpdateUiInput(wxUpdateUIEvent &inEvent);
	void OnTextEnter(wxCommandEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // Listener_H
