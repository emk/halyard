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

#ifndef FancyStatusBar_H
#define FancyStatusBar_H

class ProgressMeter;

/// Custom wxStatusBar subclass used by StageFrame window.
class FancyStatusBar : public wxStatusBar
{
	ProgressMeter *mProgressMeter;
	
    void OnSize(wxSizeEvent& event);

public:
	FancyStatusBar(wxWindow *inParent);

	static wxColour DEFAULT_PROGRESS_COLOR;

	void SetProgressColor(wxColour &inColor);
	void SetProgress(float inValue);

    DECLARE_EVENT_TABLE()
};

#endif // FancyStatusBar_H
