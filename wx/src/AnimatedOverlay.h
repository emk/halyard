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

#ifndef AnimatedOverlay_H
#define AnimatedOverlay_H

#include "Overlay.h"

/// An Overlay which moves and changes appearance automatically.
/// The changes are controlled by keys in the TStateDB.
class AnimatedOverlay : public Overlay, public FIVEL_NS TStateListener {
    wxPoint mBasePosition;
    wxPoint mCurrentOffset;
    std::string mCurrentGraphic;

	typedef std::vector<std::string> GraphicsList;
	GraphicsList mGraphics;
	std::string mStatePath;
	
    void UpdatePosition();
	wxBitmap LoadPicture(const std::string &inName);
	void DrawGraphic(const std::string &inName);
	
public:
	AnimatedOverlay(Stage *inStage, const wxString &inName, 
                    const wxRect &inBounds, 
                    FIVEL_NS TCallbackPtr inDispatch,
                    wxCursor &inCursor,
                    bool inHasAlpha,
                    std::string &inStatePath, 
                    FIVEL_NS TValueList graphics);

    virtual void MoveTo(const wxPoint &inPoint);
	virtual void NotifyStateChanged();
};

#endif // AnimatedOverlay_H
