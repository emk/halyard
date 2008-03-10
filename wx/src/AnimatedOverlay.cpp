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

#include "TamaleHeaders.h"
#include "FiveLApp.h"
#include "ImageCache.h"
#include "AnimatedOverlay.h"
#include "DrawingArea.h"
#include "CommonWxConv.h"
#include "TPoint.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL


AnimatedOverlay::AnimatedOverlay(Stage *inStage, const wxString &inName,
								 const wxRect &inBounds, 
								 FIVEL_NS TCallbackPtr inDispatch,
								 const std::string &inCursorName,
                                 bool inHasAlpha,
                                 const std::string &inStatePath, 
								 TValueList graphics)
     : Overlay(inStage, inName, inBounds, inDispatch, inCursorName,
               inHasAlpha),
       mBasePosition(inBounds.GetPosition()), mCurrentOffset(0, 0),
       mStatePath(inStatePath)
{
	// convert TValueList to GraphicsList
	TValueList::iterator iter = graphics.begin();
	for (; iter != graphics.end(); ++iter)
		mGraphics.push_back(*iter);
	
	NotifyStateChanged();
}

void AnimatedOverlay::MoveTo(const wxPoint &inPoint) {
    if (mBasePosition != inPoint) {
        mBasePosition = inPoint;
        UpdatePosition();
    }
}

void AnimatedOverlay::UpdatePosition() {
	Overlay::MoveTo(wxPoint(mBasePosition.x + mCurrentOffset.x,
                            mBasePosition.y + mCurrentOffset.y));
}

wxBitmap AnimatedOverlay::LoadPicture(const std::string &inName) {
	// Load our image.
	return wxGetApp().GetStage()->GetImageCache()->GetBitmap(inName.c_str());
}

void AnimatedOverlay::DrawGraphic(const std::string &inName) {
	// Load our image.
	wxBitmap bitmap(LoadPicture(inName));
	if (!bitmap.Ok()) {
		THROW("Error loading image for AnimatedOverlay");
	}
	
	// Draw our bitmap.
	DrawingArea *dc = GetDrawingArea();
	dc->Clear();
	dc->DrawBitmap(bitmap, 0, 0);
}

void AnimatedOverlay::NotifyStateChanged() {
    // Handle changes to our x,y co-ordinates.
	wxPoint offset(int32(gStateDB.Get(this, mStatePath + "/x")),
                   int32(gStateDB.Get(this, mStatePath + "/y")));
	if (offset != mCurrentOffset) {
        mCurrentOffset = offset;
        UpdatePosition();
    }

    // Handle changes to our graphic.
	int32 index = gStateDB.Get(this, mStatePath + "/index");
	if (index < 0 || static_cast<size_t>(index) >= mGraphics.size())
		THROW("Invalid index for AnimatedOverlay");
	std::string graphic = mGraphics.at(index);
    if (graphic != mCurrentGraphic) {
        mCurrentGraphic = graphic;
        DrawGraphic(graphic);
    }
}
