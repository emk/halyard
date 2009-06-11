// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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
#include "HalyardApp.h"
#include "ImageCache.h"
#include "AnimatedOverlay.h"
#include "DrawingArea.h"
#include "CommonWxConv.h"
#include "TPoint.h"
#include "Stage.h"

using namespace Halyard;


AnimatedOverlay::AnimatedOverlay(Stage *inStage, const wxString &inName,
                                 const wxRect &inBounds, 
                                 Halyard::TCallbackPtr inDispatch,
                                 const std::string &inCursorName,
                                 bool inHasAlpha,
                                 const std::string &inStatePath, 
                                 TValueList images)
     : Overlay(inStage, inName, inBounds, inDispatch, inCursorName,
               inHasAlpha),
       mBasePosition(inBounds.GetPosition()), mCurrentOffset(0, 0),
       mStatePath(inStatePath)
{
    // convert TValueList to ImageList
    TValueList::iterator iter = images.begin();
    for (; iter != images.end(); ++iter)
        mImages.push_back(tvalue_cast<std::string>(*iter));
    
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

CairoSurfacePtr AnimatedOverlay::LoadImage(const std::string &inName) {
    // Load our image.
    wxString name(inName.c_str(), wxConvLocal);
    return wxGetApp().GetStage()->GetImageCache()->GetImage(name);
}

void AnimatedOverlay::DrawImage(const std::string &inName) {
    // Load our image.
    CairoSurfacePtr image(LoadImage(inName));
    if (image.is_null())
        THROW("Error loading image for AnimatedOverlay");
    
    // Draw our bitmap.
    DrawingArea *dc = GetDrawingArea();
    dc->Clear();
    dc->DrawImage(image, 0, 0);
}

void AnimatedOverlay::NotifyStateChanged() {
    // Handle changes to our x,y co-ordinates.
    wxPoint offset(tvalue_cast<int32>(gStateDB.Get(this, mStatePath + "/x")),
                   tvalue_cast<int32>(gStateDB.Get(this, mStatePath + "/y")));
    if (offset != mCurrentOffset) {
        mCurrentOffset = offset;
        UpdatePosition();
    }

    // Handle changes to our image.
    int32 index(tvalue_cast<int32>(gStateDB.Get(this, mStatePath + "/index")));
    if (index < 0 || static_cast<size_t>(index) >= mImages.size())
        THROW("Invalid index for AnimatedOverlay");
    std::string image = mImages.at(index);
    if (image != mCurrentImage) {
        mCurrentImage = image;
        DrawImage(image);
    }
}
