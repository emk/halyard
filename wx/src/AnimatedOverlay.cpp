// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

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
								 wxCursor &inCursor,
                                 bool inHasAlpha,
                                 std::string &inStatePath, 
								 TValueList graphics)
     : Overlay(inStage, inName, inBounds, inDispatch, inCursor, inHasAlpha),
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
	if (index >= mGraphics.size() || index < 0)
		THROW("Invalid index for AnimatedOverlay");
	std::string graphic = mGraphics.at(index);
    if (graphic != mCurrentGraphic) {
        mCurrentGraphic = graphic;
        DrawGraphic(graphic);
    }
}
