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
								 wxCursor &inCursor, std::string &inState, 
								 TValueList graphics)
     : Overlay(inStage, inName, inBounds, inDispatch, inCursor, true),
       mState(inState)
{
	// convert TValueList to GraphicsList
	TValueList::iterator iter = graphics.begin();
	for (; iter != graphics.end(); ++iter)
		mGraphics.push_back(*iter);
	
	NotifyStateChanged();
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
	TPoint loc = TPoint (gStateDB.Get(this, mState + "/x"),
						 gStateDB.Get(this, mState + "/y"));
	
	MoveTo(TToWxPoint(loc));
	int32 index = gStateDB.Get(this, mState + "/index");
	if (index >= mGraphics.size() || index < 0)
		THROW("Invalid index for AnimatedOverlay");
	
	std::string currentGraphic = mGraphics.at(index);
	DrawGraphic(currentGraphic);
}
