// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AnimatedOverlay_H
#define AnimatedOverlay_H

#include "Overlay.h"

class AnimatedOverlay : public Overlay {
	typedef std::vector<std::string> GraphicsList;
	GraphicsList mGraphics;
	std::string mState;
	
	wxBitmap LoadPicture(const std::string &inName);
	void DrawGraphic(const std::string &inName);
	
public:
	AnimatedOverlay (Stage *inStage, const wxString &inName, 
					 const wxRect &inBounds, 
					 FIVEL_NS TCallbackPtr inDispatch,
					 wxCursor &inCursor, std::string &inState, 
					 TValueList graphics);     
	
	virtual void NotifyStateChanged();
};

#endif // AnimatedOverlay_H
