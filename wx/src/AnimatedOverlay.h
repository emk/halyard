// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AnimatedOverlay_H
#define AnimatedOverlay_H

#include "Overlay.h"

class AnimatedOverlay : public Overlay, public TStateListener {
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
                    TValueList graphics);

    virtual void MoveTo(const wxPoint &inPoint);
	virtual void NotifyStateChanged();
};

#endif // AnimatedOverlay_H
