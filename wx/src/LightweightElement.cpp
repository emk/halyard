// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "LightweightElement.h"
#include "EventDispatcher.h"
#include "Stage.h"

LightweightElement::LightweightElement(Stage *inStage, const wxString &inName,
									   FIVEL_NS TCallbackPtr inDispatch,
									   wxCursor &inCursor)
    : Element(inStage, inName, inDispatch), mCursor(inCursor), mIsShown(true)
{
}

void LightweightElement::Show(bool inShow) {
    if (inShow != mIsShown) {
        // Update our state, and notify the stage.
        mIsShown = inShow;
        GetStage()->NotifyElementsChanged();
    }
}
