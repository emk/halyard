// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "DrawingContextStack.h"
#include "Stage.h"
#include "Element.h"
#include "DrawingArea.h"

bool DrawingContextStack::ContainsElement(Element *inElement) {
    ElementStack::iterator i = mDrawingContextStack.begin();
    for (; i != mDrawingContextStack.end(); ++i)
        if (*i == inElement)
            return true;
    return false;
}

DrawingArea *DrawingContextStack::GetCurrentDrawingArea() {
    if (IsEmpty()) {
        return mStage->GetBackgroundDrawingArea();
    } else {
        DrawingArea *area = mDrawingContextStack.back()->GetDrawingArea();
        ASSERT(area != NULL);
        return area;
    }
}

void DrawingContextStack::PushDrawingContext(Element *inElement) {
    DrawingArea *area = inElement->GetDrawingArea();
    if (!area)
        THROW("Element has no associated drawing context");
    mDrawingContextStack.push_back(inElement);
}

void DrawingContextStack::PopDrawingContext(Element *inElement) {
    ASSERT(!IsEmpty());
    if (inElement != mDrawingContextStack.back())
        gLog.FatalError("Mismatched drawing context push/pop");
    mDrawingContextStack.pop_back();
}
