// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "BrowserElement.h"
#include "EventDispatcher.h"

BrowserElement::BrowserElement(Stage *inStage, const wxString &inName,
                               FIVEL_NS TCallbackPtr inDispatch)
    : Widget(inStage, inName, inDispatch)
{
}
