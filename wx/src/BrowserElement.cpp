// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "BrowserElement.h"
#include "EventDispatcher.h"

BrowserElement::BrowserElement(Stage *inStage, const wxString &inName,
                               FIVEL_NS TCallback *inDispatch)
    : Widget(inStage, inName)
{
    ASSERT(inDispatch);
	mDispatcher = new EventDispatcher();
	mDispatcher->SetDispatcher(inDispatch);
}

BrowserElement::~BrowserElement() {
	// XXX - The callback in this dispatcher may still be active!
    delete mDispatcher;
}
