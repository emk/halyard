// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "LightweightElement.h"
#include "EventDispatcher.h"

LightweightElement::LightweightElement(Stage *inStage, const wxString &inName,
									   FIVEL_NS TCallback *inDispatch,
									   wxCursor &inCursor)
    : Element(inStage, inName), mCursor(inCursor)
{
    ASSERT(inDispatch);
	mDispatcher = new EventDispatcher();
	mDispatcher->SetDispatcher(inDispatch);
}

LightweightElement::~LightweightElement()
{
	// XXX - The callback in this dispatcher may still be active!
    delete mDispatcher;
}

