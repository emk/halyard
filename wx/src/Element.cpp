// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "Element.h"
#include "Stage.h"


//=========================================================================
//  Element Methods
//=========================================================================

Element::Element(Stage *inStage, const wxString &inName)
	: mStage(inStage), mName(inName)
{
    ASSERT(mStage);
    ASSERT(mName != "");

	mStage->AddElement(this);
}
