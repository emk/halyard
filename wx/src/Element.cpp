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

void Element::DrawElementBorder(wxDC &inDC)
{
	// Draw the border *outside* our rectangle.
	wxRect r = this->GetRect();
	r.Inflate(1);
	inDC.DrawRectangle(r.x, r.y, r.width, r.height);
}
