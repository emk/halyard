// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

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

void Element::OperationNotSupported(const char *inOperationName) {
    std::string op(inOperationName);
    std::string name(mName.mb_str());
    THROW("Cannot " + op + " element: " + name);
}

void Element::MoveTo(const wxPoint &inPoint) {
    OperationNotSupported("move");
}

void Element::Show(bool inShow) {
    if (inShow != IsShown()) {
        if (inShow)
            OperationNotSupported("show");
        else
            OperationNotSupported("hide");
    }
}
