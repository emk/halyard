// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef InvisibleElement_H
#define InvisibleElement_H

#include "Element.h"

//////////
// An invisible element never actually gets displayed.  Most of these are
// currently associated with audio, but there may be more types in the
// future.
//
class InvisibleElement : public Element {
public:
	InvisibleElement(Stage *inStage, const wxString &inName)
        : Element(inStage, inName) {}

    virtual wxRect GetRect() { return wxRect(0, 0, 0, 0); }
    virtual bool HasVisibleRepresentation() { return false; }
    virtual bool IsShown() { return false; }
    virtual bool IsLightWeight() { return true; }
    virtual bool IsPointInElement(const wxPoint &inPoint) { return false; }
};

#endif // InvisibleElement_H
