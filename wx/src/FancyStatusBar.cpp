// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "FancyStatusBar.h"

FancyStatusBar::FancyStatusBar(wxWindow *inParent)
	: wxStatusBar(inParent, -1)
{
	int field_widths[] = {-1, 150, 20};
	SetFieldsCount(3, field_widths);
}
