// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef ProgramPropDlg_H
#define ProgramPropDlg_H

#include "PropertyDlg.h"

namespace model { class Object; }

//////////
// This is the dialog which pops up at startup time and asks you whether
// you want to open an existing program or create a new one.
//
class ProgramPropDlg : public PropertyDlg
{
    DECLARE_EVENT_TABLE();
	DECLARE_PROPERTY_DIALOG();

public:
	ProgramPropDlg(wxWindow *inParent, model::Object *inObject);
};

#endif // ProgramPropDlg_H
