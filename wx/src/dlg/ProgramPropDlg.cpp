// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/xrc/xmlres.h>

#include "TCommon.h"
#include "TException.h"

#include "ProgramPropDlg.h"

BEGIN_EVENT_TABLE(ProgramPropDlg, PropertyDlg)
END_EVENT_TABLE()

BEGIN_PROPERTY_DIALOG(ProgramPropDlg, "Program Properties")
	PROPERTY_FIELD("name", "Name:", 0)
	PROPERTY_FIELD("version", "Version:", 0)
	PROPERTY_FIELD("copyright", "Copyright:", 0)
	PROPERTY_FIELD("comments", "Comments:", PropertyDlg::MULTILINE)
END_PROPERTY_DIALOG()

ProgramPropDlg::ProgramPropDlg(wxWindow *inParent, model::Object *inObject)
	: PropertyDlg(inParent, GetDialogDescription(), inObject)
{
}
