// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "XrcDlg.h"

BEGIN_EVENT_TABLE(XrcDlg, wxDialog)
END_EVENT_TABLE()

XrcDlg::XrcDlg(wxWindow *inParent, const wxString &inResourceName)
{
	wxTheXmlResource->LoadDialog(this, inParent, inResourceName);
}

