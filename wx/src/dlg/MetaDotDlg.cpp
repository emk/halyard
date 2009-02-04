// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "AppHeaders.h"
#include "MetaDotDlg.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(MetaDotDlg, XrcDlg)
	EVT_BUTTON(wxID_OK, MetaDotDlg::OnOK)
END_EVENT_TABLE()

MetaDotDlg::MetaDotDlg(wxWindow *inParent,
                       const ScriptEditorDB::Definitions &defs)
    : XrcDlg(inParent, wxT("DLG_META_DOT")),
      mDefinitions(defs), mChosenDefIndex(0)
{
    ASSERT(!mDefinitions.empty());
	Bind(mDefinitionsListBox, XRCID("DLG_DEF_LIST"));

    ScriptEditorDB::Definitions::iterator i = mDefinitions.begin();
    for (; i != mDefinitions.end(); ++i) {
        std::stringstream line_stream;
        line_stream << i->file_path << ", line " << i->line_number;
        wxString line(line_stream.str().c_str(), wxConvLocal);
        mDefinitionsListBox->Append(line);
    }
    mDefinitionsListBox->SetSelection(0);
}

void MetaDotDlg::OnOK(wxCommandEvent &inEvent) {
	Hide();
    mChosenDefIndex = mDefinitionsListBox->GetSelection();
	EndModal(wxID_OK);
}

ScriptEditorDB::Definition MetaDotDlg::GetChosenDef() {
    ASSERT(mChosenDefIndex < mDefinitions.size());
    return mDefinitions[mChosenDefIndex];
}
