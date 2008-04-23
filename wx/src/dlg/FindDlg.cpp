// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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
#include <wx/config.h>
#include "FindDlg.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(FindDlg, XrcDlg)
    EVT_TEXT(XRCID("DLG_SEARCH_TEXT"), FindDlg::OnText)
    EVT_TEXT(XRCID("DLG_REPLACE_TEXT"), FindDlg::OnText)
    EVT_TEXT_ENTER(XRCID("DLG_SEARCH_TEXT"), FindDlg::OnTextEnter)
    EVT_TEXT_ENTER(XRCID("DLG_REPLACE_TEXT"), FindDlg::OnTextEnter)
    EVT_BUTTON(XRCID("DLG_FIND"), FindDlg::OnButton)
    EVT_BUTTON(XRCID("DLG_FIND_ALL"), FindDlg::OnButton)
    EVT_BUTTON(XRCID("DLG_REPLACE"), FindDlg::OnButton)
    EVT_BUTTON(XRCID("DLG_REPLACE_ALL"), FindDlg::OnButton)
    EVT_BUTTON(XRCID("DLG_DONT_FIND"), FindDlg::OnButton)
END_EVENT_TABLE()

FindDlg::FindDlg(wxWindow *inParent, bool haveSelection)
  : XrcDlg(inParent, wxT("DLG_FIND_REPLACE"))
{
    // Bind variables to some of our child widgets.
    Bind(mUseRegex, XRCID("DLG_USE_REGEX"));
    Bind(mSearchText, XRCID("DLG_SEARCH_TEXT"));
    Bind(mStartAtTop, XRCID("DLG_START_AT_TOP"));
    Bind(mWrapAround, XRCID("DLG_WRAP_AROUND"));
    Bind(mSearchBackwards, XRCID("DLG_SEARCH_BACKWARDS"));
    Bind(mCaseSensitive, XRCID("DLG_CASE_SENSITIVE"));
    Bind(mMatchEntireWords, XRCID("DLG_MATCH_ENTIRE_WORDS"));
    Bind(mSearchCurrentScript, XRCID("DLG_SEARCH_CURRENT_SCRIPT"));
    Bind(mSearchAllScripts, XRCID("DLG_SEARCH_ALL_SCRIPTS"));
    Bind(mSearchSelectionOnly, XRCID("DLG_SEARCH_SELECTION_ONLY"));
    Bind(mReplaceText, XRCID("DLG_REPLACE_TEXT"));
    Bind(mButtonFind, XRCID("DLG_FIND"));
    Bind(mButtonFindAll, XRCID("DLG_FIND_ALL"));
    Bind(mButtonReplace, XRCID("DLG_REPLACE"));
    Bind(mButtonReplaceAll, XRCID("DLG_REPLACE_ALL"));
    Bind(mButtonDontFind, XRCID("DLG_DONT_FIND"));
    Bind(mButtonCancel, wxID_CANCEL);

    /// We need to do this now, because our dialog editor refuses to allow
    /// this flag to be set in conjunction with wxTE_MULTILINE.
    TurnOnProcessEnter(mSearchText);
    TurnOnProcessEnter(mReplaceText);

    // Change our tab order.  This will have weird results unless tabbing
    // *out* of a multiline wxTextCtrl is handled by wxWidgets (this event
    // is typically handled by ::IsDialogMessage and we currently need to
    // patch wxWidgets to cope).
    mReplaceText->MoveBeforeInTabOrder(mUseRegex);
    mSearchText->MoveBeforeInTabOrder(mReplaceText);

    // Restore our values from the registry.
    mUseRegex->SetValue(GetUseRegex());
    mSearchText->SetValue(GetSearchText());
    mStartAtTop->SetValue(GetStartAtTop());
    mWrapAround->SetValue(GetWrapAround());
    mSearchBackwards->SetValue(GetSearchBackwards());
    mCaseSensitive->SetValue(GetCaseSensitive());
    mMatchEntireWords->SetValue(GetMatchEntireWords());
    mReplaceText->SetValue(GetReplaceText());
    mSearchCurrentScript->SetValue(true);
    switch (GetSearchArea()) {
        case CURRENT_SCRIPT:
            // This is set up as the default above.
            break;
        case ALL_SCRIPTS:
            mSearchAllScripts->SetValue(true);
            break;
        case SELECTION_ONLY:
            if (haveSelection)
                mSearchSelectionOnly->SetValue(true);
            else
                mSearchCurrentScript->SetValue(true);
            break;
    }

    // Disable inapplicable widgets.
    if (!haveSelection)
        mSearchSelectionOnly->Enable(false);

    // Disable unimplemented widgets.
    mSearchBackwards->Enable(false);
    mSearchAllScripts->Enable(false);
    mButtonFindAll->Enable(false);

    // Update all the controls in the dialog.
    UpdateControls();
}

void FindDlg::EndModal(int retCode) {
    XrcDlg::EndModal(retCode);
    if (retCode != wxID_CANCEL) {
        // Save our values to the registry.
        SetUseRegex(mUseRegex->GetValue());
        SetSearchText(mSearchText->GetValue());
        SetStartAtTop(mStartAtTop->GetValue());
        SetWrapAround(mWrapAround->GetValue());
        SetSearchBackwards(mSearchBackwards->GetValue());
        SetCaseSensitive(mCaseSensitive->GetValue());
        SetMatchEntireWords(mMatchEntireWords->GetValue());
        SetReplaceText(mReplaceText->GetValue());        
        SearchArea area = CURRENT_SCRIPT;
        if (mSearchAllScripts->GetValue())
            area = ALL_SCRIPTS;
        else if (mSearchSelectionOnly->GetValue())
            area = SELECTION_ONLY;
        SetSearchArea(area);
    }
}

/// Enable the wxPROCESS_ENTER style on an existing widget.
void FindDlg::TurnOnProcessEnter(wxTextCtrl *ctrl) {
    long style = ctrl->GetWindowStyleFlag();
    ctrl->SetWindowStyleFlag(style & wxTE_PROCESS_ENTER);
}

/// Update the controls in our dialog box.
void FindDlg::UpdateControls() {
    if (mSearchText->GetValue().Length() > 0) {
        mButtonFind->Enable();
        mButtonReplace->Enable();
        mButtonReplaceAll->Enable();
        mButtonFind->SetDefault();
    } else {
        mButtonFind->Disable();
        mButtonReplace->Disable();
        mButtonReplaceAll->Disable();
        mButtonCancel->SetDefault();
    }
}

/// Called whenever the text in one of our search fields changes.
void FindDlg::OnText(wxCommandEvent &event) {
    UpdateControls();
}

/// Dismiss the dialog by selecting the default button.
void FindDlg::OnTextEnter(wxCommandEvent &event) {
    EndModal(GetDefaultItem()->GetId());
}

/// Dismiss the dialog, returning our button ID.
void FindDlg::OnButton(wxCommandEvent &event) {
    EndModal(event.GetId());
}

/// Set a registry key.
void FindDlg::SetKey(const wxString &key, bool value) {
	shared_ptr<wxConfigBase> config(new wxConfig);
    config->Write(wxT("/Search/") + key, value);
}

/// Set a registry key.
void FindDlg::SetKey(const wxString &key, const wxString &value) {
	shared_ptr<wxConfigBase> config(new wxConfig);
    config->Write(wxT("/Search/") + key, value);    
}

/// Get a registry key.
bool FindDlg::GetKeyBool(const wxString &key) {
	shared_ptr<wxConfigBase> config(new wxConfig);
    bool result;
    config->Read(wxT("/Search/") + key, &result, false);
    return result;
}

/// Get a registry key.
wxString FindDlg::GetKeyString(const wxString &key) {
	shared_ptr<wxConfigBase> config(new wxConfig);
    wxString result;
    config->Read(wxT("/Search/") + key, &result, wxT(""));
    return result;    
}

/// Set our search area.
void FindDlg::SetSearchArea(SearchArea area) {
	shared_ptr<wxConfigBase> config(new wxConfig);
    config->Write(wxT("/Search/SearchArea"), static_cast<int>(area));
}

/// Get our search area.
FindDlg::SearchArea FindDlg::GetSearchArea() {
	shared_ptr<wxConfigBase> config(new wxConfig);
    int regval;
    config->Read(wxT("/Search/SearchArea"), &regval, CURRENT_SCRIPT);
    switch (regval) {
        case CURRENT_SCRIPT:
        case ALL_SCRIPTS:
        case SELECTION_ONLY:
            return static_cast<SearchArea>(regval);
        default:
            // Unrecognized value; do something reasonable.
            return CURRENT_SCRIPT;
    }
}
