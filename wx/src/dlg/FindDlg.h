// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#ifndef FindDlg_H
#define FindDlg_H

#include "XrcDlg.h"

//////////
/// This is the dialog which pops up at startup time and asks you whether
/// you want to open an existing program or create a new one.
///
class FindDlg : public XrcDlg
{
public:
    enum SearchArea {
        CURRENT_SCRIPT,
        ALL_SCRIPTS,
        SELECTION_ONLY
        // If you add something new here, you'll need to tweak
        // a bunch of places in the source code.
    };


private:
    DECLARE_EVENT_TABLE();
    
    wxCheckBox *mUseRegex;
    wxTextCtrl *mSearchText;
    wxCheckBox *mStartAtTop;
    wxCheckBox *mWrapAround;
    wxCheckBox *mSearchBackwards;
    wxCheckBox *mCaseSensitive;
    wxCheckBox *mMatchEntireWords;
    wxRadioButton *mSearchCurrentScript;
    wxRadioButton *mSearchAllScripts;
    wxRadioButton *mSearchSelectionOnly;
    wxTextCtrl *mReplaceText;
    wxButton *mButtonFind;
    wxButton *mButtonFindAll;
    wxButton *mButtonReplace;
    wxButton *mButtonReplaceAll;
    wxButton *mButtonDontFind;
    wxButton *mButtonCancel;

    void InterceptEnterEvents(wxTextCtrl *ctrl);
    void UpdateControls();

    void OnText(wxCommandEvent &event);
    void OnKeyDownCheckForEnter(wxKeyEvent &event);
    void OnButton(wxCommandEvent &event);

    /// \todo Is it safe to put these in the registry, or does that make
    /// multiple instances of our app really weird?
    static void SetKey(const wxString &key, bool value);
    static void SetKey(const wxString &key, const wxString &value);
    static bool GetKeyBool(const wxString &key);
    static wxString GetKeyString(const wxString &key);

public:
    FindDlg(wxWindow *inParent, bool haveSelection);

    virtual void EndModal(int retCode);

#define DEFINE_REGISTRY_ACCESSORS_BOOL(KEY) \
    static void Set##KEY(bool value) { SetKey(wxT(#KEY), value); }  \
    static bool Get##KEY() { return GetKeyBool(wxT(#KEY)); }
#define DEFINE_REGISTRY_ACCESSORS_STRING(KEY) \
    static void Set##KEY(const wxString &value) { SetKey(wxT(#KEY), value); } \
    static wxString Get##KEY() { return GetKeyString(wxT(#KEY)); }

    DEFINE_REGISTRY_ACCESSORS_BOOL(UseRegex);
    DEFINE_REGISTRY_ACCESSORS_STRING(SearchText);
    DEFINE_REGISTRY_ACCESSORS_BOOL(StartAtTop);
    DEFINE_REGISTRY_ACCESSORS_BOOL(WrapAround);
    DEFINE_REGISTRY_ACCESSORS_BOOL(SearchBackwards);
    DEFINE_REGISTRY_ACCESSORS_BOOL(CaseSensitive);
    DEFINE_REGISTRY_ACCESSORS_BOOL(MatchEntireWords);
    DEFINE_REGISTRY_ACCESSORS_STRING(ReplaceText);

#undef DEFINE_REGISTRY_ACCESSORS_BOOL
#undef DEFINE_REGISTRY_ACCESSORS_STRING

    static void SetSearchArea(SearchArea area);
    static SearchArea GetSearchArea();
};

#endif // FindDlg_H
