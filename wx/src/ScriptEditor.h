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

#ifndef ScriptEditor_H
#define ScriptEditor_H

#include "TInterpreter.h"
#include "EventDelegator.h"
#include "SashFrame.h"

class wxSashLayoutWindow;
class ScriptTree;
class DocNotebook;

typedef std::vector<Halyard::TScriptIdentifier> IdentifierList;

class ScriptEditor : public SashFrame, public Halyard::TReloadNotified  {
    static ScriptEditor *sFrame;
    static void MaybeCreateFrame();

    wxSashLayoutWindow *mTreeContainer;
    ScriptTree *mTree;
    DocNotebook *mNotebook;
    EventDelegator mDelegator;
    bool mProcessingActivateEvent;
    IdentifierList mIdentifiers;

public:
    static void EditScripts();
    static bool SaveAllForReloadScript();
    static bool ProcessEventIfExists(wxEvent &event);
    static void OpenDocument(const wxString &path, int line = 1);
    static void ShowDefinition(const wxString &identifier);
    static void HighlightFile(const wxString &path);
    static IdentifierList GetIdentifiers();

    ScriptEditor();
    ~ScriptEditor();

    virtual bool ProcessEvent(wxEvent& event);

    int GetTextSize();
    void SetTextSize(int size);
    void ChangeTextSize(int delta);

private:
    void LoadSashLayout(shared_ptr<wxConfigBase> inConfig);
    void SaveSashLayout(shared_ptr<wxConfigBase> inConfig);

    void OpenDocumentInternal(const wxString &path, int line = 1);
    void ShowDefinitionInternal(const wxString &identifier);
    void HighlightFileInternal(const wxString &path);
    void NotifyReloadScriptSucceeded();
    void UpdateIdentifierInformation();

    void OnActivate(wxActivateEvent &event);
    void OnClose(wxCloseEvent &event);
    void OnNew(wxCommandEvent &event);
    void OnOpen(wxCommandEvent &event);
    void OnCloseWindow(wxCommandEvent &event);
    void DisableUiItem(wxUpdateUIEvent &event);
    void OnIncreaseTextSize(wxCommandEvent &event);
    void OnDecreaseTextSize(wxCommandEvent &event);

    DECLARE_EVENT_TABLE();
};

#endif // ScriptEditor_H
