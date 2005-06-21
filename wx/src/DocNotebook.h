// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

// New design:
//   DocNotebook
//     - Document list
//     wxSplitterWindow
//       vbox
//         DocNotebookBar
//           - Current tab number
//         wxWindow displaying document
//       vbox
//         DocNotebookBar
//           - Current tab number
//         wxWindow displaying document

#ifndef DocNotebook_H
#define DocNotebook_H

#include "EventDelegator.h"

class DocNotebookBar;
class DocNotebookTab;

//////////
/// A tabbed notebook containing zero or more documents.  This works like
/// Mozilla or MS Visual Studio: The individual tabs can be closed.
///
class DocNotebook : public wxWindow {
    friend class DocNotebookTab;
    friend class DocNotebookBar;

    wxFrame *mFrameToTitle;
    wxString mOriginalFrameTitle;
    int mUntitledDocumentCount;
    EventDelegator mDelegator;

    std::vector<DocNotebookTab*> mDocs;
    DocNotebookBar *mBar;
    wxSizer *mSizer;
    wxWindow *mCurrentDoc;

    void SetCurrentDoc(wxWindow *newDoc);
    DocNotebookTab *GetCurrentDocument();
    void UpdateFrameTitle();

public:
    DocNotebook(wxWindow *parent, wxWindowID id = -1);

    void SetFrameToTitle(wxFrame *frame);

    void AddDocument(DocNotebookTab *doc);

    void RemoveDocumentInternal(size_t i);

    size_t GetDocumentCount() const;
    DocNotebookTab *GetDocument(size_t index);
    void SelectDocument(size_t index);
    void SelectDocument(const DocNotebookTab *doc);

    bool MaybeCloseTab();
    bool MaybeSaveAll(bool canVeto, const wxString &title,
                      const wxString &prompt);

    wxString GetNextUntitledDocumentName();

private:
    virtual bool ProcessEvent(wxEvent& event);

    void OnSize(wxSizeEvent &event);
    void OnEraseBackground(wxEraseEvent &inEvent);
    void OnSaveAll(wxCommandEvent &event);
    void OnUpdateUiSaveAll(wxUpdateUIEvent &event);
    void OnCloseTab(wxCommandEvent &event);
    void OnUpdateUiCloseTab(wxUpdateUIEvent &event);

    DECLARE_EVENT_TABLE();
};

//////////
/// A document tab in DocNotebook.  This is intended to be used as a mixin
/// class for wxWindow subclasses.
///
class DocNotebookTab : boost::noncopyable {
    friend class DocNotebook;
    friend class DocNotebookBar;
    
    DocNotebook *mParent;
    DocNotebookBar *mBar;
    wxString mDocumentTitle;
    wxString mDocumentPath;
    bool mDocumentDirty;
    wxCoord mTabRightEdge;

    void ReportChanges();
    void SetTabRightEdge(wxCoord edge);
    wxCoord GetTabRightEdge() const;

protected:
    DocNotebookTab(DocNotebook *parent);

    void SetDocumentTitle(const wxString &title);
    void SetDocumentPath(const wxString &path);
    void SetDocumentDirty(bool dirty);

    virtual bool SaveDocument(bool force) { return true; }
    bool MaybeSave(bool canVeto, const wxString &title,
                   const wxString &prompt);

    virtual void NotifySelected() {}

public:
    wxString GetDocumentTitle() const;
    wxString GetDocumentPath() const;
    bool GetDocumentDirty() const;
    wxString GetDocumentTitleAndDirtyFlag() const;

    void SelectDocument() const;

    /// This can be used by the containing wxFrame class to check
    /// timestamps and reload files on window activation.
    virtual void OfferToReloadIfChanged() {}

    /// Call this method to (attempt to) change the text size.
    virtual void SetTextSize(int size) {}

    //virtual void Save() = 0;
    virtual wxWindow *GetDocumentWindow() = 0;
};

//////////
/// The tab bar across the top of a DocNotebook.  This does most of the real
/// work.
///
class DocNotebookBar : public wxWindow {
    friend class DocNotebook;
    friend class DocNotebookTab;

public:
    DocNotebookBar(DocNotebook *parent, wxWindowID id = -1);

    size_t GetCurrentTab();
    void SetCurrentTab(size_t tabId);
    void LastTabDeleted();

private:
    enum {
        TOP_PAD = 4,
        TAB_START = TOP_PAD / 2,
        BOTTOM_PAD = 2,
        SIDE_PAD = 4,

        BUTTON_INSIDE_PAD = 2,
        BUTTON_GRAPHIC_SIZE = 9,
        BUTTON_SIZE = BUTTON_GRAPHIC_SIZE + BUTTON_INSIDE_PAD*2,
    };

    enum ButtonId {
        BUTTON_NONE = -1,
        BUTTON_LEFT = 0,
        BUTTON_RIGHT = 1,
        BUTTON_CLOSE = 2,
        BUTTON_COUNT = 3
    };

    enum ButtonState {
        STATE_DISABLED,
        STATE_ENABLED,
        STATE_PRELIGHTED,
        STATE_CLICKED
    };

    enum GuiColor {
        SELECTED_TEXT,
        SELECTED_BACKGROUND,
        SELECTED_HIGHLIGHT,
        SELECTED_SHADOW,
        UNSELECTED_TEXT,
        UNSELECTED_BACKGROUND,
        UNSELECTED_DIVIDER
    };

    DocNotebook *mNotebook;
    size_t mCurrentTab;
    wxCoord mScrollAmount;
    ButtonState mButtonStates[BUTTON_COUNT];
    ButtonId mGrabbedButton;
    wxLongLong mNextRepeatTime;

    void ReportChanges();

    unsigned char Lighten(unsigned char value, double fraction);
    wxColor GetGuiColor(GuiColor color);
    wxFont GetGuiFont(bool bold);
    wxCoord UpdateBarHeight();

    void OnPaint(wxPaintEvent &event);
    void OnLeftDown(wxMouseEvent &event);
    void OnLeftUp(wxMouseEvent &event);
    void OnIdle(wxIdleEvent &event);
    void OnMouseMove(wxMouseEvent &event);
    void OnSize(wxSizeEvent &event);

    void DoButtonPressed(ButtonId buttonId);
    void DoButtonHeld(ButtonId buttonId);
    void DoButtonReleased(ButtonId buttonId);

    void SetButtonState(ButtonId buttonId, ButtonState state,
                        bool redraw = true);
    void SetButtonStateIfNotDisabled(ButtonId buttonId, ButtonState state,
                                     bool redraw = true);
    void EnableButton(ButtonId buttonId, bool enable, bool redraw = true);
    bool SafeToRunCommandsForButton(ButtonId buttonId);
    void DrawButton(wxDC &dc, ButtonId buttonId);
    wxBitmap GetButtonBitmap(ButtonId buttonId);
    wxRect GetButtonRect(ButtonId buttonId);
    ButtonId GetButtonForPoint(const wxPoint &p);

    wxCoord GetTabLimit();
    void ScrollTabs(wxCoord pixels);
    
    DECLARE_EVENT_TABLE();
};

#endif DocNotebook_H
