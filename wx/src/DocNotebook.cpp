// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"
#include "FiveLApp.h"
#include "AppGlobals.h"
#include "DocNotebook.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  DocNotebook Methods
//=========================================================================

BEGIN_EVENT_TABLE(DocNotebook, wxWindow)
    EVT_SIZE(DocNotebook::OnSize)
    EVT_ERASE_BACKGROUND(DocNotebook::OnEraseBackground)

    EVT_MENU(FIVEL_SAVE_ALL, DocNotebook::OnSaveAll)
    EVT_UPDATE_UI(FIVEL_SAVE_ALL, DocNotebook::OnUpdateUiSaveAll)
    EVT_MENU(FIVEL_CLOSE_TAB, DocNotebook::OnCloseTab)
    EVT_UPDATE_UI(FIVEL_CLOSE_TAB, DocNotebook::OnUpdateUiCloseTab)
END_EVENT_TABLE()

DocNotebook::DocNotebook(wxWindow *parent, wxWindowID id)
    : wxWindow(parent, id), mFrameToTitle(NULL), mUntitledDocumentCount(0)
{
    wxColor bgcolor = wxSystemSettings::GetColour(wxSYS_COLOUR_APPWORKSPACE);
    SetBackgroundColour(bgcolor);

    mBar = new DocNotebookBar(this);
    mCurrentDoc = NULL;

    mSizer = new wxBoxSizer(wxVERTICAL);
    mSizer->Add(mBar, 0 /* don't stretch */, wxGROW, 0);
    SetSizer(mSizer);
    mSizer->SetSizeHints(this);
}

void DocNotebook::SetCurrentDoc(wxWindow *newDoc) {
    // Don't change documents if we don't have to.
    if (newDoc == mCurrentDoc)
        return;

    // Update mCurrentDoc immediately so the OnEraseBackground method is
    // living in the right general universe.
    wxWindow *old_doc = mCurrentDoc;
    mCurrentDoc = newDoc;

    // Remove the current doc, if any.
    if (old_doc) {
        mSizer->Remove(1);
        old_doc->Hide();
    }

    // Insert the new doc, if any.
    if (newDoc) {
        mSizer->Add(newDoc, 1 /* stretch */, wxGROW, 0);
        newDoc->Show();
        newDoc->SetFocus();
    }

    // Set up event delegation (if newDoc == NULL, this cancels).
    mDelegator.SetDelegate(newDoc);

    // Fix the title bar.
    UpdateFrameTitle();

    // Rerun layout.
    Layout();
}

/// Return the currently displayed document, or NULL if none.
DocNotebookTab *DocNotebook::GetCurrentDocument() {
    if (GetDocumentCount() == 0)
        return NULL;
    else
        return GetDocument(mBar->GetCurrentTab());
}

void DocNotebook::UpdateFrameTitle() {
    if (mFrameToTitle) {
        DocNotebookTab *doc = GetCurrentDocument();
        wxString new_title;
        if (!doc) {
            new_title = mOriginalFrameTitle;
        } else {
            // According to Windows standards, we should display the name
            // of the document followed by the name of the application.
            new_title += (doc->GetDocumentTitleAndDirtyFlag() +
                          " - " + wxGetApp().GetAppName());

            // If we have a path name, add it to be polite.
            wxString path = doc->GetDocumentPath();
            if (path != "")
                new_title += " - [" + path + "]";
        }

        // Update the frame title if necessary.
        if (mFrameToTitle->GetTitle() != new_title)
            mFrameToTitle->SetTitle(new_title);
    }
}

/// Select a frame on which to display the title of the currently
/// selected document.
void DocNotebook::SetFrameToTitle(wxFrame *frame) {
    mFrameToTitle = frame;
    mOriginalFrameTitle = frame->GetTitle();
    UpdateFrameTitle();
}

/// Add a new document to the notebook.
void DocNotebook::AddDocument(DocNotebookTab *doc) {
    // New document windows should be created with a Hide(), Create(...)
    // sequence to prevent them from being drawn before we can lay them
    // out.  Yes, this is obnoxious.
    ASSERT(!doc->GetDocumentWindow()->IsShown());
    wxASSERT(doc->GetDocumentWindow()->GetParent() == this);
    mDocs.push_back(doc);
    SelectDocument(GetDocumentCount()-1);
}

/// Temporary function which will exist until refactoring is done.
void DocNotebook::RemoveDocumentInternal(size_t i) {
    mDocs.erase(mDocs.begin()+i);
}

/// Get the number of open documents.
size_t DocNotebook::GetDocumentCount() const {
    return mDocs.size();
}

/// Get a pointer to the specified document.
DocNotebookTab *DocNotebook::GetDocument(size_t index) {
    wxASSERT(0 <= index && index < mDocs.size());
    return mDocs[index];
}

/// Select the specified document.
void DocNotebook::SelectDocument(size_t index) {
    wxASSERT(0 <= index && index < GetDocumentCount());
    mBar->SetCurrentTab(index);
    SetCurrentDoc(GetCurrentDocument()->GetDocumentWindow());
    GetCurrentDocument()->NotifySelected();
}

/// Select the specified document.
void DocNotebook::SelectDocument(const DocNotebookTab *doc) {
    for (size_t i = 0; i < GetDocumentCount(); i++)
        if (GetDocument(i) == doc)
            SelectDocument(i);
}

bool DocNotebook::MaybeCloseTab() {
    DocNotebookTab *tab = GetCurrentDocument();
    ASSERT(tab);

    // Verify the closing of this tab.
    if (tab->GetDocumentDirty())
        if (!tab->MaybeSave(true, "Close File", "Save \"%s\" before closing?"))
            return false;

	// Remove the document from our internal list.
    mDocs.erase(mDocs.begin()+mBar->GetCurrentTab());

    // Update our tab bar.
    if (GetDocumentCount() == 0) {
        // Handle deletion of last tab.
        mBar->LastTabDeleted();
        SetCurrentDoc(NULL);
    } else if (mBar->mCurrentTab >= GetDocumentCount()) {
		/// \bug We have to check mCurrentTab instead of calling
		/// GetCurrentTab because mBar is temporarily in an
		/// inconsistent state.  We should fix this *before*
		/// deleting the current tab.

        // Choose a new valid tab number.
        SelectDocument(GetDocumentCount()-1);
    } else {
        // Keep the tab number, but update the display.
        SelectDocument(mBar->GetCurrentTab());
    }

    /// \bug This cleanup procedure is almost certainly very buggy.
    tab->GetDocumentWindow()->Destroy();
    // delete tab;
    return true;
}

bool DocNotebook::MaybeSaveAll(bool canVeto, const wxString &title,
                               const wxString &prompt)
{
    for (size_t i = 0; i < GetDocumentCount(); i++) {
        if (GetDocument(i)->GetDocumentDirty()) {
            SelectDocument(i);
            if (!GetDocument(i)->MaybeSave(canVeto, title, prompt) && canVeto)
                return false;
        }
    }
    return true;
}

/// A handy utility function: Creates unique-per-notebook names for untitled
/// documents.
wxString DocNotebook::GetNextUntitledDocumentName() {
    mUntitledDocumentCount++;
    if (mUntitledDocumentCount == 1) {
        return "Untitled";
    } else {
        wxString result;
        result.Printf("Untitled %d", mUntitledDocumentCount);
        return result;
    }
}

bool DocNotebook::ProcessEvent(wxEvent& event) {
    bool result;
    if (mDelegator.DelegateEvent(event, &result))
        return result;
    else
        return wxWindow::ProcessEvent(event);
}

void DocNotebook::OnSize(wxSizeEvent &event) {
    /// \todo Use auto-layout for this instead.
    Layout();
}

void DocNotebook::OnEraseBackground(wxEraseEvent &inEvent) {
    if (mCurrentDoc == NULL)
        // We have no doc, so skip to the next handler and erase things
        // normally.
        inEvent.Skip();
    else
        /* Ignore event to reduce redraw flicker. */;
}

void DocNotebook::OnSaveAll(wxCommandEvent &event) {
    for (size_t i = 0; i < GetDocumentCount(); i++) {
        DocNotebookTab *doc = GetDocument(i);
        if (doc->GetDocumentDirty())
            if (!doc->SaveDocument(false))
                break;
    }
}

void DocNotebook::OnUpdateUiSaveAll(wxUpdateUIEvent &event) {
    bool enable = false;
    for (size_t i = 0; i < GetDocumentCount(); i++) {
        if (GetDocument(i)->GetDocumentDirty()) {
            enable = true;
            break;
        }
    }
    event.Enable(enable);
}

void DocNotebook::OnCloseTab(wxCommandEvent &event) {
    MaybeCloseTab();
}

void DocNotebook::OnUpdateUiCloseTab(wxUpdateUIEvent &event) {
    event.Enable(GetDocumentCount() > 0);
}


//=========================================================================
//  DocNotebookTab Methods
//=========================================================================

DocNotebookTab::DocNotebookTab(DocNotebook *parent)
    : mParent(parent), mBar(NULL),
      mDocumentTitle("<no title>"), mDocumentDirty(false),
      mTabRightEdge(0)
{
}

void DocNotebookTab::ReportChanges() {
    DocNotebookBar *bar = mParent->mBar;
    bar->ReportChanges();
    if (mParent->GetCurrentDocument() == this)
        mParent->UpdateFrameTitle();
}

void DocNotebookTab::SetTabRightEdge(wxCoord edge) {
    mTabRightEdge = edge;
}

wxCoord DocNotebookTab::GetTabRightEdge() const {
    return mTabRightEdge;
}

bool DocNotebookTab::MaybeSave(bool canVeto, const wxString &title,
                               const wxString &prompt)
{
    ASSERT(GetDocumentDirty());
    if (!canVeto) {
        // Don't bother to prompt if the user can't cancel.
        return SaveDocument(true);
    } else {
        // Add the name of our document to the prompt.
        wxString full_prompt;
        full_prompt.Printf(prompt, GetDocumentTitle());

        // Prompt the user.
        wxMessageDialog dlg(GetDocumentWindow(), full_prompt, title,
                            wxYES_NO|wxCANCEL|wxYES_DEFAULT);
        switch (dlg.ShowModal()) {
            case wxID_YES:
                return SaveDocument(false);
            case wxID_NO:
                return true;
            case wxID_CANCEL:
                return false;
            default:
                ASSERT(0);
            return true;
        }
    }
}

void DocNotebookTab::SetDocumentTitle(const wxString &title) {
    if (mDocumentTitle != title) {
        mDocumentTitle = title;
        ReportChanges();
    }
}

wxString DocNotebookTab::GetDocumentTitle() const {
    return mDocumentTitle;
}

void DocNotebookTab::SetDocumentPath(const wxString &path) {
    if (mDocumentPath != path) {
        mDocumentPath = path;
        ReportChanges();
    }
}

wxString DocNotebookTab::GetDocumentPath() const {
    return mDocumentPath;
}

void DocNotebookTab::SetDocumentDirty(bool dirty) {
    if (mDocumentDirty != dirty) {
        mDocumentDirty = dirty;
        ReportChanges();
    }
}

bool DocNotebookTab::GetDocumentDirty() const {
    return mDocumentDirty;
}

wxString DocNotebookTab::GetDocumentTitleAndDirtyFlag() const {
    if (GetDocumentDirty())
        return GetDocumentTitle() + "*";
    else
        return GetDocumentTitle();
}

void DocNotebookTab::SelectDocument() const {
    mParent->SelectDocument(this);
}



//=========================================================================
//  DocNotebookBar Methods
//=========================================================================

BEGIN_EVENT_TABLE(DocNotebookBar, wxWindow)
    EVT_PAINT(DocNotebookBar::OnPaint)
    EVT_LEFT_DOWN(DocNotebookBar::OnLeftDown)
    EVT_LEFT_DCLICK(DocNotebookBar::OnLeftDown)
    EVT_LEFT_UP(DocNotebookBar::OnLeftUp)
    EVT_IDLE(DocNotebookBar::OnIdle)
    EVT_MOTION(DocNotebookBar::OnMouseMove)
    EVT_SIZE(DocNotebookBar::OnSize)
END_EVENT_TABLE()

DocNotebookBar::DocNotebookBar(DocNotebook *parent, wxWindowID id)
    : wxWindow(parent, id), mNotebook(parent)
{
    UpdateBarHeight();
    mCurrentTab = 0;
    mScrollAmount = 0;
    mButtonStates[BUTTON_LEFT] = STATE_DISABLED;
    mButtonStates[BUTTON_RIGHT] = STATE_DISABLED;
    mButtonStates[BUTTON_CLOSE] = STATE_ENABLED;
    mGrabbedButton = BUTTON_NONE;
}

/// Return the index of the currently selected tab.
size_t DocNotebookBar::GetCurrentTab() {
    ASSERT(0 <= mCurrentTab && mCurrentTab < mNotebook->GetDocumentCount());
    return mCurrentTab;
}

/// Set the currently active tab.
void DocNotebookBar::SetCurrentTab(size_t tabId) {
    // Always update this, even if tabId == mCurrentTab--we might be
    // renumbering tabs because of a deletion.
    ASSERT(0 <= tabId && tabId < mNotebook->GetDocumentCount());
    mCurrentTab = tabId;
    Refresh();
}

/// This function is called by the DocNotebook when the last tab is deleted.
void DocNotebookBar::LastTabDeleted() {
    ASSERT(mNotebook->GetDocumentCount() == 0);
    mCurrentTab = 0;
    Refresh();
}

void DocNotebookBar::ReportChanges() {
    Refresh();
}

unsigned char DocNotebookBar::Lighten(unsigned char value, double fraction) {
    return value + (255 - value)*fraction;
}

wxColor DocNotebookBar::GetGuiColor(GuiColor color) {
    wxColor temp;
    switch (color) {
        case SELECTED_TEXT:
            return wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT);
        case SELECTED_BACKGROUND:
            return wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE);
        case SELECTED_HIGHLIGHT:
            //return wxSystemSettings::GetColour(wxSYS_COLOUR_BTNHIGHLIGHT);
            return *wxWHITE;
        case SELECTED_SHADOW:
            //return wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW);
            return *wxBLACK;
        case UNSELECTED_TEXT:
            temp = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT);
            return wxColor(Lighten(temp.Red(), 0.35),
                           Lighten(temp.Green(), 0.35),
                           Lighten(temp.Blue(), 0.35));
        case UNSELECTED_DIVIDER:
            temp = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT);
            return wxColor(Lighten(temp.Red(), 0.5),
                           Lighten(temp.Green(), 0.5),
                           Lighten(temp.Blue(), 0.5));
        case UNSELECTED_BACKGROUND:
            {
                temp = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE);
                int red_green = temp.Red()/2 + temp.Green()/2;
                return wxColor(Lighten(red_green, 0.6),
                               Lighten(red_green, 0.6),
                               Lighten(temp.Blue(), 0.3));
            }
        default:
            ASSERT(0);
            return *wxBLACK;
    }
}

wxFont DocNotebookBar::GetGuiFont(bool bold) {
    wxFont f = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
    if (bold) {
        // We have to duplicate this font manually to avoid stomping the
        // refcounted system version.  Bleh.
        f = wxFont(f.GetPointSize(), f.GetFamily(), f.GetStyle(), wxBOLD,
                   f.GetUnderlined(), f.GetFaceName(), f.GetEncoding());
    }
    return f;
}

wxCoord DocNotebookBar::UpdateBarHeight() {
    // Calculate the bar height.
    wxCoord junk, text_height;
    {
        wxClientDC dc(this);
        dc.SetFont(GetGuiFont(false));
        dc.GetTextExtent("W", &junk, &text_height);
    }
    wxCoord bar_height = TOP_PAD + text_height + BOTTOM_PAD;

    // Set up our size hints, etc.
    SetSizeHints(-1, bar_height, -1, bar_height);
    if (GetSize().GetHeight() != bar_height)
        SetSize(GetSize().GetWidth(), bar_height);

    // Return our bar height.
    return bar_height;
}

void DocNotebookBar::OnPaint(wxPaintEvent &event) {
    // Get the correct height for our bar.
    wxCoord height = UpdateBarHeight();

    // Begin refresh painting.
    wxPaintDC dc(this);

    // Clear the background.
    dc.SetBackground(GetGuiColor(UNSELECTED_BACKGROUND));
    dc.Clear();

    // Clip our tab drawing area for drawing tabs.
    dc.SetClippingRegion(wxPoint(0, 0), wxSize(GetTabLimit(), height));

    // Draw our tabs.
    wxCoord space_used = mScrollAmount;
    for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++) {
        // Choose our basic drawing parameters.
        if (i == mCurrentTab) {
            dc.SetFont(GetGuiFont(true));
            dc.SetTextBackground(GetGuiColor(SELECTED_BACKGROUND));
            dc.SetTextForeground(GetGuiColor(SELECTED_TEXT));
        } else {
            dc.SetFont(GetGuiFont(false));
            dc.SetTextBackground(GetGuiColor(UNSELECTED_BACKGROUND));
            dc.SetTextForeground(GetGuiColor(UNSELECTED_TEXT));
        }

        // We want a title like "foo.cpp*".
        DocNotebookTab *doc = mNotebook->GetDocument(i);
        wxString title = doc->GetDocumentTitleAndDirtyFlag();

        // Calculate the width of our tab.
        wxCoord text_width;
        dc.GetTextExtent(title, &text_width, NULL);
        wxCoord tab_width = text_width + SIDE_PAD*2;

        // Draw an optional background & top for the selected tab.
        if (i == mCurrentTab) {
            dc.SetBrush(wxBrush(GetGuiColor(SELECTED_BACKGROUND), wxSOLID));
            dc.SetPen(*wxTRANSPARENT_PEN);
            dc.DrawRectangle(space_used, TAB_START, tab_width,
                             height - TAB_START);
            dc.SetPen(wxPen(GetGuiColor(SELECTED_HIGHLIGHT), 1, wxSOLID));
            dc.DrawLine(space_used, TAB_START-1, space_used+tab_width,
                        TAB_START-1);
        }

        // Draw in the actual text.
        dc.DrawText(title, space_used + SIDE_PAD, TOP_PAD);
        space_used += tab_width;

        // Draw a divider.
        wxColor div_color;
        wxCoord div_bottom;
        if (i == mCurrentTab) {
            div_color = GetGuiColor(SELECTED_SHADOW);
            div_bottom = height;
        } else if (i+1 == mCurrentTab) {
            div_color = GetGuiColor(SELECTED_HIGHLIGHT);
            div_bottom = height;
        } else {
            div_color = GetGuiColor(UNSELECTED_DIVIDER);
            div_bottom = height - BOTTOM_PAD;
        }        
        dc.SetPen(wxPen(div_color, 1, wxSOLID));
        dc.DrawLine(space_used, TAB_START, space_used, div_bottom);
        ++space_used;
        
        // Update the rightmost extent of this tab (for hit testing).
        mNotebook->GetDocument(i)->SetTabRightEdge(space_used);
    }

    // Turn off clipping.
    dc.DestroyClippingRegion();

    // Enable and disable our scrolling buttons as needed.
    EnableButton(BUTTON_LEFT, (mScrollAmount < 0), false);
    EnableButton(BUTTON_RIGHT, (space_used > GetTabLimit()), false);

    // Draw our buttons.
    for (size_t i = 0; i < BUTTON_COUNT; i++)
        DrawButton(dc, static_cast<ButtonId>(i));
}

void DocNotebookBar::OnLeftDown(wxMouseEvent &event) {
    wxASSERT(mGrabbedButton == BUTTON_NONE);
    wxPoint click = event.GetPosition();

    // See if the click is in a button.
    ButtonId button = GetButtonForPoint(click);
    if (button != BUTTON_NONE) {
        CaptureMouse();
        mGrabbedButton = button;
        SetButtonStateIfNotDisabled(button, STATE_CLICKED);
        DoButtonPressed(button);
        ::wxWakeUpIdle();
        mNextRepeatTime = ::wxGetLocalTimeMillis() + 200;
        return;
    }

    // See if the click is in a tab.
    if (click.x < GetTabLimit()) {
        for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++) {
            if (click.x < mNotebook->GetDocument(i)->GetTabRightEdge()) {
                mNotebook->SelectDocument(i);
                return;
            }
        }
    }
}

void DocNotebookBar::OnLeftUp(wxMouseEvent &event) {
    if (mGrabbedButton != BUTTON_NONE) {
        wxPoint click = event.GetPosition();
        ButtonId button = GetButtonForPoint(click);
        ButtonId grabbed = mGrabbedButton;

        // Release the mouse.
        mGrabbedButton = BUTTON_NONE;
        ReleaseMouse();

        // If we're over a button, prelight it.
        if (button != BUTTON_NONE)
            SetButtonStateIfNotDisabled(button, STATE_PRELIGHTED);

        if (button == grabbed) {
            // We're over the grabbed button, so call the appropriate
            // action now.
            DoButtonReleased(button);
        } else {
            // We're *not* over the grabbed button, so mark it as released.
            SetButtonStateIfNotDisabled(grabbed, STATE_ENABLED);
        }
    }
}

void DocNotebookBar::OnIdle(wxIdleEvent &event) {
    // Issue repeating commands for buttons which are held down.
    if (mGrabbedButton != BUTTON_NONE) {
        event.RequestMore();
        if (::wxGetLocalTimeMillis() > mNextRepeatTime) {
            ButtonId button =
                GetButtonForPoint(ScreenToClient(::wxGetMousePosition()));
            if (button == mGrabbedButton) {
                DoButtonHeld(button);
                mNextRepeatTime = ::wxGetLocalTimeMillis() + 100;
            }
        }
    }
}

void DocNotebookBar::OnMouseMove(wxMouseEvent &event) {
    // Prelight buttons.
    ButtonId current = GetButtonForPoint(event.GetPosition());
    for (size_t i = 0; i < BUTTON_COUNT; i++) {
        if (mButtonStates[i] != STATE_DISABLED) {
            ButtonState new_state;
            if (current == i && mGrabbedButton == current)
                new_state = STATE_CLICKED;
            else if (current == i && mGrabbedButton == BUTTON_NONE)
                new_state = STATE_PRELIGHTED;
            else
                new_state = STATE_ENABLED;
            SetButtonStateIfNotDisabled(static_cast<ButtonId>(i), new_state);
        }
    }
}

void DocNotebookBar::OnSize(wxSizeEvent &event) {
    Refresh();
}

void DocNotebookBar::DoButtonPressed(ButtonId buttonId) {
    if (!SafeToRunCommandsForButton(buttonId))
        return;
    switch (buttonId) {
        case BUTTON_LEFT: ScrollTabs(100); break;
        case BUTTON_RIGHT: ScrollTabs(-100); break;
    }
}

void DocNotebookBar::DoButtonHeld(ButtonId buttonId) {
    if (!SafeToRunCommandsForButton(buttonId))
        return;
    switch (buttonId) {
        case BUTTON_LEFT: ScrollTabs(30); break;
        case BUTTON_RIGHT: ScrollTabs(-30); break;
    }
}

void DocNotebookBar::DoButtonReleased(ButtonId buttonId) {
    if (!SafeToRunCommandsForButton(buttonId))
        return;
    switch (buttonId) {
        case BUTTON_CLOSE: mNotebook->MaybeCloseTab(); break;
    }
}

/// Set the state of the specified button, redrawing it if requested.
void DocNotebookBar::SetButtonState(ButtonId buttonId, ButtonState state,
                                    bool redraw)
{
    mButtonStates[buttonId] = state;
    if (redraw) {
        wxClientDC dc(this);
        DrawButton(dc, buttonId);
    }
}

/// Only set the button state if the button is not disabled.  We call this
/// to avoid accidentally re-enabling buttons.
void DocNotebookBar::SetButtonStateIfNotDisabled(ButtonId buttonId,
                                                 ButtonState state,
                                                 bool redraw)
{
    if (mButtonStates[buttonId] != STATE_DISABLED)
        SetButtonState(buttonId, state, redraw);
}

/// Enable or disable the specified button.  If the button is already
/// enabled, don't change the enabled/prelighted/clicked state.
void DocNotebookBar::EnableButton(ButtonId buttonId, bool enable,
                                  bool redraw)
{
    if (enable) {
        if (mButtonStates[buttonId] == STATE_DISABLED)
            SetButtonState(buttonId, STATE_ENABLED, redraw);
    } else {
        SetButtonState(buttonId, STATE_DISABLED, redraw);
    }
}

bool DocNotebookBar::SafeToRunCommandsForButton(ButtonId buttonId) {
    return (mNotebook->GetDocumentCount() > 0 && mButtonStates[buttonId] != STATE_DISABLED);
}

void DocNotebookBar::DrawButton(wxDC &dc, ButtonId buttonId) {
    wxASSERT(dc.Ok());
    wxRect r = GetButtonRect(buttonId);
    dc.BeginDrawing();

    // Draw the button's icon.
    dc.SetTextForeground(GetGuiColor(UNSELECTED_TEXT));
    dc.SetTextBackground(GetGuiColor(UNSELECTED_BACKGROUND));
    wxBitmap bitmap = GetButtonBitmap(buttonId);
    dc.DrawBitmap(bitmap,
                  r.GetLeft() + BUTTON_INSIDE_PAD,
                  r.GetTop() + BUTTON_INSIDE_PAD);

    // Draw the button's border.
    wxColor topleft, bottomright;
    switch (mButtonStates[buttonId]) {
        case STATE_PRELIGHTED:
            topleft = GetGuiColor(UNSELECTED_DIVIDER);
            bottomright = GetGuiColor(SELECTED_SHADOW);
            break;
        case STATE_CLICKED:
            topleft = GetGuiColor(SELECTED_SHADOW);
            bottomright = GetGuiColor(UNSELECTED_DIVIDER);
            break;
        default:
            topleft = GetGuiColor(UNSELECTED_BACKGROUND);
            bottomright = GetGuiColor(UNSELECTED_BACKGROUND);
    }
    dc.SetPen(wxPen(topleft, 1, wxSOLID));
    dc.DrawLine(r.GetLeft(), r.GetTop(), r.GetRight()+1, r.GetTop());
    dc.DrawLine(r.GetLeft(), r.GetTop(), r.GetLeft(), r.GetBottom()+1);
    dc.SetPen(wxPen(bottomright, 1, wxSOLID));
    dc.DrawLine(r.GetLeft()+1, r.GetBottom(), r.GetRight()+1, r.GetBottom());
    dc.DrawLine(r.GetRight(), r.GetTop()+1, r.GetRight(), r.GetBottom());

    dc.EndDrawing();
}

wxBitmap DocNotebookBar::GetButtonBitmap(ButtonId buttonId) {
    switch (buttonId) {
        case BUTTON_LEFT:
            if (mButtonStates[buttonId] != STATE_DISABLED)
                return wxBITMAP(tab_left_full);
            else
                return wxBITMAP(tab_left_empty);
        case BUTTON_RIGHT:
            if (mButtonStates[buttonId] != STATE_DISABLED)
                return wxBITMAP(tab_right_full);
            else
                return wxBITMAP(tab_right_empty);
        case BUTTON_CLOSE:
            return wxBITMAP(tab_close);
        default:
            ASSERT(0);
            return wxBitmap();
    }
}

wxRect DocNotebookBar::GetButtonRect(ButtonId buttonId) {
    wxSize sz = GetSize();
    wxCoord right_limit = sz.GetWidth() - SIDE_PAD;
    return wxRect(wxPoint(right_limit - (3-buttonId)*BUTTON_SIZE,
                          (sz.GetHeight() - BUTTON_SIZE) / 2),
                  wxSize(BUTTON_SIZE, BUTTON_SIZE));
}

DocNotebookBar::ButtonId DocNotebookBar::GetButtonForPoint(const wxPoint &p) {
    for (size_t i = 0; i < BUTTON_COUNT; i++)
        if (GetButtonRect(static_cast<ButtonId>(i)).Inside(p))
            return static_cast<ButtonId>(i);
    return BUTTON_NONE;
}

wxCoord DocNotebookBar::GetTabLimit() {
    wxRect r = GetButtonRect(static_cast<ButtonId>(0));
    return (r.GetLeft() - SIDE_PAD);
}

void DocNotebookBar::ScrollTabs(wxCoord pixels) {
    size_t last_doc = mNotebook->GetDocumentCount() - 1;
    wxCoord tab_length =
        -mScrollAmount + mNotebook->GetDocument(last_doc)->GetTabRightEdge();
    wxCoord min_scroll = GetTabLimit() - tab_length;
    mScrollAmount += pixels;
    if (mScrollAmount < min_scroll)
        mScrollAmount = min_scroll;
    if (mScrollAmount > 0)
        mScrollAmount = 0;
    Refresh();
}

