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

#include "TamaleHeaders.h"
#include <wx/file.h>
#include <wx/stc/stc.h>
#include "AppGlobals.h"
#include "ScriptEditor.h"
#include "DocNotebook.h"
#include "BufferSpan.h"
#include "dlg/FindDlg.h"
#include "FiveLApp.h"

// Only so we can find our parent window.
#include "StageFrame.h"


USING_NAMESPACE_FIVEL

/* TODO

Soon:

/ Search selection only
/ Find selection

/ Search state vs multiple buffer issues (mostly)
/ Undo on newly-loaded file should not erase text 

/ Goto line

/ Dirty marks
/ Save, save as
/ Revert
/ Save on close
/ Fix StageFrame::OnClose and OnExit to save files in script editor, too
/ Save on reload
  Reload on change

  Fully implement square braces
  Highlight keywords
  Indent keywords intelligently

  Indent head forms of macros by 4 spaces
  Indent certain macros like functions: AND, OR, PROVIDE?

  Fix brace balancing to handle multiple brace types

/ Fix deletion of last tab
/ Fix window title bar to show file name
  Fix accelerator for Replace All

Next:

  Indexer based:
    Meta-dot (aka "go to definition")
    Buffer index
    Function argument tooltips

*/


namespace {
    wxString kSchemeWildcards(
        "Tamale scripts (*.ss)|*.ss|"
        "All Scheme scripts (*.ss; *.scm)|*.ss;*.scm|"
        "All files|*");
}


//=========================================================================
//  ScriptTextCtrl Definition & Methods
//=========================================================================

class ScriptTextCtrl : public wxStyledTextCtrl {
    enum {
        /// The number of the margin where we put our fold widgets.
        MARGIN_FOLD = 2
    };

    enum {
        /// The location where we started our last search.
        SPAN_SEARCH_START,
        /// The text we found with the last find command.
        SPAN_FOUND_TEXT,
        /// The area within which to search.
        SPAN_SEARCH_LIMITS
    };

    typedef std::vector<FIVEL_NS TScriptIdentifier> IdentifierList;

    /// A range of locations to search.  This is split into two
    /// parts to accomodate wrap.
    struct SearchRange {
        int start1, end1;
        int start2, end2;

        SearchRange(int s1 = wxSTC_INVALID_POSITION,
                    int e1 = wxSTC_INVALID_POSITION,
                    int s2 = wxSTC_INVALID_POSITION,
                    int e2 = wxSTC_INVALID_POSITION)
            : start1(s1), end1(e1), start2(s2), end2(e2) {}
    };

    IdentifierList mIdentifiers;
    BufferSpanTable mSpans;

    bool mReplaceStateUseRegex;
    wxString mReplaceStateText;
    
public:    
    ScriptTextCtrl(wxWindow *parent, wxWindowID id = -1);

    void SetStatusText(const wxString &text);

    bool IsBraceAt(int pos);
    bool IsBraceStyle(int style);
    bool IsBrace(char c);
    bool IsOpenBrace(char c);
    bool IsCloseBrace(char c);
    bool IsIdentifierChar(char c);
    bool IsIdentifierStartChar(char c);
    bool IsTokenStart(int pos);
    std::string GetKeywords();
    IdentifierList GetCompletions(const std::string &partial);

    void OnModified(wxStyledTextEvent &event);
    void OnMarginClick(wxStyledTextEvent &event);
    void OnCharAdded(wxStyledTextEvent &event);
    void OnUpdateTextUI(wxStyledTextEvent &event);
    void OnKeyDown(wxKeyEvent &event);

    void OnUpdateUiAlwaysOn(wxUpdateUIEvent &event);

    void OnUndo(wxCommandEvent &event);
    void OnUpdateUiUndo(wxUpdateUIEvent &event);
    void OnRedo(wxCommandEvent &event);
    void OnUpdateUiRedo(wxUpdateUIEvent &event);
    void OnCut(wxCommandEvent &event);
    void OnUpdateUiCutCopyClear(wxUpdateUIEvent &event);
    void OnCopy(wxCommandEvent &event);
    void OnPaste(wxCommandEvent &event);
    void OnUpdateUiPaste(wxUpdateUIEvent &event);
    void OnClear(wxCommandEvent &event);
    void OnSelectAll(wxCommandEvent &event);

    void OnFind(wxCommandEvent &event);
    void OnFindAgain(wxCommandEvent &event);
    void OnUpdateUiFindAgain(wxUpdateUIEvent &event);
    void OnFindSelection(wxCommandEvent &event);
    void OnUpdateUiFindSelection(wxUpdateUIEvent &event);
    void OnReplace(wxCommandEvent &event);
    void OnReplaceAndFindAgain(wxCommandEvent &event);
    void OnReplaceAll(wxCommandEvent &event);
    void OnUpdateUiReplace(wxUpdateUIEvent &event);
    void OnGotoLine(wxCommandEvent &event);

    void EnsureSelectionVisible();
    void SetSelectionAndEnsureVisible(int start, int end);
    void PutCursorAtTopOfSelection();
    void PutCursorAtBottomOfSelection();
    void InitializeFindState();
    void GetSearchLimits(int &start, int &end);
    SearchRange GetFindSearchRange(bool find_again = false);
    SearchRange GetReplaceAllSearchRange(bool after_original_start_ok);
    bool DoFind(const SearchRange &range, bool interactive = true);
    bool CanReplace();
    void DoReplace(bool interactive = true);
    void DoReplaceAll();

    void AutoCompleteIdentifier();
    void IndentSelection();
    void IndentLine(int inLine);
    int CalculateIndentation(int inParentPos,
                             const std::vector<int> &inSiblingPos);
    int CalculateSyntaxIndentation(int inParentPos,
                                   const std::vector<int> &inSiblingPos);
    int CalculateFunctionIndentation(int inParentPos,
                                     const std::vector<int> &inSiblingPos);
    int GetBaseIndentFromPosition(int inPos);

    DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(ScriptTextCtrl, wxStyledTextCtrl)
    EVT_STC_MODIFIED(-1, ScriptTextCtrl::OnModified)
    EVT_STC_MARGINCLICK(-1, ScriptTextCtrl::OnMarginClick)
    EVT_STC_CHARADDED(-1, ScriptTextCtrl::OnCharAdded)
    EVT_STC_UPDATEUI(-1, ScriptTextCtrl::OnUpdateTextUI)
    EVT_KEY_DOWN(ScriptTextCtrl::OnKeyDown)

    // Edit menu.
    EVT_MENU(wxID_UNDO, ScriptTextCtrl::OnUndo)
    EVT_UPDATE_UI(wxID_UNDO, ScriptTextCtrl::OnUpdateUiUndo)
    EVT_MENU(wxID_REDO, ScriptTextCtrl::OnRedo)
    EVT_UPDATE_UI(wxID_REDO, ScriptTextCtrl::OnUpdateUiRedo)
    EVT_MENU(wxID_CUT, ScriptTextCtrl::OnCut)
    EVT_UPDATE_UI(wxID_CUT, ScriptTextCtrl::OnUpdateUiCutCopyClear)
    EVT_MENU(wxID_COPY, ScriptTextCtrl::OnCopy)
    EVT_UPDATE_UI(wxID_COPY, ScriptTextCtrl::OnUpdateUiCutCopyClear)
    EVT_MENU(wxID_PASTE, ScriptTextCtrl::OnPaste)
    EVT_UPDATE_UI(wxID_PASTE, ScriptTextCtrl::OnUpdateUiPaste)
    EVT_MENU(wxID_CLEAR, ScriptTextCtrl::OnClear)
    EVT_UPDATE_UI(wxID_CLEAR, ScriptTextCtrl::OnUpdateUiCutCopyClear)
    EVT_MENU(wxID_SELECTALL, ScriptTextCtrl::OnSelectAll)
    EVT_UPDATE_UI(wxID_SELECTALL, ScriptTextCtrl::OnUpdateUiAlwaysOn)

    // Search menu.
    EVT_MENU(wxID_FIND, ScriptTextCtrl::OnFind)
    EVT_UPDATE_UI(wxID_FIND, ScriptTextCtrl::OnUpdateUiAlwaysOn)
    EVT_MENU(FIVEL_FIND_AGAIN, ScriptTextCtrl::OnFindAgain)
    EVT_UPDATE_UI(FIVEL_FIND_AGAIN, ScriptTextCtrl::OnUpdateUiFindAgain)
    EVT_MENU(FIVEL_FIND_SELECTION, ScriptTextCtrl::OnFindSelection)
    EVT_UPDATE_UI(FIVEL_FIND_SELECTION,
                  ScriptTextCtrl::OnUpdateUiFindSelection)
    EVT_MENU(wxID_REPLACE, ScriptTextCtrl::OnReplace)
    EVT_UPDATE_UI(wxID_REPLACE, ScriptTextCtrl::OnUpdateUiReplace)
    EVT_MENU(FIVEL_REPLACE_AND_FIND_AGAIN,
             ScriptTextCtrl::OnReplaceAndFindAgain)
    EVT_UPDATE_UI(FIVEL_REPLACE_AND_FIND_AGAIN,
                  ScriptTextCtrl::OnUpdateUiReplace)
    EVT_MENU(wxID_REPLACE_ALL, ScriptTextCtrl::OnReplaceAll)
    EVT_UPDATE_UI(wxID_REPLACE_ALL, ScriptTextCtrl::OnUpdateUiFindAgain)
    EVT_MENU(FIVEL_GOTO_LINE, ScriptTextCtrl::OnGotoLine)
    EVT_UPDATE_UI(FIVEL_GOTO_LINE, ScriptTextCtrl::OnUpdateUiAlwaysOn)
END_EVENT_TABLE()

ScriptTextCtrl::ScriptTextCtrl(wxWindow *parent, wxWindowID id)
    : wxStyledTextCtrl(parent, id)
{
    // If possible, fetch an identifier list.
    if (TInterpreter::HaveInstance())
        mIdentifiers = TInterpreter::GetInstance()->GetKnownIdentifiers();

    // Set up some basic text editing parameters.
    SetTabWidth(4);
    SetUseTabs(false);
    SetTabIndents(true);
    SetEdgeMode(wxSTC_EDGE_BACKGROUND);
    SetEdgeColour(*wxRED);
    SetEdgeColumn(79);
    AutoCompSetTypeSeparator(')'); // Safe for Scheme.

    // Set up parameters affecting EnsureVisibleEnforcePolicy.  This
    // makes sure search results, etc., are on screen.
    SetVisiblePolicy(wxSTC_CARET_SLOP, 10);

    // Let Scintilla know which modification events we want.
    SetModEventMask(GetModEventMask()
                    | wxSTC_MOD_INSERTTEXT | wxSTC_MOD_DELETETEXT);

    // Set up default style attributes and copy them to all styles.
    StyleSetFont(wxSTC_STYLE_DEFAULT,
                 wxFont(12, wxMODERN, wxNORMAL, wxNORMAL));
    StyleClearAll();

    // Choose our language and set up syntax highlighting.
    SetLexer(wxSTC_LEX_LISP);
    StyleSetBold(wxSTC_STYLE_BRACELIGHT, true);
    StyleSetForeground(wxSTC_STYLE_BRACELIGHT, *wxBLUE);
    StyleSetBackground(wxSTC_STYLE_BRACELIGHT, wxColor(0xD0, 0xD0, 0xD0));
    StyleSetBold(wxSTC_STYLE_BRACEBAD, true);
    StyleSetForeground(wxSTC_STYLE_BRACEBAD, *wxRED);
    StyleSetForeground(wxSTC_LISP_COMMENT, wxColor(0xC0, 0x00, 0x00));
    StyleSetForeground(wxSTC_LISP_KEYWORD, wxColor(0x00, 0x00, 0xC0));
    StyleSetForeground(wxSTC_LISP_STRING, wxColor(0x80, 0x80, 0x80));
    StyleSetForeground(wxSTC_LISP_STRINGEOL, *wxRED);
    // Boring: wxSTC_LISP_NUMBER, wxSTC_LISP_IDENTIFIER, wxSTC_LISP_OPERATOR

    // Set up folding.
    //SetStyleBits(5); Not actually necessary...
    SetProperty("fold", "1");
    SetMarginWidth(MARGIN_FOLD, 0);
    SetMarginType(MARGIN_FOLD, wxSTC_MARGIN_SYMBOL);
    SetMarginMask(MARGIN_FOLD, wxSTC_MASK_FOLDERS);
    SetMarginWidth(MARGIN_FOLD, 20);
    SetMarginSensitive(MARGIN_FOLD, true);
    MarkerDefine(wxSTC_MARKNUM_FOLDER, wxSTC_MARK_PLUS);
    MarkerDefine(wxSTC_MARKNUM_FOLDEROPEN, wxSTC_MARK_MINUS);
    MarkerDefine(wxSTC_MARKNUM_FOLDEREND, wxSTC_MARK_EMPTY);
    MarkerDefine(wxSTC_MARKNUM_FOLDERMIDTAIL, wxSTC_MARK_EMPTY);
    MarkerDefine(wxSTC_MARKNUM_FOLDEROPENMID, wxSTC_MARK_EMPTY);
    MarkerDefine(wxSTC_MARKNUM_FOLDERSUB, wxSTC_MARK_EMPTY);
    MarkerDefine(wxSTC_MARKNUM_FOLDERTAIL, wxSTC_MARK_EMPTY);
    SetFoldFlags(16); // Draw fold line...

    // Keywords.
    SetKeyWords(0, GetKeywords().c_str());
}

void ScriptTextCtrl::SetStatusText(const wxString &text) {
    // Walk up our containing hierarchy, looking for a wxFrame object.
    wxFrame *frame = NULL;
    for (wxWindow *window = GetParent();
         window && !(frame = wxDynamicCast(window, wxFrame));
         window = window->GetParent())
        ;

    // If we found a frame, show our status text.
    frame->SetStatusText(text);
}

bool ScriptTextCtrl::IsBraceAt(int pos) {
    return (IsBraceStyle(GetStyleAt(pos)) && IsBrace(GetCharAt(pos)));
}

/// Is this the style which contains all real braces?  (Not all characters
/// with the style will be braces, however.)
bool ScriptTextCtrl::IsBraceStyle(int style) {
    /// \todo Scheme-specific.
    return (style == wxSTC_LISP_OPERATOR);
}

bool ScriptTextCtrl::IsBrace(char c) {
    return IsOpenBrace(c) || IsCloseBrace(c);
}

bool ScriptTextCtrl::IsOpenBrace(char c) {
    return (c == '(' || c == '[' || c == '{' || c == '<');
}

bool ScriptTextCtrl::IsCloseBrace(char c) {
    return (c == ')' || c == ']' || c == '}' || c == '>');
}

bool ScriptTextCtrl::IsIdentifierChar(char c) {
    return IsIdentifierStartChar(c) || isdigit(c);
}

bool ScriptTextCtrl::IsIdentifierStartChar(char c) {
    if (isalpha(c))
        return true;
    switch (c) {
        /// \todo Scheme-specific.
        case '!': case '$': case '%': case '&': case '*': case '+':
        case '-': case '.': case '/': case ':': case '<': case '=':
        case '>': case '?': case '@': case '^': case '_': case '~':
            return true;
        
        default:
            return false;
     }
}

bool ScriptTextCtrl::IsTokenStart(int pos) {
    // First, make sure we have a token character at pos.
    char c = GetCharAt(pos);
    if (isspace(c) || IsBrace(c))
        return false;
    int style = GetStyleAt(pos);
    if (style == wxSTC_LISP_COMMENT)
        return false;

    // Now look at the previous character.
    if (pos == 0)
        // Ooops.  No previous character.
        return true;
    char prev_c = GetCharAt(pos - 1);
    if (!isspace(prev_c) && !IsBrace(prev_c))
        return false;
    int prev_style = GetStyleAt(pos - 1);
    if (prev_style == wxSTC_LISP_STRING ||
        prev_style == wxSTC_LISP_STRINGEOL)
        return false;
    return true;
}
 
std::string ScriptTextCtrl::GetKeywords() {
	std::string result;
    IdentifierList::iterator i = mIdentifiers.begin();
    for (; i != mIdentifiers.end(); ++i) {
        if (i->GetType() == TScriptIdentifier::KEYWORD) {
            if (result.size() != 0)
                result += " ";
            result += i->GetName();
        }
    }
    return result;
}

ScriptTextCtrl::IdentifierList
ScriptTextCtrl::GetCompletions(const std::string &partial) {
    IdentifierList result;
    IdentifierList::iterator i = mIdentifiers.begin();
    for (; i != mIdentifiers.end(); ++i) {
        std::string name = i->GetName();
        if (name.size() < partial.size())
            continue;
        if (name.compare(0, partial.size(), partial) == 0)
            result.push_back(*i);
    }
    return result;
}

void ScriptTextCtrl::OnModified(wxStyledTextEvent &event) {
    int mod_type = event.GetModificationType();

    // Clear our status text on modifications.
    /// \todo Clear status text sooner?
    if (mod_type & (wxSTC_MOD_INSERTTEXT|wxSTC_MOD_DELETETEXT))
        SetStatusText("");

    // If inserts and deletes come in the same event, we're in trouble.
    ASSERT(!((mod_type & wxSTC_MOD_INSERTTEXT) &&
             (mod_type & wxSTC_MOD_DELETETEXT)));
    
    // Maintain our list of floating spans as text is inserted and removed.
    if (mod_type & wxSTC_MOD_INSERTTEXT)
        mSpans.ProcessInsertion(event.GetPosition(), event.GetLength());
    else if (mod_type & wxSTC_MOD_DELETETEXT)
        mSpans.ProcessDeletion(event.GetPosition(), event.GetLength());
}

void ScriptTextCtrl::OnMarginClick(wxStyledTextEvent &event) {
    if (event.GetMargin() == MARGIN_FOLD) {
        int line = LineFromPosition(event.GetPosition());
        ToggleFold(line);
    }   
}

void ScriptTextCtrl::OnCharAdded(wxStyledTextEvent &event) {
    // Sanity-check our buffer position--we should be immediately
    // *after* the inserted character.
	int pos = GetCurrentPos();
    if (pos < 1)
        return;

    // Get the inserted character.
    char inserted = GetCharAt(pos - 1);

    // Update our autocompletion state as needed.
    if (IsIdentifierChar(inserted))
        AutoCompleteIdentifier();
    else
        AutoCompCancel();

    // Auto-indent new lines.
    if (inserted == '\n' || inserted == '\r')
        IndentSelection();
}

void ScriptTextCtrl::OnUpdateTextUI(wxStyledTextEvent &event) {
    // Find a brace, and perhaps a matching brace.
    int pos = GetCurrentPos();
    int brace_pos = wxSTC_INVALID_POSITION;
    if (pos > 0 && IsBraceAt(pos - 1))
        // Give preference to brace at left of caret.
        brace_pos = pos - 1;
    else if (IsBraceAt(pos))
        // Fall back to brace at right of caret.
        brace_pos = pos;
    int brace_match_pos = wxSTC_INVALID_POSITION;
    if (brace_pos != wxSTC_INVALID_POSITION)
        /// \bug BraceMatch doesn't balance multiple brace types correctly.
        brace_match_pos = BraceMatch(brace_pos);

    // Highlight braces appropriately.
    if (brace_match_pos != wxSTC_INVALID_POSITION)
        BraceHighlight(brace_pos, brace_match_pos);
    else 
        BraceBadLight(brace_pos); // Handles unhighlighting, too.
}

void ScriptTextCtrl::OnKeyDown(wxKeyEvent &event) {
    if (event.GetKeyCode() == WXK_TAB && !AutoCompActive() &&
        !event.ControlDown() && !event.AltDown() &&
        !event.MetaDown() && !event.ShiftDown()) {

        // Handle TAB ourselves, before Scintilla sees it.
        IndentSelection();
    } else if (event.GetKeyCode() == '+' &&
               event.ControlDown() && !event.AltDown() &&
               !event.MetaDown() && !event.ShiftDown()) {
        // HACK - Under Windows, a keypress of Ctrl-= gets sent as Ctrl-+,
        // causing our Ctrl-= accelerator to fail.  Apply a cluestick of
        // dubious origins.
        /// \todo Move this code up to ScriptEditor class?
        wxUpdateUIEvent update(wxID_REPLACE);
        if (ProcessEvent(update) && update.GetEnabled()) {            
            wxCommandEvent command(wxEVT_COMMAND_MENU_SELECTED, wxID_REPLACE);
            ProcessEvent(command);
        }
    } else {
        event.Skip();
    }
}

void ScriptTextCtrl::OnUpdateUiAlwaysOn(wxUpdateUIEvent &event) {
    event.Enable(true);
}

void ScriptTextCtrl::OnUndo(wxCommandEvent &event) {
    Undo();
}

void ScriptTextCtrl::OnUpdateUiUndo(wxUpdateUIEvent &event) {
    event.Enable(CanUndo());
}

void ScriptTextCtrl::OnRedo(wxCommandEvent &event) {
    Redo();
}

void ScriptTextCtrl::OnUpdateUiRedo(wxUpdateUIEvent &event) {
    event.Enable(CanRedo());
}

void ScriptTextCtrl::OnCut(wxCommandEvent &event) {
    Cut();
}

void ScriptTextCtrl::OnUpdateUiCutCopyClear(wxUpdateUIEvent &event) {
    int begin, end;
    GetSelection(&begin, &end);
    event.Enable(begin != end);
}

void ScriptTextCtrl::OnCopy(wxCommandEvent &event) {
    Copy();
}

void ScriptTextCtrl::OnPaste(wxCommandEvent &event) {
    Paste();
}

void ScriptTextCtrl::OnUpdateUiPaste(wxUpdateUIEvent &event) {
    event.Enable(CanPaste());
}

void ScriptTextCtrl::OnClear(wxCommandEvent &event) {
    Clear();
}

void ScriptTextCtrl::OnSelectAll(wxCommandEvent &event) {
    SelectAll();
}

void ScriptTextCtrl::OnFind(wxCommandEvent &event) {
    FindDlg dlg(this, GetSelectionStart() != GetSelectionEnd());
    
    int button = dlg.ShowModal();
    if (button != wxID_CANCEL) {
        // Set up some basic search parameters.
        InitializeFindState();
        
        // Run an appropriate search depending on which button was clicked.
        if (button == XRCID("DLG_FIND")) {
            DoFind(GetFindSearchRange());
        } else if (button == XRCID("DLG_REPLACE")) {
            if (DoFind(GetFindSearchRange()))
                DoReplace();
        } else if (button == XRCID("DLG_REPLACE_ALL")) {
            DoReplaceAll();
        } else if (button == XRCID("DLG_DONT_FIND")) {
            // Do nothing.
        }
    }
}

void ScriptTextCtrl::OnFindAgain(wxCommandEvent &event) {
    DoFind(GetFindSearchRange(true));
}

void ScriptTextCtrl::OnUpdateUiFindAgain(wxUpdateUIEvent &event) {
    event.Enable(FindDlg::GetSearchText() != "");
}

void ScriptTextCtrl::OnFindSelection(wxCommandEvent &event) {
    PutCursorAtBottomOfSelection();
    FindDlg::SetUseRegex(false);
    FindDlg::SetSearchText(GetSelectedText());
    FindDlg::SetSearchArea(FindDlg::CURRENT_SCRIPT);
    InitializeFindState();
    DoFind(GetFindSearchRange());
}

void ScriptTextCtrl::OnUpdateUiFindSelection(wxUpdateUIEvent &event) {
    event.Enable(GetSelectionStart() != GetSelectionEnd());
}

void ScriptTextCtrl::OnReplace(wxCommandEvent &event) {
    DoReplace();
}

void ScriptTextCtrl::OnReplaceAndFindAgain(wxCommandEvent &event) {
    DoReplace();
    DoFind(GetFindSearchRange(true));
}

void ScriptTextCtrl::OnReplaceAll(wxCommandEvent &event) {
    DoReplaceAll();
}

void ScriptTextCtrl::OnUpdateUiReplace(wxUpdateUIEvent &event) {
    event.Enable(CanReplace());
}

void ScriptTextCtrl::OnGotoLine(wxCommandEvent &event) {
    wxTextEntryDialog dlg(this, "Go to Line", "Line:");
    if (dlg.ShowModal() == wxID_OK) {
        wxString lineStr = dlg.GetValue();
        long line;
        if (lineStr.ToLong(&line)
            && 0 < line && line <= GetLineCount())
        {
            GotoLine(line-1);
            EnsureVisibleEnforcePolicy(line-1);
        } else {
            ::wxBell();
            SetStatusText("No such line.");
        }
    }
}

void ScriptTextCtrl::EnsureSelectionVisible() {
    int start_line = LineFromPosition(GetSelectionStart());
    int end_line = LineFromPosition(GetSelectionEnd());
    for (int i = start_line; i <= end_line; i++)
        EnsureVisible(i);
    EnsureVisibleEnforcePolicy(start_line);
}

void ScriptTextCtrl::SetSelectionAndEnsureVisible(int start, int end) {
    SetSelectionStart(start);
    SetSelectionEnd(end);
    EnsureSelectionVisible();
}

/// Make sure the cursor is at the top of the selection, not the bottom.
/// This is needed for searching within the current selection.
void ScriptTextCtrl::PutCursorAtTopOfSelection() {
    int start = GetSelectionStart();
    int end = GetSelectionEnd();
    SetAnchor(end);
    SetCurrentPos(start);
}

/// Make sure the cursor is at the bottom of the selection, not the top.
/// This is needed for searching for the current selection.
void ScriptTextCtrl::PutCursorAtBottomOfSelection() {
    int start = GetSelectionStart();
    int end = GetSelectionEnd();
    SetAnchor(start);
    SetCurrentPos(end);
}

void ScriptTextCtrl::InitializeFindState()
{
    // Set up some basic search parameters.
    if (FindDlg::GetSearchArea() == FindDlg::SELECTION_ONLY) {
        mSpans.SetSpan(BufferSpan(SPAN_SEARCH_LIMITS,
                                  GetSelectionStart(),
                                  GetSelectionEnd()));
        mSpans.SetSpan(BufferSpan(SPAN_SEARCH_START,
                                  GetSelectionStart()));
        // The cursor could be at either end of the selection.
        // We want it at the top.
        PutCursorAtTopOfSelection();
    } else {
        mSpans.DeleteSpanIfExists(SPAN_SEARCH_LIMITS);
        if (FindDlg::GetStartAtTop())
            mSpans.SetSpan(BufferSpan(SPAN_SEARCH_START, 0));
        else
            mSpans.SetSpan(BufferSpan(SPAN_SEARCH_START,
                                      GetCurrentPos()));
    }
}

void ScriptTextCtrl::GetSearchLimits(int &start, int &end) {
    const BufferSpan *span = mSpans.FindSpan(SPAN_SEARCH_LIMITS);
    if (FindDlg::GetSearchArea() != FindDlg::SELECTION_ONLY
        || !span
        || span->GetStatus() == BufferSpan::CLOBBERED)
    {
        start = 0;
        end = GetTextLength();
    }  else {
        start = span->GetBeginPos();
        end = span->GetEndPos();
    }
}

ScriptTextCtrl::SearchRange
ScriptTextCtrl::GetFindSearchRange(bool find_again) {
    int start_limit, end_limit, start;
    GetSearchLimits(start_limit, end_limit);
    if (find_again)
        start = GetCurrentPos();
    else
        start = mSpans.GetSpan(SPAN_SEARCH_START)->GetBeginPos();
    return SearchRange(start, end_limit, start_limit, start);
}

ScriptTextCtrl::SearchRange
ScriptTextCtrl::GetReplaceAllSearchRange(bool after_original_start_ok) {
    int start_limit, end_limit;
    GetSearchLimits(start_limit, end_limit);
    int original_start = mSpans.GetSpan(SPAN_SEARCH_START)->GetBeginPos();
    int start = GetCurrentPos();

    if (start < original_start) {
        // We're approaching our original start point from behind.
        return SearchRange(start, original_start);
    } else if (!after_original_start_ok) {
        // We're after the original start point, and it's not OK to be here.
        // Bail now before we start looping.
        return SearchRange();
    } else {
        // We're on or after the original start point, and it's OK to be
        // here.  Search normally.
        return SearchRange(start, end_limit, start_limit, original_start);
    }
}

bool ScriptTextCtrl::DoFind(const SearchRange &range, bool interactive) {
    // Figure out our Find flags.
    int flags = 0;
    if (FindDlg::GetCaseSensitive())
        flags |= wxSTC_FIND_MATCHCASE;
    if (FindDlg::GetMatchEntireWords())
        flags |= wxSTC_FIND_WHOLEWORD;
    if (FindDlg::GetUseRegex())
        flags |= wxSTC_FIND_REGEXP|wxSTC_FIND_POSIX;

    // Run the Find command.
    wxString text = FindDlg::GetSearchText();
	int match_start = wxSTC_INVALID_POSITION,
        match_end = wxSTC_INVALID_POSITION;
    if (range.start1 != wxSTC_INVALID_POSITION)
        FindText(range.start1, range.end1, text, flags,
                 &match_start, &match_end);

    // Wrap around, if that's what we need to do.
    if (match_start == wxSTC_INVALID_POSITION &&
        range.start2 != wxSTC_INVALID_POSITION &&
        FindDlg::GetWrapAround())
    {
        FindText(range.start2, range.end2, text, flags,
                 &match_start, &match_end);
    }

    // Either highlight what we found, or beep annoyingly.
    if (match_start == wxSTC_INVALID_POSITION) {
        mSpans.DeleteSpanIfExists(SPAN_FOUND_TEXT);
        if (interactive) {
            SetStatusText("Not found.");
            ::wxBell();
        }
        return false;
    } else {
        // Remember what we found, so we can replace it.  This gives
        // us some hope of surviving changes to global search state in
        // other buffers.
        mSpans.SetSpan(BufferSpan(SPAN_FOUND_TEXT, match_start, match_end));
        mReplaceStateUseRegex = FindDlg::GetUseRegex();
        mReplaceStateText = FindDlg::GetReplaceText();

        if (interactive)
            SetSelectionAndEnsureVisible(match_start, match_end);
        return true;
    }
}

bool ScriptTextCtrl::CanReplace() {
    /// \bug Handle changes of search text in other buffers.
    const BufferSpan *found = mSpans.FindSpan(SPAN_FOUND_TEXT);
    return (found
            && found->GetStatus() == BufferSpan::UNCHANGED
            && found->GetBeginPos() == GetSelectionStart()
            && found->GetEndPos() == GetSelectionEnd()
            && FindDlg::GetSearchText() != "");
}

/// Replace the selected text.  We assume that a find has just been
/// performed, and that the selection marks the result of the Find.
void ScriptTextCtrl::DoReplace(bool interactive) {
    // If this is an interactive search, we'd better have met our usual
    // replacement conditions.  Non-interactive searches don't set up
    // the selection, so CanReplace() doesn't apply to them.
    ASSERT(!interactive || CanReplace());

    // Set our replace target to the most recently found text.
    const BufferSpan *found = mSpans.GetSpan(SPAN_FOUND_TEXT);
    SetTargetStart(found->GetBeginPos());
    SetTargetEnd(found->GetEndPos());

    // Do the replacement.
    if (mReplaceStateUseRegex) {
        ReplaceTargetRE(mReplaceStateText);
    } else {
        ReplaceTarget(mReplaceStateText);
    }

    SetSelection(GetTargetStart(), GetTargetEnd());
    if (interactive)
        EnsureSelectionVisible();
}

/// Replace all instances of the selected text.
void ScriptTextCtrl::DoReplaceAll() {
    BeginUndoAction();

    // Replace all instances of the search text.
    size_t count = 0;
    bool after_original_start_ok = true;
    while (DoFind(GetReplaceAllSearchRange(after_original_start_ok), false)) {
        // Infinite loop protection: After we've replaced something before
        // the official start point, we can't go beyond it again.
        int original_start = mSpans.GetSpan(SPAN_SEARCH_START)->GetBeginPos();
        if (GetSelectionStart() < original_start)
            after_original_start_ok = false;

        // Do the replacement.
        DoReplace(false);
        count++;
    }

    EndUndoAction();
    
    // Show our final replacement.
    EnsureSelectionVisible();

    // Tell the user how many copies we replaced.
    wxString str;
    str.Printf("%d occurences replaced.", count);
    SetStatusText(str);
}

void ScriptTextCtrl::AutoCompleteIdentifier() {
    // Parse the current name, backwards and forwards.
    int pos;
    wxString line = GetCurLine(&pos);
    ASSERT(pos >= 1 && IsIdentifierChar(line[pos-1]));
    int word_begin = pos;
    while (word_begin > 0 && IsIdentifierChar(line[word_begin-1]))
        word_begin--;
    int word_end = pos;
    while (static_cast<size_t>(word_end) < line.length() &&
		   IsIdentifierChar(line[word_end]))
        word_end++;
    if (pos < word_end)
        return;
    if (!IsIdentifierStartChar(line[word_begin]))
        return;
    
    // If we're adding characters at the end of a name, find any known
    // commands which begin with the name.
    wxString prefix = line.Mid(word_begin, word_end - word_begin);
    IdentifierList candidates = GetCompletions(prefix.mb_str());
    if (candidates.size() < 1 || candidates.size() > 50) {
        AutoCompCancel();
        return;
    }

    // Assemble the candidates into a single string.
    std::string candidate_str;
    IdentifierList::iterator i = candidates.begin();
    for (; i != candidates.end(); ++i) {
        if (candidate_str.size() != 0)
            candidate_str += " ";
        candidate_str += i->GetName();
    }

    // Pop up a completion doo-hicky with the candidates.
    AutoCompShow(pos - word_begin, candidate_str.c_str());    
}

void ScriptTextCtrl::IndentSelection() {
    // Figure out which lines are in the selection.
    int begin_pos, end_pos;
    GetSelection(&begin_pos, &end_pos);
    int begin_line = LineFromPosition(begin_pos);
    int end_line = LineFromPosition(end_pos);

    // Indent each line in the selection.
    for (int i = begin_line; i <= end_line; i++)
        IndentLine(i);

    // Figure out where to put the new selection.
    if (begin_pos == end_pos) {
        // We had a caret, not a selection, so figure out where the caret
        // is now (and reposition it if needed).
        int pos = GetCurrentPos();
        int line = LineFromPosition(pos);
        int line_start = PositionFromLine(line);
        int indent = GetLineIndentation(begin_line);
        if (pos - line_start < indent) {
            int pos_after_indent = line_start + indent;
            SetSelectionStart(pos_after_indent);
            SetSelectionEnd(pos_after_indent);
        }
    } else {
        // We had a selection (which will have gotten completely
        // clobbered), so re-select all the lines we indented.
        SetSelectionStart(PositionFromLine(begin_line));
        SetSelectionEnd(GetLineEndPosition(end_line));
    }
}

void ScriptTextCtrl::IndentLine(int inLine) {
    // Scan backwards.  Every time we see a close brace, add 1 to our nest
    // count.  Every time we see an open brace:
    //   1. If our count is > 1, subtract 1 from our nest count.
    //   2. If our count == 1, subtract 1 and remember as a sibling form.
    //   3. If our count == 0, we've found our parent.
    int nest_level = 0;
    int parent_pos = wxSTC_INVALID_POSITION;
    std::vector<int> sibling_pos;
    bool done = false;
    for (int rpos = PositionFromLine(inLine); !done && rpos > 0; rpos--)
    {
        int pos = rpos - 1;
        if (IsBraceAt(pos)) {
            char c = GetCharAt(pos);
            if (IsCloseBrace(c)) {
                nest_level++;
            } else /* is open brace */ {
                switch (nest_level) {
                    case 0:
                        parent_pos = pos;
                        done = true;
                        break;
                    case 1:
                        sibling_pos.push_back(pos);
                        // Fall through.
                    default:
                        nest_level--;
                }
            }
        } else if (nest_level == 0 && IsTokenStart(pos)) {
            // We've found a non-parenthesized sibling subform...
            sibling_pos.push_back(pos);
        }
    }
    std::reverse(sibling_pos.begin(), sibling_pos.end());

    // Do the actual indentation.
    SetLineIndentation(inLine, CalculateIndentation(parent_pos, sibling_pos));
    /// \todo Handle :foo function arguments correctly.
}

int ScriptTextCtrl::CalculateIndentation(int inParentPos,
                                         const std::vector<int> &inSiblingPos)
{
    if (inParentPos == wxSTC_INVALID_POSITION) {
        // We're a top-level form, so indent to 0.
        return 0;
    } else {
        int parent_indent = GetBaseIndentFromPosition(inParentPos);

        // This shouldn't happen, but we'll handle it anyway.
        if (inParentPos + 1 >= GetTextLength())
            return (parent_indent + 2);

        // Choose an appropriate indentation algorithm based on what's to the
        // right of our parent's open paren.
        switch (GetStyleAt(inParentPos + 1)) {
            // In all of these cases, we should be in a list of literals,
            // or something else we wouldn't normally indent much.
            case wxSTC_LISP_DEFAULT:
            case wxSTC_LISP_COMMENT:
            case wxSTC_LISP_NUMBER:
            case wxSTC_LISP_STRING:
            case wxSTC_LISP_STRINGEOL:
                return (parent_indent + 1);
                
            // Special forms and macros require lots of special-case
            // processing.
            case wxSTC_LISP_KEYWORD:
                return CalculateSyntaxIndentation(inParentPos, inSiblingPos);

            // These *might* be function calls, so use the standard rule.
            case wxSTC_LISP_IDENTIFIER:
            case wxSTC_LISP_OPERATOR:
			default:
                return CalculateFunctionIndentation(inParentPos, inSiblingPos);
        }
    }
}

int ScriptTextCtrl::CalculateSyntaxIndentation(
    int inParentPos,
    const std::vector<int> &inSiblingPos)
{
    /// \todo Count sub-forms correctly.
    return (GetBaseIndentFromPosition(inParentPos) + 2);
}

int ScriptTextCtrl::CalculateFunctionIndentation(
    int inParentPos,
    const std::vector<int> &inSiblingPos)
{
    if (inSiblingPos.size() < 2) {
        // We're on a new line, and we had no previous siblings besides
        // (possibly) the first subform, so assume one-space indentation.
        // We do this because (1) we might actually be a list of literals,
        // not a function call and (2) if we are a function call, the
        // indentation situation is presumably pretty dire.
        return (GetBaseIndentFromPosition(inParentPos) + 1);
    } else {
        // Reuse the indentation of the second sibling, which will typically
        // be the first argument to a function.
        return GetBaseIndentFromPosition(inSiblingPos[1]);
    }
}

int ScriptTextCtrl::GetBaseIndentFromPosition(int inPos) {
    int line_start = PositionFromLine(LineFromPosition(inPos));
    return (inPos - line_start);
}

//=========================================================================
//  ScriptDoc Definition & Methods
//=========================================================================

class ScriptDoc : public ScriptTextCtrl, public DocNotebookTab {
public:
    ScriptDoc(DocNotebook *parent, wxWindowID id = -1);
    ScriptDoc(DocNotebook *parent, wxWindowID id,
              const wxString &path);

    virtual wxWindow *GetDocumentWindow() { return this; }

protected:
    virtual bool SaveDocument(bool force);

private:
    void SetTitleAndPath(const wxString &fullPath);

    void WriteDocument();
    void ReadDocument();

    bool DoSaveAs(const wxString &dialogTitle,
                  const wxString &defaultDir,
                  const wxString &defaultName);

    void OnSave(wxCommandEvent &event);
    void OnUpdateUiSave(wxUpdateUIEvent &event);
    void OnSaveAs(wxCommandEvent &event);
    void OnUpdateUiSaveAs(wxUpdateUIEvent &event);
    void OnRevert(wxCommandEvent &event);
    void OnUpdateUiRevert(wxUpdateUIEvent &event);

    void OnSavePointReached(wxStyledTextEvent &event);
    void OnSavePointLeft(wxStyledTextEvent &event);

    DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(ScriptDoc, ScriptTextCtrl)
    EVT_MENU(wxID_SAVE, ScriptDoc::OnSave)
    EVT_UPDATE_UI(wxID_SAVE, ScriptDoc::OnUpdateUiSave)
    EVT_MENU(wxID_SAVEAS, ScriptDoc::OnSaveAs)
    EVT_UPDATE_UI(wxID_SAVEAS, ScriptDoc::OnUpdateUiSaveAs)
    EVT_MENU(wxID_REVERT, ScriptDoc::OnRevert)
    EVT_UPDATE_UI(wxID_REVERT, ScriptDoc::OnUpdateUiRevert)

    EVT_STC_SAVEPOINTREACHED(-1, ScriptDoc::OnSavePointReached)
    EVT_STC_SAVEPOINTLEFT(-1, ScriptDoc::OnSavePointLeft)
END_EVENT_TABLE()

ScriptDoc::ScriptDoc(DocNotebook *parent, wxWindowID id)
    : ScriptTextCtrl(parent, id), DocNotebookTab(parent)
{
    SetDocumentTitle(parent->GetNextUntitledDocumentName());
}

ScriptDoc::ScriptDoc(DocNotebook *parent, wxWindowID id,
                     const wxString &path)
    : ScriptTextCtrl(parent, id), DocNotebookTab(parent)
{
    SetTitleAndPath(path);
    ReadDocument();
}

void ScriptDoc::SetTitleAndPath(const wxString &fullPath) {
    SetDocumentPath(fullPath);

    // Use the (short) filename as the document name.
    SetDocumentTitle(wxFileName(fullPath).GetFullName());
}

void ScriptDoc::WriteDocument() {
    wxString path(GetDocumentPath());
    wxFile file(path, wxFile::write);
    if (!file.IsOpened())
        THROW(("Error opening file: "+path).mb_str());
    wxString text(GetText());
    if (!file.Write(text))
        THROW(("Error writing from file: "+path).mb_str());
    /// \todo How does wxWindows deal with errors returned
    /// from close()?  These usually indicate that a previous
    /// write failed.

    // Set a save point so Scintilla knows that the document isn't dirty.
    SetSavePoint();
}

void ScriptDoc::ReadDocument() {
    // Read the data into our buffer.  (We can probably do this with
    // less copying if we work at it.)
    wxString path(GetDocumentPath());
    wxFile file(path);
    if (!file.IsOpened())
        THROW(("Error opening file: "+path).mb_str());
    off_t length = file.Length();
    std::vector<char> data(length);
    if (file.Read(&data[0], length) != length)
        THROW(("Error reading from file: "+path).mb_str());
    SetText(wxString(&data[0], length));

    // Don't let the user undo document setup.
    EmptyUndoBuffer();

    // Mark the document as saved.
    SetSavePoint();
}

bool ScriptDoc::SaveDocument(bool force) {
    if (GetDocumentPath() != "") {
        WriteDocument();
        return true;
    } else {
        if (force) {
            /// \todo Not quite sure what to do here.  Generate a temporary
            /// file name?  Do something else?
            gLog.Log("Forced to discard unsaved, untitled document.");
            return true;
        } else {
            FileSystem::Path path(FileSystem::GetScriptsDirectory());
            return DoSaveAs("Save File", path.ToNativePathString().c_str(),"");
        }
    }
}

bool ScriptDoc::DoSaveAs(const wxString &dialogTitle,
                         const wxString &defaultDir,
                         const wxString &defaultName)
{
    wxFileDialog dlg(this, dialogTitle, defaultDir,
                     defaultName, kSchemeWildcards,
                     wxSAVE|wxOVERWRITE_PROMPT);
    if (dlg.ShowModal() == wxID_OK) {
        SetTitleAndPath(dlg.GetPath());
        WriteDocument();
        return true;
    }
    return false;
}
                         

void ScriptDoc::OnSave(wxCommandEvent &event) {
    SaveDocument(false);
}

void ScriptDoc::OnUpdateUiSave(wxUpdateUIEvent &event) {
    event.Enable(GetDocumentDirty() || GetDocumentPath() == "");
}

void ScriptDoc::OnSaveAs(wxCommandEvent &event) {
    wxFileName path(GetDocumentPath());
    if (path == "") {
        FileSystem::Path path(FileSystem::GetScriptsDirectory());
        DoSaveAs("Save File As", path.ToNativePathString().c_str(), "");
    } else {
        DoSaveAs("Save File As", path.GetPath(), path.GetFullName());
    }
}

void ScriptDoc::OnUpdateUiSaveAs(wxUpdateUIEvent &event) {
    event.Enable(true);
}

void ScriptDoc::OnRevert(wxCommandEvent &event) {
    wxMessageDialog dlg(this, "Discard changes and revert to saved version?",
                        "Revert", wxYES_NO|wxNO_DEFAULT);
    if (dlg.ShowModal() == wxID_YES)
        ReadDocument();
}

void ScriptDoc::OnUpdateUiRevert(wxUpdateUIEvent &event) {
    event.Enable(GetDocumentDirty() && GetDocumentPath() != "");    
}

/// Called when Scintilla's editing/undo/redo reach a point where
/// the associated document matches what's on disk.
void ScriptDoc::OnSavePointReached(wxStyledTextEvent &event) {
    SetDocumentDirty(false);
}

/// Called when Scintilla's editing/undo/redo reach a point where
/// the associated document doesn't match what's on disk.
void ScriptDoc::OnSavePointLeft(wxStyledTextEvent &event) {
    SetDocumentDirty(true);
}


//=========================================================================
//  ScriptEditor Methods
//=========================================================================

ScriptEditor *ScriptEditor::sFrame = NULL;

BEGIN_EVENT_TABLE(ScriptEditor, wxFrame)
    EVT_CLOSE(ScriptEditor::OnClose)

    EVT_MENU(wxID_NEW, ScriptEditor::OnNew)
    EVT_MENU(wxID_OPEN, ScriptEditor::OnOpen)
    EVT_UPDATE_UI(wxID_SAVE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_SAVEAS, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REVERT, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_CLOSE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_CLOSE_WINDOW, ScriptEditor::DisableUiItem)

    EVT_UPDATE_UI(wxID_UNDO, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REDO, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_CUT, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_COPY, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_PASTE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_CLEAR, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_SELECTALL, ScriptEditor::DisableUiItem)

	EVT_UPDATE_UI(wxID_FIND, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_FIND_AGAIN, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_FIND_SELECTION, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_FIND_IN_NEXT_FILE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REPLACE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REPLACE_ALL, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_REPLACE_AND_FIND_AGAIN, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(FIVEL_GOTO_LINE, ScriptEditor::DisableUiItem)
END_EVENT_TABLE()

void ScriptEditor::MaybeCreateFrame() {
    if (!sFrame)
        new ScriptEditor;
    sFrame->Show();
    sFrame->Raise();    
}

void ScriptEditor::EditScripts() {
    MaybeCreateFrame();
}

bool ScriptEditor::SaveAllForReloadScript() {
    if (sFrame) {
        DocNotebook *notebook = sFrame->mNotebook;
        return notebook->MaybeSaveAll(true, "Reload Scripts",
                                      "Save \"%s\" before reloading?");
    } else {
        return true;
    }
}

bool ScriptEditor::ProcessEventIfExists(wxEvent &event) {
    if (!sFrame)
        return false;
    else
        return sFrame->ProcessEvent(event);
}

ScriptEditor::ScriptEditor()
    : wxFrame(wxGetApp().GetStageFrame(), -1,
              "Script Editor - " + wxGetApp().GetAppName(),
              wxDefaultPosition, wxDefaultSize, wxDEFAULT_FRAME_STYLE)
{
    // Set up the static variable pointing to this frame.
    ASSERT(!sFrame);
    sFrame = this;

	// Get an appropriate icon for this window.
    SetIcon(wxICON(ic_SCRIPTS));

    // Add a status bar to our frame.
    CreateStatusBar();

    // Set up our File menu.
    wxMenu *file_menu = new wxMenu();
    file_menu->Append(wxID_NEW, "&New\tCtrl+N", "Create a new file.");
    file_menu->Append(wxID_OPEN, "&Open...\tCtrl+O",
                      "Open an existing file.");
    file_menu->Append(wxID_SAVE, "&Save\tCtrl+S", "Save the current file.");
    file_menu->Append(wxID_SAVEAS, "&Save &As...",
                      "Save the current file under a new name.");
    file_menu->Append(wxID_REVERT, "&Revert",
                      "Revert to the previously saved version of this file.");
    file_menu->AppendSeparator();
    file_menu->Append(wxID_CLOSE, "&Close Tab\tCtrl+W",
                      "Close the current tab.");
    file_menu->Append(FIVEL_CLOSE_WINDOW, "Close &Window\tCtrl+Shift+W",
                      "Close the window (including all tabs).");
    file_menu->AppendSeparator();
    file_menu->Append(FIVEL_RELOAD_SCRIPTS, "&Reload Scripts\tCtrl+R",
                      "Reload the currently executing Tamale scripts.");
    file_menu->AppendSeparator();
    file_menu->Append(FIVEL_EXIT, "E&xit\tCtrl+Q", "Exit the application.");

    // Set up our Edit menu.
    wxMenu *edit_menu = new wxMenu();
    edit_menu->Append(wxID_UNDO, "&Undo\tCtrl+Z",
                      "Reverses the previous action.");
    edit_menu->Append(wxID_REDO, "&Redo\tCtrl+Y",
                      "Reverses the last undo.");
    edit_menu->AppendSeparator();
    edit_menu->Append(wxID_CUT, "Cu&t\tCtrl+X",
                      "Delete the selection and put it onto the clipboard.");
    edit_menu->Append(wxID_COPY, "&Copy\tCtrl+C",
                      "Copy the selection to the clipboard.");
    edit_menu->Append(wxID_PASTE, "&Paste\tCtrl+V",
                      "Paste the contents of the clipboard.");
    edit_menu->Append(wxID_CLEAR, "&Delete",
                      "Delete the selection.");
    edit_menu->AppendSeparator();
    edit_menu->Append(wxID_SELECTALL, "Select &All\tCtrl+A",
                      "Select the entire document.");

    // Set up our Search menu.
    wxMenu *search_menu = new wxMenu();
    search_menu->Append(wxID_FIND, "&Find...\tCtrl+F",
                        "Grand Unified Find and Replace Dialog.");
    search_menu->AppendSeparator();
    search_menu->Append(FIVEL_FIND_AGAIN, "Find A&gain\tCtrl+G",
                        "Find the next occurrance of the search string.");
    search_menu->Append(FIVEL_FIND_SELECTION, "Find &Selection\tCtrl+H",
                        "Find the selected text.");
    search_menu->Append(FIVEL_FIND_IN_NEXT_FILE, "Find in &Next File",
                        "Find the search string in the next file.");
    search_menu->AppendSeparator();
    search_menu->Append(wxID_REPLACE, "&Replace\tCtrl+=",
                        "Replace the selected text.");
    search_menu->Append(wxID_REPLACE_ALL, "Replace &All\tCtrl+Alt+=",
                        "Replace all occurances of the search string.");
    search_menu->Append(FIVEL_REPLACE_AND_FIND_AGAIN,
                        "Re&place and Find Again\tCtrl+T",
                        ("Replace the selected text and find the next "
                         "occurance."));
    search_menu->AppendSeparator();
    search_menu->Append(FIVEL_GOTO_LINE, "Go to &Line...\tCtrl+J",
                        "Go to a specific line number.");

    // Set up our Window menu.
    wxMenu *window_menu = new wxMenu();

    // Set up our menu bar.
    wxMenuBar *menu_bar = new wxMenuBar();
    menu_bar->Append(file_menu, "&File");
    menu_bar->Append(edit_menu, "&Edit");
    menu_bar->Append(search_menu, "&Search");
    menu_bar->Append(window_menu, "&Window");
    SetMenuBar(menu_bar);

    // Create a document notebook, delegate menu events to it, and put
    // it in charge of our title bar.
    mNotebook = new DocNotebook(this);
    mDelegator.SetDelegate(mNotebook);
    mNotebook->SetFrameToTitle(this);

    // Create a sizer to handle window layout.  This doesn't do much for
    // now, but we'll add more window cruft later.
    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(mNotebook, 1 /* stretch */, wxGROW, 0);
    SetSizer(sizer);
    sizer->SetSizeHints(this);
    Layout();

    // Set an appropriate default window size.
    SetSize(wxSize(900, 650));

    // If we have a Tamale program already, open up the start script.
    if (TInterpreterManager::HaveInstance() &&
        TInterpreterManager::GetInstance()->InterpreterHasBegun())
    {
        FileSystem::Path start_script =
            FileSystem::GetScriptsDirectory().AddComponent("start.ss");
        wxString filename = start_script.ToNativePathString().c_str();
        OpenDocument(filename);
    }
}

ScriptEditor::~ScriptEditor() {
    ASSERT(sFrame);
    sFrame = NULL;
}

bool ScriptEditor::ProcessEvent(wxEvent& event) {
    bool result;
    if (mDelegator.DelegateEvent(event, &result))
        return result;
    else
        return wxFrame::ProcessEvent(event);
}

void ScriptEditor::OpenDocument(const wxString &path) {
    // If the document is already open, show it.
    for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++) {
        if (mNotebook->GetDocument(i)->GetDocumentPath() == path) {
            mNotebook->SelectDocument(i);
            return;
        }
    }
    
    // Otherwise, open the new document.
    mNotebook->AddDocument(new ScriptDoc(mNotebook, -1, path));
}

void ScriptEditor::OnClose(wxCloseEvent &event) {
    if (!mNotebook->MaybeSaveAll(event.CanVeto(), "Close Window",
                                 "Save \"%s\" before closing?"))
    {
        ASSERT(event.CanVeto());
        event.Veto();
    } else {
        Destroy();
    }
}

void ScriptEditor::OnNew(wxCommandEvent &event) {
    mNotebook->AddDocument(new ScriptDoc(mNotebook));
}

void ScriptEditor::OnOpen(wxCommandEvent &event) {
    wxString dir =
        FileSystem::GetScriptsDirectory().ToNativePathString().c_str();
    wxFileDialog dlg(this, "Open File", dir,
                     /// \todo Remove wxHIDE_READONLY and implement?
                     "", kSchemeWildcards,
                     wxOPEN|wxHIDE_READONLY|wxMULTIPLE);
    if (dlg.ShowModal() == wxID_OK) {
        wxArrayString paths;
        dlg.GetPaths(paths);
        for (size_t i = 0; i < paths.GetCount(); i++)
            OpenDocument(paths[i]);
    }
}

void ScriptEditor::DisableUiItem(wxUpdateUIEvent &event) {
    event.Enable(false);
}
