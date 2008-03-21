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
#include <wx/file.h>
#include <wx/stc/stc.h>
#include <wx/config.h>
#include <wx/laywin.h>
#include "TTemplateUtils.h"
#include "ScriptEditorDB.h"
#include "AppGlobals.h"
#include "ScriptEditor.h"
#include "DocNotebook.h"
#include "BufferSpan.h"
#include "dlg/FindDlg.h"
#include "dlg/MetaDotDlg.h"
#include "CustomTreeCtrl.h"
#include "HalyardApp.h"

// Only so we can find our parent window.
#include "StageFrame.h"


using namespace Halyard;

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
/ Reload on change

/ Fully implement square braces
/ Highlight argument keywords
/ Highlight more types of identifiers
/   Keywords Functions Variables Classes Constants Templates

/ Highlight Scheme character constants
/ Fully handle #| |# comments

/ Line numbers

/ Fix deletion of last tab
/ Fix window title bar to show file name
/ Fix accelerator for Replace All
/ Fix brace balancing to handle multiple brace types

/ Indent head forms of macros by 4 spaces
/ Indent certain macros like functions: AND, OR, PROVIDE?
/ Update indent info for known special forms

/ Update keywords after reloading scripts SUCCESSFULLY

/ Folding:
/   Expand all
/   Collapse siblings
/   Expand siblings

Next:

  Indexer based:
/   Meta-dot (aka "go to definition")
/   Function argument tooltips
    Buffer index
/     Fix "Editor opened before program" problems
/     Double-click on file opens file
/     Highlight & go to currently open file
      Better behavior when jumping to identifier

Cosmetic:
  Unhighlight name of last file closed

Sometime:

  Line & column numbers
  Indent keywords intelligently
  Syntax highlighting can't recognize identifiers at end of file

*/


namespace {
    wxString kSchemeWildcards(
        "Halyard scripts (*.ss)|*.ss|"
        "All Scheme scripts (*.ss; *.scm)|*.ss;*.scm|"
        "All files|*");
}


//=========================================================================
//  ScriptTextCtrl Definition & Methods
//=========================================================================

class ScriptTextCtrl : public wxStyledTextCtrl {
    enum {
        /// The margin where we put our line numbers.
        MARGIN_LINENUM = 0,
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

    bool mShowLineNums;
    IdentifierList mIdentifiers;
    BufferSpanTable mSpans;

    bool mReplaceStateUseRegex;
    wxString mReplaceStateText;
    
public:    
    ScriptTextCtrl(wxWindow *parent, wxWindowID id = -1, int font_size = 10);
    void GotoLineEnsureVisible(int line);
    void UpdateIdentifierInformation();

protected:
    void SetUpTextStyles(int size);

private:
    void SetStatusText(const wxString &text);

    bool IsBraceAt(int pos);
    bool IsBraceStyle(int style);
    bool IsBrace(char c);
    bool IsOpenBrace(char c);
    bool IsCloseBrace(char c);
    bool IsIdentifierChar(char c);
    bool IsIdentifierStartChar(char c);
    bool IsTokenStart(int pos);

    bool BracesMatch(char brace1, char brace2);
    int BetterBraceMatch(int pos);

    std::string GetKeywords(TScriptIdentifier::Type type);
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

    void OnWrapLines(wxCommandEvent &event);
    void OnUpdateUiWrapLines(wxUpdateUIEvent &event);
    void OnShowWhitespace(wxCommandEvent &event);
    void OnUpdateUiShowWhitespace(wxUpdateUIEvent &event);
    void OnShowLineNums(wxCommandEvent &event);
    void OnUpdateUiShowLineNums(wxUpdateUIEvent &event);
    void OnExpandAll(wxCommandEvent &event);

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
    void OnGotoDefinition(wxCommandEvent &event);
    void OnUpdateUiGotoDefinition(wxUpdateUIEvent &event);

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

    wxString GetWordAt(int pos,
                       int *outBegin = NULL, int *outEnd = NULL);
    wxString GetWordAtCursor(int *outPos = NULL,
                             int *outBegin = NULL, int *outEnd = NULL);
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
    void MaybeShowCallTip();

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

    // View menu.
    EVT_MENU(HALYARD_WRAP_LINES, ScriptTextCtrl::OnWrapLines)
    EVT_UPDATE_UI(HALYARD_WRAP_LINES, ScriptTextCtrl::OnUpdateUiWrapLines)
    EVT_MENU(HALYARD_SHOW_WHITESPACE, ScriptTextCtrl::OnShowWhitespace)
    EVT_UPDATE_UI(HALYARD_SHOW_WHITESPACE,
                  ScriptTextCtrl::OnUpdateUiShowWhitespace)
    EVT_MENU(HALYARD_SHOW_LINENUMS, ScriptTextCtrl::OnShowLineNums)
    EVT_UPDATE_UI(HALYARD_SHOW_LINENUMS,
                  ScriptTextCtrl::OnUpdateUiShowLineNums)
    EVT_MENU(HALYARD_EXPAND_ALL, ScriptTextCtrl::OnExpandAll)
    EVT_UPDATE_UI(HALYARD_EXPAND_ALL, ScriptTextCtrl::OnUpdateUiAlwaysOn)

    // Search menu.
    EVT_MENU(wxID_FIND, ScriptTextCtrl::OnFind)
    EVT_UPDATE_UI(wxID_FIND, ScriptTextCtrl::OnUpdateUiAlwaysOn)
    EVT_MENU(HALYARD_FIND_AGAIN, ScriptTextCtrl::OnFindAgain)
    EVT_UPDATE_UI(HALYARD_FIND_AGAIN, ScriptTextCtrl::OnUpdateUiFindAgain)
    EVT_MENU(HALYARD_FIND_SELECTION, ScriptTextCtrl::OnFindSelection)
    EVT_UPDATE_UI(HALYARD_FIND_SELECTION,
                  ScriptTextCtrl::OnUpdateUiFindSelection)
    EVT_MENU(wxID_REPLACE, ScriptTextCtrl::OnReplace)
    EVT_UPDATE_UI(wxID_REPLACE, ScriptTextCtrl::OnUpdateUiReplace)
    EVT_MENU(HALYARD_REPLACE_AND_FIND_AGAIN,
             ScriptTextCtrl::OnReplaceAndFindAgain)
    EVT_UPDATE_UI(HALYARD_REPLACE_AND_FIND_AGAIN,
                  ScriptTextCtrl::OnUpdateUiReplace)
    EVT_MENU(wxID_REPLACE_ALL, ScriptTextCtrl::OnReplaceAll)
    EVT_UPDATE_UI(wxID_REPLACE_ALL, ScriptTextCtrl::OnUpdateUiFindAgain)
    EVT_MENU(HALYARD_GOTO_LINE, ScriptTextCtrl::OnGotoLine)
    EVT_UPDATE_UI(HALYARD_GOTO_LINE, ScriptTextCtrl::OnUpdateUiAlwaysOn)
    EVT_MENU(HALYARD_GOTO_DEFINITION, ScriptTextCtrl::OnGotoDefinition)
    EVT_UPDATE_UI(HALYARD_GOTO_DEFINITION,
                  ScriptTextCtrl::OnUpdateUiGotoDefinition)
END_EVENT_TABLE()

ScriptTextCtrl::ScriptTextCtrl(wxWindow *parent, wxWindowID id, int font_size)
    : wxStyledTextCtrl() // We need to use the empty constructor; see below.
{
    // Set up our class options, *then* call Create(), to avoid early
    // repainting of this window with the wrong options.
    Hide();
    Create(parent, id);

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
    SetVisiblePolicy(wxSTC_VISIBLE_SLOP, 10);

    // Let Scintilla know which modification events we want.
    SetModEventMask(GetModEventMask()
                    | wxSTC_MOD_INSERTTEXT | wxSTC_MOD_DELETETEXT);

    // Choose our language and set up syntax highlighting.
    SetLexer(wxSTC_LEX_LISP);
    SetUpTextStyles(font_size);

    // Set up line numbers.
    SetMarginWidth(MARGIN_LINENUM, 0);
    SetMarginType(MARGIN_LINENUM, wxSTC_MARGIN_NUMBER);
    mShowLineNums = false;

    // Set up folding.
    //SetStyleBits(5); Not actually necessary...
    SetProperty("fold", "1");
    SetProperty("fold.compact", "0"); // Hiding blank lines is weird...
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

    UpdateIdentifierInformation();
}

void ScriptTextCtrl::SetUpTextStyles(int size) {
    // Set up default style attributes and copy them to all styles.
    StyleSetFont(wxSTC_STYLE_DEFAULT,
                 wxFont(size, wxMODERN, wxNORMAL, wxNORMAL));
    StyleClearAll();

    // Set up syntax highlighting.
    StyleSetBold(wxSTC_STYLE_BRACELIGHT, true);
    StyleSetForeground(wxSTC_STYLE_BRACELIGHT, *wxBLUE);
    StyleSetBackground(wxSTC_STYLE_BRACELIGHT, wxColor(0xD0, 0xD0, 0xD0));
    StyleSetBold(wxSTC_STYLE_BRACEBAD, true);
    StyleSetForeground(wxSTC_STYLE_BRACEBAD, *wxRED);
    StyleSetForeground(wxSTC_LISP_COMMENT, wxColor(0xC0, 0x00, 0x00));
    StyleSetForeground(wxSTC_LISP_COMMENTBLOCK, wxColor(0xC0, 0x00, 0x00));
    StyleSetForeground(wxSTC_LISP_KEYWORD, wxColor(0x00, 0x00, 0xC0));
    StyleSetForeground(wxSTC_LISP_STRING, wxColor(0x80, 0x40, 0x00));
    StyleSetForeground(wxSTC_LISP_CHAR, wxColor(0x80, 0x40, 0x00));
    StyleSetForeground(wxSTC_LISP_STRINGEOL, *wxRED);
    StyleSetForeground(wxSTC_LISP_KEYWORDARG, wxColor(0x80, 0x00, 0x80));
    StyleSetForeground(wxSTC_LISP_WORD1, wxColor(0x00, 0x60, 0x20));
    StyleSetForeground(wxSTC_LISP_WORD2, wxColor(0x00, 0x60, 0x20));
    StyleSetForeground(wxSTC_LISP_WORD3, wxColor(0x00, 0x60, 0x20));
    StyleSetForeground(wxSTC_LISP_WORD4, wxColor(0x00, 0x60, 0x20));
    StyleSetForeground(wxSTC_LISP_WORD5, wxColor(0x00, 0x60, 0x20));

    // Boring: wxSTC_LISP_NUMBER, wxSTC_LISP_IDENTIFIER, wxSTC_LISP_OPERATOR
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
    return (c == '(' || c == '[' || c == '{');
}

bool ScriptTextCtrl::IsCloseBrace(char c) {
    return (c == ')' || c == ']' || c == '}');
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
 
bool ScriptTextCtrl::BracesMatch(char brace1, char brace2) {
    switch (brace1) {
        case '(': return brace2 == ')';
        case '[': return brace2 == ']';
        case '{': return brace2 == '}';
        case '<': return brace2 == '>';
        case ')': return brace2 == '(';
        case ']': return brace2 == '[';
        case '}': return brace2 == '{';
        case '>': return brace2 == '<';
        default:
            ASSERT(0);
            return false;
    }
}

/// Like the built-in BraceMatch function, but we don't get confused by
/// "[(])".  I don't know why this isn't included in Scintilla.
int ScriptTextCtrl::BetterBraceMatch(int pos) {
    ASSERT(IsBraceAt(pos));

    // Push our starting brace onto the stack.
    char c = GetCharAt(pos);
    std::vector<char> stack;
    stack.push_back(c);

    // Decide which direction to travel.
    int incr = IsOpenBrace(c) ? 1 : -1;

    // Iterate.
    int len = GetTextLength();
    for (int i = pos + incr; 0 <= i && i < len; i += incr) {
        ASSERT(stack.size() > 0);
        if (IsBraceAt(i)) {
            c = GetCharAt(i);
            if (BracesMatch(c, stack.back())) {
                stack.pop_back();
                if (stack.size() == 0)
                    return i;
            } else {
                stack.push_back(c);
            }
        }
    }
    return wxSTC_INVALID_POSITION;
}

void ScriptTextCtrl::UpdateIdentifierInformation() {
    // Fetch the identifier list provided by the ScriptEditor.
    mIdentifiers = ScriptEditor::GetIdentifiers();

    // Install keywords.
    SetKeyWords(0, GetKeywords(TScriptIdentifier::KEYWORD).c_str());
    SetKeyWords(1, GetKeywords(TScriptIdentifier::FUNCTION).c_str());
    SetKeyWords(2, GetKeywords(TScriptIdentifier::VARIABLE).c_str());
    SetKeyWords(3, GetKeywords(TScriptIdentifier::CONSTANT).c_str());
    SetKeyWords(4, GetKeywords(TScriptIdentifier::CLASS).c_str());
    SetKeyWords(5, GetKeywords(TScriptIdentifier::TEMPLATE).c_str());

    // Recolourize the document with new keywords.
    Colourise(0, GetTextLength());
}

std::string ScriptTextCtrl::GetKeywords(TScriptIdentifier::Type type) {
	std::string result;
    IdentifierList::iterator i = mIdentifiers.begin();
    for (; i != mIdentifiers.end(); ++i) {
        if (i->GetType() == type) {
            if (result.size() != 0)
                result += " ";
            result += i->GetName();
        }
    }
    return result;
}

IdentifierList ScriptTextCtrl::GetCompletions(const std::string &partial) {
    IdentifierList result;
    IdentifierList::iterator i = mIdentifiers.begin();
    for (; i != mIdentifiers.end(); ++i) {
        std::string name = i->GetName();
        if (name.size() < partial.size())
            continue;
        if (name.compare(0, partial.size(), partial) == 0)
            result.push_back(*i);
    }
    std::sort(result.begin(), result.end());
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
        if (event.GetModifiers() == 0) {
            // Toggle just this line.
            ToggleFold(line);
        } else if (event.GetModifiers() == wxSTC_SCMOD_SHIFT) {
            // Set all siblings to same state.
            bool goal_state = !GetFoldExpanded(line);
            int parent = GetFoldParent(line);
            int first_sibling, last_sibling;
            if (parent != wxSTC_INVALID_POSITION) {
                first_sibling = parent + 1;
                last_sibling = GetLastChild(parent, -1);
            } else {
                first_sibling = 0;
                last_sibling = GetLineCount() - 1;
            }
            for (int i = first_sibling; i <= last_sibling; i++) {
                if (GetFoldExpanded(i) != goal_state)
                    ToggleFold(i);
                i = GetLastChild(i, -1);
            }
            EnsureVisibleEnforcePolicy(line);
        }
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

    // Display tool tips when appropriate.
    if (inserted == ' ')
        MaybeShowCallTip();
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
        brace_match_pos = BetterBraceMatch(brace_pos);

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
               event.ControlDown() && !event.AltDown() && !event.MetaDown()) {
        // HACK - Under Windows, a keypress of Ctrl-= gets sent as Ctrl-+,
        // causing our Ctrl-= accelerator to fail.  Apply a cluestick of
        // dubious origins.
        /// \todo Move this code up to ScriptEditor class?
        int id = (event.ShiftDown()) ? wxID_REPLACE_ALL : wxID_REPLACE;
        wxUpdateUIEvent update(id);
        if (ProcessEvent(update) && update.GetEnabled()) {            
            wxCommandEvent command(wxEVT_COMMAND_MENU_SELECTED, id);
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

void ScriptTextCtrl::OnWrapLines(wxCommandEvent &event) {
    if (GetWrapMode() == wxSTC_WRAP_NONE) {
        SetWrapMode(wxSTC_WRAP_WORD);
        SetEdgeMode(wxSTC_EDGE_LINE);
        SetEdgeColour(wxColour(0x80, 0x80, 0x80));
    } else {
        SetWrapMode(wxSTC_WRAP_NONE);
        SetEdgeMode(wxSTC_EDGE_BACKGROUND);
        SetEdgeColour(*wxRED);
    }
}

void ScriptTextCtrl::OnUpdateUiWrapLines(wxUpdateUIEvent &event) {
    event.Enable(true);
    event.Check(GetWrapMode() != wxSTC_WRAP_NONE);
}

void ScriptTextCtrl::OnShowWhitespace(wxCommandEvent &event) {
    bool new_value = !GetViewWhiteSpace();
    SetViewWhiteSpace(new_value);
    SetViewEOL(new_value);
}

void ScriptTextCtrl::OnUpdateUiShowWhitespace(wxUpdateUIEvent &event) {
    event.Enable(true);
	event.Check(GetViewWhiteSpace() ? true : false);
}

void ScriptTextCtrl::OnShowLineNums(wxCommandEvent &event) {
    mShowLineNums = !mShowLineNums;
    if (mShowLineNums)
        SetMarginWidth(MARGIN_LINENUM, 45);
    else
        SetMarginWidth(MARGIN_LINENUM, 0);
}

void ScriptTextCtrl::OnUpdateUiShowLineNums(wxUpdateUIEvent &event) {
    event.Enable(true);
    event.Check(mShowLineNums);
}

void ScriptTextCtrl::OnExpandAll(wxCommandEvent &event) {
    size_t count = GetLineCount();
    for (size_t i = 0; i < count; ++i)
        if (!GetFoldExpanded(i))
            ToggleFold(i);
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
            GotoLineEnsureVisible(line-1);
        } else {
            ::wxBell();
            SetStatusText("No such line.");
        }
    }
}

void ScriptTextCtrl::OnGotoDefinition(wxCommandEvent &event) {
    // Get the word underneath the cursor.
    wxString identifier = GetWordAtCursor();
    ASSERT(identifier != "");
    ScriptEditor::ShowDefinition(identifier);
}

void ScriptTextCtrl::OnUpdateUiGotoDefinition(wxUpdateUIEvent &event) {
    event.Enable(GetWordAtCursor() != "" &&
                 TInterpreterManager::GetScriptEditorDB());
                 
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

void ScriptTextCtrl::GotoLineEnsureVisible(int line) {
    GotoLine(line);
    // This sort-of-approximately-maybe puts the line we're searching
    // for comfortably onto the screen.
    //
    /// \bug This does silly things if the window is shorter than 10 lines.
    SetVisiblePolicy(wxSTC_VISIBLE_STRICT|wxSTC_VISIBLE_SLOP, 10);
    EnsureVisibleEnforcePolicy(line);
    SetVisiblePolicy(wxSTC_VISIBLE_SLOP, 10);

    // Make sure this window is focused.
    SetFocus();
}

wxString ScriptTextCtrl::GetWordAt(int pos, int *outBegin, int *outEnd)
{
    int line_number = LineFromPosition(pos);
    int line_start = PositionFromLine(line_number);
    wxString line = GetLine(line_number);

    int word_begin = pos - line_start;
    while (word_begin > 0 && IsIdentifierChar(line[word_begin-1]))
        word_begin--;
    int word_end = pos - line_start;
    while (static_cast<size_t>(word_end) < line.length() &&
		   IsIdentifierChar(line[word_end]))
        word_end++;
    if (word_begin == word_end ||
        !IsIdentifierStartChar(line[word_begin]))
        return "";
    if (outBegin)
        *outBegin = line_start + word_begin;
    if (outEnd)
        *outEnd = line_start + word_end;
    return line.Mid(word_begin, word_end - word_begin);
}

wxString ScriptTextCtrl::GetWordAtCursor(int *outPos,
                                         int *outBegin, int *outEnd)
{
    int pos = GetCurrentPos();
    wxString result = GetWordAt(pos, outBegin, outEnd);
    if (outPos)
        *outPos = pos;
    return result;
}

void ScriptTextCtrl::AutoCompleteIdentifier() {
    int pos, word_begin, word_end;
    wxString prefix(GetWordAtCursor(&pos, &word_begin, &word_end));
    if (prefix == "" || pos < word_end)
        return;
    ASSERT(pos >= 1);
    
    // If we're adding characters at the end of a name, find any known
    // commands which begin with the name.
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
    BeginUndoAction();

    // Figure out which lines are in the selection.
    int begin_pos, end_pos;
    GetSelection(&begin_pos, &end_pos);
    Colourise(begin_pos, end_pos);
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

    EndUndoAction();
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
    // Extract the name of our special form or macro.
    int parent_end = inParentPos+1;
    int length = GetTextLength();
    while (parent_end < length && GetStyleAt(parent_end) == wxSTC_LISP_KEYWORD)
        parent_end++;
    std::string key(GetTextRange(inParentPos+1, parent_end).mb_str());

    // Look up the number of head forms used by this macro.
    /// \todo Linear probe is *slow*.
    int head_forms = 0;
    for (size_t i = 0; i < mIdentifiers.size(); i++) {
        if (mIdentifiers[i].GetName() == key) {
            head_forms = mIdentifiers[i].GetIndentHint();
            break;
        }
    }

    // Calculate our indentation.
    if (head_forms == -1)
        return CalculateFunctionIndentation(inParentPos, inSiblingPos);
    else if (static_cast<int>(inSiblingPos.size()) <= head_forms)
        return (GetBaseIndentFromPosition(inParentPos) + 4);
    else
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

void ScriptTextCtrl::MaybeShowCallTip() {
    // Try to extract a function name.  If we can't, give up.
    int pos = GetCurrentPos();
    if (pos < 2) 
        return;
    ASSERT(GetCharAt(pos - 1) == ' ');
    int word_begin;
    wxString word = GetWordAt(pos - 2, &word_begin);
    if (word == "")
        return;

    // Look the word up in the database.
    ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
    if (!db)
        return;
    std::vector<std::string> help(db->FindHelp(word.Lower().c_str()));
    if (help.empty())
        return;

    // Combine our help strings.
    std::string combined_help;
    std::vector<std::string>::iterator i = help.begin();
    for (; i != help.end(); ++i) {
        if (i != help.begin())
            combined_help += "\n";
        combined_help += *i;
    }
    
    // Display the actual call tip.
    CallTipShow(word_begin, combined_help.c_str());
}


//=========================================================================
//  ScriptDoc Definition & Methods
//=========================================================================

class ScriptDoc : public ScriptTextCtrl, public DocNotebookTab {
public:
    ScriptDoc(DocNotebook *parent, wxWindowID id = -1, int font_size = 10);
    ScriptDoc(DocNotebook *parent, wxWindowID id, int font_size,
              const wxString &path);

    virtual wxWindow *GetDocumentWindow() { return this; }

    virtual void OfferToReloadIfChanged();
    virtual void SetTextSize(int size);

protected:
    virtual bool SaveDocument(bool force);

private:
    wxDateTime mSavePointModTime;
    wxDateTime mLastKnownModTime;

    void SetTitleAndPath(const wxString &fullPath);

    void NotifySelected();

    bool OkToSaveChanges();
    void UpdateSavePointModTime();
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

ScriptDoc::ScriptDoc(DocNotebook *parent, wxWindowID id, int font_size)
    : ScriptTextCtrl(parent, id, font_size), DocNotebookTab(parent)
{
    SetDocumentTitle(parent->GetNextUntitledDocumentName());
}

ScriptDoc::ScriptDoc(DocNotebook *parent, wxWindowID id, int font_size,
                     const wxString &path)
    : ScriptTextCtrl(parent, id, font_size), DocNotebookTab(parent)
{
    SetTitleAndPath(path);
    ReadDocument();
}

void ScriptDoc::SetTitleAndPath(const wxString &fullPath) {
    SetDocumentPath(fullPath);

    // Use the (short) filename as the document name.
    SetDocumentTitle(wxFileName(fullPath).GetFullName());
}

void ScriptDoc::NotifySelected() {
    // Let the ScriptTree know we're the active tab.
    ScriptEditor::HighlightFile(GetDocumentPath());
}

void ScriptDoc::OfferToReloadIfChanged() {
    // Make sure we have a file to check.
    if (GetDocumentPath() == "")
        return;
    wxFileName path(GetDocumentPath());
    if (!path.FileExists())
        return;

    // If the modification time hasn't changed, we're OK.
    wxDateTime on_disk_mod_time = path.GetModificationTime();
    if (on_disk_mod_time == mLastKnownModTime)
        return;

    // Ask the user what to do.
    SelectDocument();
    int flags;
    wxString prompt;
    if (GetDocumentDirty()) {
        flags = wxYES_NO|wxNO_DEFAULT;
        prompt.Printf("The file \"%s\" has changed on disk.  Reload it "
                      "and lose the changes you've made in the editor?",
                      GetDocumentTitle());
    } else {
        flags = wxYES_NO|wxYES_DEFAULT;
        prompt.Printf("The file \"%s\" has changed on disk.  Reload it?",
                      GetDocumentTitle());
    }
    wxMessageDialog dlg(this, prompt, "File Changed", flags);
    if (dlg.ShowModal() == wxID_YES) {
        ReadDocument();
    } else {
        // Update the last known mod time so we stop bugging the user.
        mLastKnownModTime = on_disk_mod_time;
    }
}

void ScriptDoc::SetTextSize(int size) {
    SetUpTextStyles(size);
}

bool ScriptDoc::OkToSaveChanges() {
    // Make sure somebody hasn't edited the file under us.
    wxFileName path(GetDocumentPath());
    if (path.FileExists()
        && path.GetModificationTime() != mSavePointModTime)
    {
        wxString prompt;
        prompt.Printf("The file \"%s\" has changed on disk.  "
                      "Overwrite and lose changes?", GetDocumentTitle());
        wxMessageDialog dlg(this, prompt, "File Changed",
                            wxYES_NO|wxNO_DEFAULT);
        return (dlg.ShowModal() == wxID_YES);
    } else {
        return true;
    }
}

void ScriptDoc::UpdateSavePointModTime() {
    ASSERT(GetDocumentPath() != "");
    wxFileName path(GetDocumentPath());
    /// \todo How does wxWindows report errors here?
    mSavePointModTime = path.GetModificationTime();
    mLastKnownModTime = mSavePointModTime;
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
    file.Close();

    UpdateSavePointModTime();

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
    file.Close();

    UpdateSavePointModTime();

    // Don't let the user undo document setup.
    EmptyUndoBuffer();

    // Mark the document as saved.
    SetSavePoint();
}

bool ScriptDoc::SaveDocument(bool force) {
    if (GetDocumentPath() != "") {
        // Check for any interesting reasons we shouldn't save, such
        // as a file changing underneath us on disk.
        if (!force && !OkToSaveChanges())
            return false;

        // Write the document to disk.
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
    SelectDocument();
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
//  FileTreeItemData Methods
//=========================================================================

class FileTreeItemData : public CustomTreeItemData {
    std::string mPath;
    
public:
    FileTreeItemData(CustomTreeCtrl *inTreeCtrl, const std::string &inPath);
    void OnLeftDClick(wxMouseEvent& event);
};

FileTreeItemData::FileTreeItemData(CustomTreeCtrl *inTreeCtrl,
                                   const std::string &inPath)
    : CustomTreeItemData(inTreeCtrl), mPath(inPath)
{
}

void FileTreeItemData::OnLeftDClick(wxMouseEvent& event) {
	ScriptEditor::OpenDocument(mPath.c_str());
}


//=========================================================================
//  DefinitionTreeItemData Methods
//=========================================================================

class DefinitionTreeItemData : public CustomTreeItemData {
    ScriptEditorDB::Definition mDefinition;

public:
    DefinitionTreeItemData(CustomTreeCtrl *inTreeCtrl,
                           ScriptEditorDB::Definition inDefinition);
    void OnLeftDClick(wxMouseEvent& event);
};

DefinitionTreeItemData::
DefinitionTreeItemData(CustomTreeCtrl *inTreeCtrl,
                       ScriptEditorDB::Definition inDefinition)
    : CustomTreeItemData(inTreeCtrl), mDefinition(inDefinition)
{
}

void DefinitionTreeItemData::OnLeftDClick(wxMouseEvent& event) {
    ScriptEditor::OpenDocument(mDefinition.GetNativePath().c_str(),
                               mDefinition.line_number);
}


//=========================================================================
//  ScriptTree Methods
//=========================================================================

class ScriptTree : public CustomTreeCtrl, private ScriptEditorDB::IListener,
                   private TReloadNotified
{
    typedef std::map<std::string,wxTreeItemId> ItemMap;
    ItemMap mItemMap;
    bool mRegisteredWithDB;
    wxTreeItemId mLastHighlightedItem;

public:
    ScriptTree(wxWindow *parent);
    ~ScriptTree();

    /// Attempt to highlight the tree node corresponding to the specified
    /// path.  May not succeed.
    void HighlightFile(const wxString &path);

private:    
    void NotifyReloadScriptSucceeded();

    void FileChanged(const std::string &relpath);
    void FileDeleted(const std::string &relpath);

    int ChooseDefinitionIcon(const ScriptEditorDB::Definition &def);
    wxTreeItemId FindItem(const std::string &relpath,
                          bool shouldCreate = false,
                          bool isDirectory = false);
    wxTreeItemId InsertItemAlphabetically(wxTreeItemId parent,
                                          const std::string name);
};

ScriptTree::ScriptTree(wxWindow *parent)
    : CustomTreeCtrl(parent, -1, wxDefaultPosition, wxDefaultSize,
                     wxTR_HIDE_ROOT|wxTR_LINES_AT_ROOT|wxTR_HAS_BUTTONS),
      mRegisteredWithDB(false)
{
    AddRoot("Program");

    // If we have a ScriptEditorDB, then send ourselves a
    // NotifyReloadScriptSucceeded message to trigger our initial setup.
    if (TInterpreterManager::GetScriptEditorDB())
        NotifyReloadScriptSucceeded();
}

ScriptTree::~ScriptTree() {
    if (mRegisteredWithDB) {
        ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
        ASSERT(db);
        db->RemoveListener(this);
    }
}

void ScriptTree::NotifyReloadScriptSucceeded() {
    if (!mRegisteredWithDB) {
        mRegisteredWithDB = true;
        ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
        ASSERT(db);
        db->AddListener(this);
    }   
}

// TODO Refactor code shared with ProgramTree into CustomTreeCtrl?
void ScriptTree::HighlightFile(const wxString &path) {
    // Try to find the new item to highlight.
    wxTreeItemId new_item;
    ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
    if (db) {
        std::string relpath(db->NativeToRelPath(path.mb_str()));
        if (relpath != "")
            new_item = FindItem(relpath);
    }

    // If there's nothing to change, bail out now.
    if (mLastHighlightedItem == new_item)
        return;

    // Unhighlight previously highlighted item.
    if (mLastHighlightedItem.IsOk())
        SetItemBold(mLastHighlightedItem, false);
        
    // Highlight the new item.
    if (new_item.IsOk()) {
        SetItemBold(new_item, true);
        // Only do scrolling and selection stuff if we're not already
        // inside one of new_item's children.
        wxTreeItemId selected = GetSelection();
        if (!selected.IsOk() || GetItemParent(selected) != new_item) {
            SelectItem(new_item);
            EnsureVisible(new_item);
        }            
    }
    mLastHighlightedItem = new_item;
}

void ScriptTree::FileChanged(const std::string &relpath) {
    ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
    ASSERT(db);

    // Find the item corresponding to this file, creating it if necessary.
    wxTreeItemId item = FindItem(relpath, true, false);
    SetItemData(item,
                new FileTreeItemData(this, db->RelPathToNative(relpath)));

    // Get the definitions for this file.
    ScriptEditorDB::Definitions defs = db->FindDefinitionsInFile(relpath);

    // Delete any existing definitions, and add our new ones. 
    DeleteChildren(item);
    ScriptEditorDB::Definitions::iterator i = defs.begin();
    for (; i != defs.end(); ++i) {
        wxTreeItemId new_id = AppendItem(item, i->name.c_str());
        SetIcon(new_id, ChooseDefinitionIcon(*i));
        SetItemData(new_id, new DefinitionTreeItemData(this, *i));
    }
}

void ScriptTree::FileDeleted(const std::string &relpath) {
    wxTreeItemId item = FindItem(relpath);
    if (item.IsOk()) {
        Delete(item);

        // Get rid of our mItemMap entry, too.
        ItemMap::iterator found = mItemMap.find(relpath);
        ASSERT(found != mItemMap.end());
        mItemMap.erase(found);
    }
}

int ScriptTree::ChooseDefinitionIcon(const ScriptEditorDB::Definition &def) {
    switch (def.type) {
        case TScriptIdentifier::KEYWORD: return ICON_KEYWORD;
        case TScriptIdentifier::FUNCTION: return ICON_FUNCTION;
        case TScriptIdentifier::VARIABLE: return ICON_VARIABLE;
        case TScriptIdentifier::CONSTANT: return ICON_CONSTANT;
        case TScriptIdentifier::CLASS: return ICON_CLASS;
        case TScriptIdentifier::TEMPLATE: return ICON_TEMPLATE;
        case TScriptIdentifier::GROUP: return ICON_GROUP;
        case TScriptIdentifier::SEQUENCE: return ICON_SEQUENCE;
        case TScriptIdentifier::CARD: return ICON_CARD;
        case TScriptIdentifier::ELEMENT: return ICON_ELEMENT;

        case TScriptIdentifier::UNKNOWN:
        default:
            return ICON_UNKNOWN;
    }
}

wxTreeItemId ScriptTree::FindItem(const std::string &relpath,
                                  bool shouldCreate,
                                  bool isDirectory)
{
    ItemMap::iterator found = mItemMap.find(relpath);
    if (found != mItemMap.end()) {
        return found->second;
    } else if (shouldCreate) {
        // Figure out where to put the item.
        std::string::size_type slash = relpath.rfind('/');
        wxTreeItemId parent_id;
        std::string name;
        if (slash == std::string::npos) {
            name = relpath;
            parent_id = GetRootItem();
        } else {
            std::string dir(relpath, 0, slash);
            name = std::string(relpath, slash+1, std::string::npos);
            parent_id = FindItem(dir, true, true);
        }

        // Add the item to the GUI.
        wxTreeItemId item_id =
            InsertItemAlphabetically(parent_id, name.c_str());
        if (isDirectory)
            SetIcon(item_id, ICON_FOLDER_CLOSED, ICON_FOLDER_OPEN);
        else
            SetIcon(item_id, ICON_SCRIPT, ICON_SCRIPT);

        // Remember the item id and return it.
        mItemMap.insert(ItemMap::value_type(relpath, item_id));
        return item_id;
    } else {
        return wxTreeItemId();
    }
}

wxTreeItemId ScriptTree::InsertItemAlphabetically(wxTreeItemId parent,
                                                  const std::string name)
{
    // Look for an item to insert before.
    wxTreeItemIdValue cookie;
    wxTreeItemId existing_id = GetFirstChild(parent, cookie);
    size_t existing_index = 0;
    while (existing_id.IsOk()) {
        std::string existing_name(GetItemText(existing_id).mb_str());
        if (MakeStringLowercase(name) < MakeStringLowercase(existing_name))
            return InsertItem(parent, existing_index, name.c_str());
        
        existing_id = GetNextChild(parent, cookie);
        ++existing_index;
    }

    // OK, there's no place to insert it, so toss it at the end.
    return AppendItem(parent, name.c_str());
}


//=========================================================================
//  ScriptEditor Methods
//=========================================================================

ScriptEditor *ScriptEditor::sFrame = NULL;

BEGIN_EVENT_TABLE(ScriptEditor, SashFrame)
    EVT_ACTIVATE(ScriptEditor::OnActivate)
    EVT_CLOSE(ScriptEditor::OnClose)

    EVT_MENU(wxID_NEW, ScriptEditor::OnNew)
    EVT_MENU(wxID_OPEN, ScriptEditor::OnOpen)
    EVT_UPDATE_UI(wxID_SAVE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_SAVEAS, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_SAVE_ALL, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REVERT, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_CLOSE_TAB, ScriptEditor::DisableUiItem)
    EVT_MENU(wxID_CLOSE, ScriptEditor::OnCloseWindow)

    EVT_UPDATE_UI(HALYARD_WRAP_LINES, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_SHOW_WHITESPACE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_SHOW_LINENUMS, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_EXPAND_ALL, ScriptEditor::DisableUiItem)
    EVT_MENU(HALYARD_TEXT_SIZE_INC, ScriptEditor::OnIncreaseTextSize)
    EVT_MENU(HALYARD_TEXT_SIZE_DEC, ScriptEditor::OnDecreaseTextSize)

    EVT_UPDATE_UI(wxID_UNDO, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REDO, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_CUT, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_COPY, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_PASTE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_CLEAR, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_SELECTALL, ScriptEditor::DisableUiItem)

	EVT_UPDATE_UI(wxID_FIND, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_FIND_AGAIN, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_FIND_SELECTION, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_FIND_IN_NEXT_FILE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REPLACE, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(wxID_REPLACE_ALL, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_REPLACE_AND_FIND_AGAIN, ScriptEditor::DisableUiItem)
    EVT_UPDATE_UI(HALYARD_GOTO_LINE, ScriptEditor::DisableUiItem)
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

void ScriptEditor::OpenDocument(const wxString &path, int line) {
    MaybeCreateFrame();
    sFrame->OpenDocumentInternal(path, line);
}

void ScriptEditor::ShowDefinition(const wxString &identifier) {
    MaybeCreateFrame();
    sFrame->ShowDefinitionInternal(identifier);
}

void ScriptEditor::HighlightFile(const wxString &path) {
    ASSERT(sFrame);
    sFrame->HighlightFileInternal(path);
}

IdentifierList ScriptEditor::GetIdentifiers() {
    ASSERT(sFrame);
    return sFrame->mIdentifiers;
}

ScriptEditor::ScriptEditor()
    : SashFrame(wxGetApp().GetStageFrame(), -1,
                "Script Editor - " + wxGetApp().GetAppName(),
                "ScriptEditor", wxDefaultSize, wxDEFAULT_FRAME_STYLE),
      mTreeContainer(NULL), mTree(NULL), mNotebook(NULL),
      mProcessingActivateEvent(false)
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
    file_menu->Append(HALYARD_SAVE_ALL, "Save A&ll\tCtrl+Shift+S",
                      "Save all open files.");
    file_menu->Append(wxID_REVERT, "&Revert",
                      "Revert to the previously saved version of this file.");
    file_menu->AppendSeparator();
    file_menu->Append(HALYARD_CLOSE_TAB, "&Close Tab\tCtrl+W",
                      "Close the current tab.");
    file_menu->Append(wxID_CLOSE, "Close &Window\tCtrl+Shift+W",
                      "Close the window (including all tabs).");
    file_menu->AppendSeparator();
    file_menu->Append(HALYARD_RELOAD_SCRIPTS, "&Reload Scripts\tCtrl+R",
                      "Reload the currently executing Halyard scripts.");
    file_menu->AppendSeparator();
    file_menu->Append(HALYARD_EXIT, "E&xit\tCtrl+Q", "Exit the application.");

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

    // Set up our View menu.
    wxMenu *view_menu = new wxMenu();
    view_menu->AppendCheckItem(HALYARD_WRAP_LINES, "&Wrap Lines",
                               "Wrap long lines at the window edge.");
    view_menu->AppendCheckItem(HALYARD_SHOW_WHITESPACE, "Show White&space",
                               "View space and end-of-line characters.");
    view_menu->AppendCheckItem(HALYARD_SHOW_LINENUMS, "Show Line &Numbers",
                               "View line numbers in the left margin.");
    view_menu->AppendSeparator();
    view_menu->Append(HALYARD_EXPAND_ALL, "&Expand All",
                      "Expand all collapsed lines of code.");
    view_menu->AppendSeparator();
    view_menu->Append(HALYARD_TEXT_SIZE_INC, "&Increase Text Size",
                      "Increase the size of displayed text.");
    view_menu->Append(HALYARD_TEXT_SIZE_DEC, "&Decrease Text Size",
                      "Decrease the size of displayed text.");

    // Set up our Search menu.
    wxMenu *search_menu = new wxMenu();
    search_menu->Append(wxID_FIND, "&Find...\tCtrl+F",
                        "Grand Unified Find and Replace Dialog.");
    search_menu->AppendSeparator();
    search_menu->Append(HALYARD_FIND_AGAIN, "Find A&gain\tCtrl+G",
                        "Find the next occurrance of the search string.");
    search_menu->Append(HALYARD_FIND_SELECTION, "Find &Selection\tCtrl+H",
                        "Find the selected text.");
    search_menu->Append(HALYARD_FIND_IN_NEXT_FILE, "Find in &Next File",
                        "Find the search string in the next file.");
    search_menu->AppendSeparator();
    search_menu->Append(wxID_REPLACE, "&Replace\tCtrl+=",
                        "Replace the selected text.");
    search_menu->Append(wxID_REPLACE_ALL, "Replace &All\tCtrl+Shift+=",
                        "Replace all occurances of the search string.");
    search_menu->Append(HALYARD_REPLACE_AND_FIND_AGAIN,
                        "Re&place and Find Again\tCtrl+T",
                        ("Replace the selected text and find the next "
                         "occurance."));
    search_menu->AppendSeparator();
    search_menu->Append(HALYARD_GOTO_LINE, "Go to &Line...\tCtrl+J",
                        "Go to a specific line number.");
    search_menu->Append(HALYARD_GOTO_DEFINITION, "Go to &Definition\tAlt+.",
                        "Look up the identifier under the cursor.");

    // Set up our Window menu.
    wxMenu *window_menu = new wxMenu();

    // Set up our menu bar.
    wxMenuBar *menu_bar = new wxMenuBar();
    menu_bar->Append(file_menu, "&File");
    menu_bar->Append(edit_menu, "&Edit");
    menu_bar->Append(view_menu, "&View");
    menu_bar->Append(search_menu, "&Search");
    menu_bar->Append(window_menu, "&Window");
    SetMenuBar(menu_bar);
    menu_bar->EnableTop(4, false);

    // Create a tool bar.
    CreateToolBar();
    wxToolBar *tb = GetToolBar();
    tb->AddTool(HALYARD_RELOAD_SCRIPTS, "Reload", wxBITMAP(tb_reload),
                "Reload Scripts");
    tb->AddSeparator();
    tb->AddTool(wxID_NEW, "New", wxBITMAP(tb_new), "New File");
    tb->AddTool(wxID_OPEN, "Open", wxBITMAP(tb_open), "Open File");
    tb->AddTool(wxID_SAVE, "Save", wxBITMAP(tb_save), "Save File");
    tb->AddTool(HALYARD_SAVE_ALL, "Save All", wxBITMAP(tb_saveall),
                "Save All Files");
    tb->AddSeparator();
    tb->AddTool(wxID_CUT, "Cut", wxBITMAP(tb_cut), "Cut Text");
    tb->AddTool(wxID_COPY, "Copy", wxBITMAP(tb_copy), "Copy Text");
    tb->AddTool(wxID_PASTE, "Paste", wxBITMAP(tb_paste), "Paste Text");
    tb->AddSeparator();
    tb->AddTool(wxID_UNDO, "Undo", wxBITMAP(tb_undo), "Undo");
    tb->AddTool(wxID_REDO, "Redo", wxBITMAP(tb_redo), "Redo");
    tb->AddSeparator();
    tb->AddCheckTool(HALYARD_WRAP_LINES, "Wrap", wxBITMAP(tb_wrap),
                     wxNullBitmap, "Wrap Lines");
    tb->AddTool(HALYARD_TEXT_SIZE_INC, "Bigger text", wxBITMAP(tb_sizeinc),
                "Increase Text Size");
    tb->AddTool(HALYARD_TEXT_SIZE_DEC, "Smaller text", wxBITMAP(tb_sizedec),
                "Decrease Text Size");
    tb->Realize();

    // Create a wxSashLayoutWindow to hold our tree widget.
    mTreeContainer = new wxSashLayoutWindow(this, -1);
	mTreeContainer->SetOrientation(wxLAYOUT_VERTICAL);
	mTreeContainer->SetAlignment(wxLAYOUT_LEFT);
	mTreeContainer->SetSashVisible(wxSASH_RIGHT, TRUE);
    mTreeContainer->SetMinimumSizeX(150);
	mTreeContainer->SetDefaultSize(wxSize(150, 0 /* unused */));

    // Create our tree widget.
    mTree = new ScriptTree(mTreeContainer);

    // Create a wxSashLayoutWindow to hold our notebook widget.
    wxSashLayoutWindow *notebook_container =
        new wxSashLayoutWindow(this, -1);
    SetMainWindow(notebook_container);

    // Create a document notebook, delegate menu events to it, and put
    // it in charge of our title bar.
    mNotebook = new DocNotebook(notebook_container);
    mDelegator.SetDelegate(mNotebook);
    mNotebook->SetFrameToTitle(this);

    // Set an appropriate default window size and load our frame layout.
    SetSize(wxSize(950, 650));
    LoadFrameLayout();

    // Update our identifier database for syntax highlighting and indentation
    UpdateIdentifierInformation();

    // If we have a Halyard program already, open up the start script.
    if (TInterpreterManager::HaveInstance() &&
        TInterpreterManager::GetInstance()->InterpreterHasBegun())
    {
        FileSystem::Path start_script =
            FileSystem::GetScriptsDirectory().AddComponent("start.ss");
        wxString filename = start_script.ToNativePathString().c_str();
        OpenDocumentInternal(filename);
    }
}

ScriptEditor::~ScriptEditor() {
    ASSERT(sFrame);
    sFrame = NULL;
}

void ScriptEditor::LoadSashLayout(wxConfigBase *inConfig) {
    long minimum = mTreeContainer->GetMinimumSizeX();
    long script_tree_width = minimum;
	inConfig->Read("ScriptTreeWidth", &script_tree_width);
    if (script_tree_width < minimum)
        script_tree_width = minimum;
	mTreeContainer->SetDefaultSize(wxSize(script_tree_width, 0 /*unused*/));
}

void ScriptEditor::SaveSashLayout(wxConfigBase *inConfig) {
	inConfig->Write("ScriptTreeWidth", mTreeContainer->GetSize().GetWidth());
}

bool ScriptEditor::ProcessEvent(wxEvent& event) {
    bool result;
    if (mDelegator.DelegateEvent(event, &result))
        return result;
    else
        return wxFrame::ProcessEvent(event);
}

int ScriptEditor::GetTextSize() {
    int size = 10;
	wxConfigBase *config = wxConfigBase::Get();
	config->Read("/ScriptEditor/TextSize", &size);
    return size;
}

void ScriptEditor::SetTextSize(int size) {
	wxConfigBase *config = wxConfigBase::Get();
    config->Write("/ScriptEditor/TextSize", size);
    
    // Push our change to all open documents.
    for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++)
        mNotebook->GetDocument(i)->SetTextSize(size);
}

void ScriptEditor::ChangeTextSize(int delta) {
    SetTextSize(GetTextSize() + delta);
}

void ScriptEditor::OpenDocumentInternal(const wxString &path, int line) {
    // If the document is already open, show it.
    for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++) {
        if (mNotebook->GetDocument(i)->GetDocumentPath() == path) {
            mNotebook->SelectDocument(i);
            ScriptDoc *doc =
                dynamic_cast<ScriptDoc*>(mNotebook->GetDocument(i));
            ASSERT(doc);
            doc->GotoLineEnsureVisible(line-1);
            return;
        }
    }
    
    ScriptDoc *doc = new ScriptDoc(mNotebook, -1, GetTextSize(), path);
    mNotebook->AddDocument(doc);
    doc->GotoLineEnsureVisible(line-1);
}

void ScriptEditor::ShowDefinitionInternal(const wxString &identifier) {
    
    // Get our ScriptEditorDB.
    ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
    if (!db) {
        ::wxBell();
        SetStatusText("No definition database available.");
        return;
    }

    // Look up the word in the database.
    ScriptEditorDB::Definitions defs = 
        db->FindDefinitions(identifier.Lower().mb_str());
    if (defs.empty()) {
        ::wxBell();
        SetStatusText("Can't find definition of \"" + identifier + "\".");
        return;
    } else if (defs.size() == 1) {
        // Jump to the appropriate file and line.
        OpenDocument(defs[0].GetNativePath().c_str(), defs[0].line_number);
    } else {
        MetaDotDlg dlg(this, defs);
        if (dlg.ShowModal() == wxID_OK) {
            ScriptEditorDB::Definition def = dlg.GetChosenDef();
            OpenDocument(def.GetNativePath().c_str(), def.line_number);
        }
    }
}

void ScriptEditor::HighlightFileInternal(const wxString &path) {
    mTree->HighlightFile(path);
}

void ScriptEditor::NotifyReloadScriptSucceeded() {
    UpdateIdentifierInformation();
}

void ScriptEditor::UpdateIdentifierInformation() {
    // If possible, fetch an identifier list.
    if (TInterpreter::HaveInstance())
        mIdentifiers = TInterpreter::GetInstance()->GetKnownIdentifiers();

    // Try to update our definition database.
    ScriptEditorDB *db = TInterpreterManager::GetScriptEditorDB();
    if (db)
        db->UpdateDatabase();
    
    // Now tell all of our tabs to update their syntax highlighting and 
    // indentation information.
    for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++) {
        ScriptDoc *doc =
            dynamic_cast<ScriptDoc*>(mNotebook->GetDocument(i));
        ASSERT(doc);
        doc->UpdateIdentifierInformation();
    }
}

void ScriptEditor::OnActivate(wxActivateEvent &event) {
    // Don't do anything if we're deactivating.
    if (!event.GetActive())
        return;

    // Shield our activate event processing from being called recursively,
    // because some of our processing may involve popping up dialogs which
    // temporarily activate and deactivate us.
    if (!mProcessingActivateEvent) {
        mProcessingActivateEvent = true;
        try {

            // Check to see if any documents have changed.
            if (event.GetActive())
                for (size_t i = 0; i < mNotebook->GetDocumentCount(); i++)
                    mNotebook->GetDocument(i)->OfferToReloadIfChanged();

        } catch (...) {
            mProcessingActivateEvent = false;
            throw;
        }
        mProcessingActivateEvent = false;
    }
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
    mNotebook->AddDocument(new ScriptDoc(mNotebook, -1, GetTextSize()));
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
            OpenDocumentInternal(paths[i]);
    }
}

void ScriptEditor::OnCloseWindow(wxCommandEvent &event) {
    Close();
}

void ScriptEditor::DisableUiItem(wxUpdateUIEvent &event) {
    event.Enable(false);
}

void ScriptEditor::OnIncreaseTextSize(wxCommandEvent &event) {
    ChangeTextSize(1);
}

void ScriptEditor::OnDecreaseTextSize(wxCommandEvent &event) {
    ChangeTextSize(-1);
}
