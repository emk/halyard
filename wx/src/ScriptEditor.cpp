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
#include <wx/stc/stc.h>
#include "ScriptEditor.h"
#include "FiveLApp.h"

// Only so we can find our parent window.
#include "StageFrame.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  ScriptTextCtrl Definition & Methods
//=========================================================================

class ScriptTextCtrl : public wxStyledTextCtrl {
    enum { MARGIN_FOLD = 2 };

    typedef std::vector<FIVEL_NS TScriptIdentifier> IdentifierList;

    IdentifierList mIdentifiers;

public:    
    ScriptTextCtrl(wxWindow *parent);

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

    void OnMarginClick(wxStyledTextEvent &event);
    void OnCharAdded(wxStyledTextEvent &event);
    void OnUpdateTextUI(wxStyledTextEvent &event);
    void OnKeyDown(wxKeyEvent &event);

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
    EVT_STC_MARGINCLICK(-1, ScriptTextCtrl::OnMarginClick)
    EVT_STC_CHARADDED(-1, ScriptTextCtrl::OnCharAdded)
    EVT_STC_UPDATEUI(-1, ScriptTextCtrl::OnUpdateTextUI)
    EVT_KEY_DOWN(ScriptTextCtrl::OnKeyDown)
END_EVENT_TABLE()

ScriptTextCtrl::ScriptTextCtrl(wxWindow *parent)
    : wxStyledTextCtrl(parent, -1)
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
    // Handle TAB ourselves.
    if (event.GetKeyCode() == WXK_TAB && !AutoCompActive() &&
        !event.ControlDown() && !event.AltDown() &&
        !event.MetaDown() && !event.ShiftDown())
    {
        IndentSelection();
    } else {
        event.Skip();
    }
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
    while (word_end < line.length() && IsIdentifierChar(line[word_end]))
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
//  ScriptEditor Methods
//=========================================================================

ScriptEditor *ScriptEditor::sFrame = NULL;

void ScriptEditor::MaybeCreateFrame() {
    if (!sFrame)
        new ScriptEditor;
    sFrame->Show();
    sFrame->Raise();    
}

void ScriptEditor::NewScript() {
    MaybeCreateFrame();
    sFrame->DoNewScript();
}

void ScriptEditor::OpenScript() {
    MaybeCreateFrame();
    sFrame->DoOpenScript();    
}

ScriptEditor::ScriptEditor()
    : wxFrame(wxGetApp().GetStageFrame(), -1, wxGetApp().GetAppName(),
              wxDefaultPosition, wxSize(850, 700), wxDEFAULT_FRAME_STYLE)
{
    // Set up the static variable pointing to this frame.
    ASSERT(!sFrame);
    sFrame = this;

    mEditor = new ScriptTextCtrl(this);
}

ScriptEditor::~ScriptEditor() {
    ASSERT(sFrame);
    sFrame = NULL;
}

void ScriptEditor::DoNewScript() {
    
}

void ScriptEditor::DoOpenScript() {
    
}
