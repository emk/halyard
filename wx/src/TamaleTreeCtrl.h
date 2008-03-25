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

#ifndef TamaleTreeCtrl_H
#define TamaleTreeCtrl_H

#include <wx/treectrl.h>

class TamaleTreeItemData;

/// Subclass of wxTreeCtrl shared between StageFrame and ScriptEditor.
class TamaleTreeCtrl : public wxTreeCtrl
{
	DECLARE_EVENT_TABLE()

	TamaleTreeItemData *mDragItemData;

	TamaleTreeItemData *GetTamaleTreeItemData(wxTreeItemId inId);
	TamaleTreeItemData *GetTamaleTreeItemData(wxMouseEvent &inEvent);
	TamaleTreeItemData *GetTamaleTreeItemData(wxTreeEvent &inEvent);

public:
    /// Any of these icons may be used by nodes in the TamaleTree.
    enum {
        ICON_CARD,
        ICON_DOCUMENT,
        ICON_FOLDER_CLOSED,
        ICON_FOLDER_OPEN,
        ICON_SCRIPT,
        ICON_KEYWORD,
        ICON_FUNCTION,
        ICON_VARIABLE,
        ICON_CONSTANT,
        ICON_CLASS,
        ICON_TEMPLATE,
        ICON_GROUP,
        ICON_SEQUENCE,
        ICON_ELEMENT,
        ICON_UNKNOWN
    };

    /// Create a new TamaleTreeCtrl.
    TamaleTreeCtrl(wxWindow *inParent,
                   wxWindowID inId = wxID_ANY,
                   const wxPoint &inPos = wxDefaultPosition,
                   const wxSize &inSize = wxDefaultSize,
                   long inStyle = wxTR_HAS_BUTTONS|wxTR_LINES_AT_ROOT);


    /// Set the icon of the specified wxTreeItemId.
    void SetIcon(wxTreeItemId id, int closed_icon, int open_icon = -1);

private:
	void BuildIconList();

	void OnLeftDClick(wxMouseEvent& event);
	void OnRightDown(wxMouseEvent& event);
	void OnBeginLabelEdit(wxTreeEvent &event);
	void OnEndLabelEdit(wxTreeEvent &event);
	void OnBeginDrag(wxTreeEvent& event);
	void OnEndDrag(wxTreeEvent& event);
	void OnMouseMoved(wxMouseEvent& event);
};


///  This class respresents a "smart" node in our TamaleTreeCtrl.  Most
///  node-specific events will be passed to a subclass of
///  TamaleTreeItemData by our event handlers.
class TamaleTreeItemData : public wxTreeItemData
{
	TamaleTreeCtrl *mTreeCtrl;

public:
	TamaleTreeItemData(TamaleTreeCtrl *inTreeCtrl);
	TamaleTreeCtrl *GetTree() { return mTreeCtrl; }

	virtual void OnLeftDClick(wxMouseEvent& event) {}
	virtual void OnRightDown(wxMouseEvent& event) {}
	virtual void OnBeginLabelEdit(wxTreeEvent &event);
	virtual void OnEndLabelEdit(wxTreeEvent &event) {}

	virtual bool CanBeDragged() { return false; }
	virtual bool CanAcceptDrag(TamaleTreeItemData *inItem) { return false; }
	virtual void DragDone(TamaleTreeItemData *inItem) {}
};

#endif // TamaleTreeCtrl_H
