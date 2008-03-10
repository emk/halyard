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

#include "TamaleHeaders.h"
#include <wx/imaglist.h>
#include "TamaleTreeCtrl.h"


//=========================================================================
//  TamaleTreeCtrl Methods
//=========================================================================

BEGIN_EVENT_TABLE(TamaleTreeCtrl, wxTreeCtrl)
    EVT_LEFT_DCLICK(TamaleTreeCtrl::OnLeftDClick)
    EVT_RIGHT_DOWN(TamaleTreeCtrl::OnRightDown)
	EVT_TREE_BEGIN_LABEL_EDIT(wxID_ANY, TamaleTreeCtrl::OnBeginLabelEdit)
	EVT_TREE_END_LABEL_EDIT(wxID_ANY, TamaleTreeCtrl::OnEndLabelEdit)
	EVT_TREE_BEGIN_DRAG(wxID_ANY, TamaleTreeCtrl::OnBeginDrag)
	EVT_TREE_END_DRAG(wxID_ANY, TamaleTreeCtrl::OnEndDrag)
    EVT_MOTION(TamaleTreeCtrl::OnMouseMoved)
END_EVENT_TABLE()

TamaleTreeCtrl::TamaleTreeCtrl(wxWindow *inParent, wxWindowID inId,
                               const wxPoint &inPos,
                               const wxSize &inSize,
                               long inStyle)
    : wxTreeCtrl(inParent, inId, inPos, inSize, inStyle),
      mDragItemData(NULL)
{
    BuildIconList();
}

void TamaleTreeCtrl::SetIcon(wxTreeItemId id, int closed_icon, int open_icon)
{
    if (open_icon = -1)
        open_icon = closed_icon;
    SetItemImage(id, closed_icon, wxTreeItemIcon_Normal);
    SetItemImage(id, closed_icon, wxTreeItemIcon_Selected);
    SetItemImage(id, open_icon, wxTreeItemIcon_Expanded);
    SetItemImage(id, open_icon, wxTreeItemIcon_SelectedExpanded);
}

void TamaleTreeCtrl::BuildIconList()
{
    // This should match the enumeration of icons in our class
    // declaration.
    wxImageList *images = new wxImageList(16, 16, TRUE);
    images->Add(wxICON(ic_card));
    images->Add(wxICON(ic_document));
    images->Add(wxICON(ic_folder_closed));
    images->Add(wxICON(ic_folder_open));
    images->Add(wxICON(ic_script));
    images->Add(wxICON(ic_keyword));
    images->Add(wxICON(ic_function));
    images->Add(wxICON(ic_variable));
    images->Add(wxICON(ic_constant));
    images->Add(wxICON(ic_class));
    images->Add(wxICON(ic_template));
    images->Add(wxICON(ic_folder_closed)); //images->Add(wxICON(ic_group));
    images->Add(wxICON(ic_folder_closed)); //images->Add(wxICON(ic_sequence));
    images->Add(wxICON(ic_unknown));       //images->Add(wxICON(ic_element));
    images->Add(wxICON(ic_unknown));
    AssignImageList(images);
}

TamaleTreeItemData *
TamaleTreeCtrl::GetTamaleTreeItemData(wxTreeItemId inId)
{
	return dynamic_cast<TamaleTreeItemData*>(GetItemData(inId));
}

TamaleTreeItemData *
TamaleTreeCtrl::GetTamaleTreeItemData(wxMouseEvent &inEvent)
{
    wxTreeItemId id = HitTest(inEvent.GetPosition());
	return id ? GetTamaleTreeItemData(id) : NULL;
}

TamaleTreeItemData *
TamaleTreeCtrl::GetTamaleTreeItemData(wxTreeEvent &inEvent)
{
	return GetTamaleTreeItemData(inEvent.GetItem());
}

void TamaleTreeCtrl::OnLeftDClick(wxMouseEvent& event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data)
		data->OnLeftDClick(event);
    else
        event.Skip();
}

void TamaleTreeCtrl::OnRightDown(wxMouseEvent& event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data)
		data->OnRightDown(event);
}

void TamaleTreeCtrl::OnBeginLabelEdit(wxTreeEvent &event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data)
		data->OnBeginLabelEdit(event);	
}

void TamaleTreeCtrl::OnEndLabelEdit(wxTreeEvent &event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data)
		data->OnEndLabelEdit(event);	
}

void TamaleTreeCtrl::OnBeginDrag(wxTreeEvent& event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data && data->CanBeDragged())
	{
		event.Allow();
		mDragItemData = data;
	}
}

void TamaleTreeCtrl::OnEndDrag(wxTreeEvent& event)
{
	TamaleTreeItemData *data = GetTamaleTreeItemData(event);
	if (data && data->CanAcceptDrag(mDragItemData))
		data->DragDone(mDragItemData);
	mDragItemData = NULL;
}

void TamaleTreeCtrl::OnMouseMoved(wxMouseEvent& event)
{
	if (!mDragItemData)
		SetCursor(*wxSTANDARD_CURSOR);
	else
	{
		TamaleTreeItemData *data = GetTamaleTreeItemData(event);
		if (data && data != mDragItemData &&
			data->CanAcceptDrag(mDragItemData))
			SetCursor(*wxSTANDARD_CURSOR);
		else
			SetCursor(wxCursor(wxCURSOR_NO_ENTRY));
	}
}


//=========================================================================
//  TamaleTreeItemData Methods
//=========================================================================

TamaleTreeItemData::TamaleTreeItemData(TamaleTreeCtrl *inTreeCtrl)
	: mTreeCtrl(inTreeCtrl)
{
	ASSERT(mTreeCtrl != NULL);
}

void TamaleTreeItemData::OnBeginLabelEdit(wxTreeEvent &event)
{
	// By default, do not allow nodes in our tree to be renamed.
	event.Veto();
}
