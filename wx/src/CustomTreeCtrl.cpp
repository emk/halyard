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
#include "AppGraphics.h"
#include <wx/imaglist.h>
#include "CustomTreeCtrl.h"


//=========================================================================
//  CustomTreeCtrl Methods
//=========================================================================

BEGIN_EVENT_TABLE(CustomTreeCtrl, wxTreeCtrl)
    EVT_LEFT_DCLICK(CustomTreeCtrl::OnLeftDClick)
    EVT_RIGHT_DOWN(CustomTreeCtrl::OnRightDown)
	EVT_TREE_BEGIN_LABEL_EDIT(wxID_ANY, CustomTreeCtrl::OnBeginLabelEdit)
	EVT_TREE_END_LABEL_EDIT(wxID_ANY, CustomTreeCtrl::OnEndLabelEdit)
	EVT_TREE_BEGIN_DRAG(wxID_ANY, CustomTreeCtrl::OnBeginDrag)
	EVT_TREE_END_DRAG(wxID_ANY, CustomTreeCtrl::OnEndDrag)
    EVT_MOTION(CustomTreeCtrl::OnMouseMoved)
END_EVENT_TABLE()

CustomTreeCtrl::CustomTreeCtrl(wxWindow *inParent, wxWindowID inId,
                               const wxPoint &inPos,
                               const wxSize &inSize,
                               long inStyle)
    : wxTreeCtrl(inParent, inId, inPos, inSize, inStyle),
      mDragItemData(NULL)
{
    BuildIconList();
}

void CustomTreeCtrl::SetIcon(wxTreeItemId id, int closed_icon, int open_icon)
{
    if (open_icon == -1)
        open_icon = closed_icon;
    SetItemImage(id, closed_icon, wxTreeItemIcon_Normal);
    SetItemImage(id, closed_icon, wxTreeItemIcon_Selected);
    SetItemImage(id, open_icon, wxTreeItemIcon_Expanded);
    SetItemImage(id, open_icon, wxTreeItemIcon_SelectedExpanded);
}

void CustomTreeCtrl::BuildIconList()
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

CustomTreeItemData *
CustomTreeCtrl::GetCustomTreeItemData(wxTreeItemId inId)
{
	return dynamic_cast<CustomTreeItemData*>(GetItemData(inId));
}

CustomTreeItemData *
CustomTreeCtrl::GetCustomTreeItemData(wxMouseEvent &inEvent)
{
    wxTreeItemId id = HitTest(inEvent.GetPosition());
	return id ? GetCustomTreeItemData(id) : NULL;
}

CustomTreeItemData *
CustomTreeCtrl::GetCustomTreeItemData(wxTreeEvent &inEvent)
{
	return GetCustomTreeItemData(inEvent.GetItem());
}

void CustomTreeCtrl::OnLeftDClick(wxMouseEvent& event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data)
		data->OnLeftDClick(event);
    else
        event.Skip();
}

void CustomTreeCtrl::OnRightDown(wxMouseEvent& event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data)
		data->OnRightDown(event);
}

void CustomTreeCtrl::OnBeginLabelEdit(wxTreeEvent &event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data)
		data->OnBeginLabelEdit(event);	
}

void CustomTreeCtrl::OnEndLabelEdit(wxTreeEvent &event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data)
		data->OnEndLabelEdit(event);	
}

void CustomTreeCtrl::OnBeginDrag(wxTreeEvent& event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data && data->CanBeDragged())
	{
		event.Allow();
		mDragItemData = data;
	}
}

void CustomTreeCtrl::OnEndDrag(wxTreeEvent& event)
{
	CustomTreeItemData *data = GetCustomTreeItemData(event);
	if (data && data->CanAcceptDrag(mDragItemData))
		data->DragDone(mDragItemData);
	mDragItemData = NULL;
}

void CustomTreeCtrl::OnMouseMoved(wxMouseEvent& event)
{
	if (!mDragItemData)
		SetCursor(*wxSTANDARD_CURSOR);
	else
	{
		CustomTreeItemData *data = GetCustomTreeItemData(event);
		if (data && data != mDragItemData &&
			data->CanAcceptDrag(mDragItemData))
			SetCursor(*wxSTANDARD_CURSOR);
		else
			SetCursor(wxCursor(wxCURSOR_NO_ENTRY));
	}
}


//=========================================================================
//  CustomTreeItemData Methods
//=========================================================================

CustomTreeItemData::CustomTreeItemData(CustomTreeCtrl *inTreeCtrl)
	: mTreeCtrl(inTreeCtrl)
{
	ASSERT(mTreeCtrl != NULL);
}

void CustomTreeItemData::OnBeginLabelEdit(wxTreeEvent &event)
{
	// By default, do not allow nodes in our tree to be renamed.
	event.Veto();
}
