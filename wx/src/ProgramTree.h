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

#ifndef ProgramTree_H
#define ProgramTree_H

#include <wx/laywin.h>
#include <wx/treectrl.h>

BEGIN_NAMESPACE_HALYARD
class Document;
END_NAMESPACE_HALYARD

class StageFrame;
class ProgramTreeCtrl;

/// Public interface to tree widget describing a Halyard script.
class ProgramTree : public wxSashLayoutWindow, public Halyard::TReloadNotified
{
	DECLARE_EVENT_TABLE()

	typedef std::map<std::string,wxTreeItemId> ItemMap;

	ProgramTreeCtrl *mTree;

	wxTreeItemId mRootID;
	wxTreeItemId mCardsID;
	wxTreeItemId mBackgroundsID;

	ItemMap mGroupMemberMap;

	bool mHaveLastHighlightedItem;
	wxTreeItemId mLastHighlightedItem;

	enum {
		MINIMUM_WIDTH = 150
	};

    /// Return true iff the specified tree item corresponds to a card.
    bool IsCardItem(wxTreeItemId inItemId);

    /// Return true iff the specified tree item is loaded.
    bool IsLoadedItem(wxTreeItemId inItemId);

    /// Given a node name, split it into a parent component and a local
    /// component.  If the node name is "/", then set outIsRootNode to
    /// true, and return empty strings for outParentName and outLocalName.
    void AnalyzeNodeName(const std::string &inName,
                         bool &outIsRootNode,
                         std::string &outParentName,
                         std::string &outLocalName);

    /// Either find or create the group member named by inName.  The two
    /// boolean parameters specify what kind of node we want to find and/or
    /// create.
    wxTreeItemId FindOrCreateGroupMember(const std::string &inName,
                                         bool inIsCard,
                                         bool inCanUpdateIsLoaded,
                                         bool inIsLoaded);

public:
	ProgramTree(StageFrame *inStageFrame, int inID);

	//////////
	/// Notify the tree that a document has been loaded.
	///
	void RegisterDocument(Halyard::Document *inDocument);

    //////////
    /// Register a newly-loaded card with the program tree.
    ///
    void RegisterGroupMember(const wxString &inName, bool inIsCard,
                             bool inIsLoaded);

	//////////
	/// Set the default width which will be used when laying out this window.
	///
	void SetDefaultWidth(int inWidth);

	//////////
	/// Notify the program tree that script is being reloaded.
	///
    void NotifyReloadScriptStarting();

    //////////
    /// Notify the program tree that the interpreter has moved to a new card.
    ///
    void NotifyEnterCard(const wxString &inName);
};

#endif // ProgramTree_H
