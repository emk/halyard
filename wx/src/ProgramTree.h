// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef ProgramTree_H
#define ProgramTree_H

class StageFrame;
class Document;
class ProgramTreeCtrl;

class ProgramTree : public wxSashLayoutWindow
{
	DECLARE_EVENT_TABLE()

	typedef std::map<std::string,wxTreeItemId> ItemMap;

	ProgramTreeCtrl *mTree;

	wxTreeItemId mRootID;
	wxTreeItemId mCardsID;
	wxTreeItemId mBackgroundsID;

	ItemMap mSequenceMap;
	ItemMap mCardMap;

	bool mHaveLastHighlightedItem;
	wxTreeItemId mLastHighlightedItem;

	enum {
		MINIMUM_WIDTH = 150
	};

	//////////
	// Find the wxTreeItemId which should contain the card or sequence with
	// the specified name.  If the name contains no slashes, the container
	// will be mCardsID.  If the name contains slashes, each component of
	// the name will be used as a nested directory.  Directories will be
	// created as needed.
	//
	wxTreeItemId FindParentContainer(const std::string &inName,
									 std::string &outLocalName);

public:
	ProgramTree(StageFrame *inStageFrame, int inID);

	//////////
	// Notify the tree that a document has been loaded.
	//
	void RegisterDocument(Document *inDocument);

    //////////
    // Register a newly-loaded card with the program tree.
    //
    void RegisterCard(const wxString &inName);

	//////////
	// Set the default width which will be used when laying out this window.
	//
	void SetDefaultWidth(int inWidth);

	//////////
	// Notify the program tree that script is being reloaded.
	//
    void NotifyScriptReload();

    //////////
    // Notify the program tree that the interpreter has moved to a new card.
    //
    void NotifyEnterCard();
};

#endif // ProgramTree_H
