// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef ProgramTree_H
#define ProgramTree_H

#include <map>

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

	ItemMap mCardMap;

	bool mHaveLastHighlightedItem;
	wxTreeItemId mLastHighlightedItem;

	enum {
		MINIMUM_WIDTH = 150
	};

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
