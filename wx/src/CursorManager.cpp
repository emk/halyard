// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/image.h>

#include "TCommon.h"
#include "TLogger.h"
#include "CursorManager.h"

CursorManager::CursorManager()
{
	// wxWindows stock cursors.
	// PORTABILITY - Not all stock cursors are guaranteed to be available
	// everywhere.
	RegisterCursor("arrow", wxNullCursor);
	RegisterCursor("cross", *wxCROSS_CURSOR);
	RegisterCursor("hand", wxCursor(wxCURSOR_HAND));
	RegisterCursor("blank", wxCursor(wxCURSOR_BLANK));
	RegisterCursor("bullseye", wxCursor(wxCURSOR_BULLSEYE));
	RegisterCursor("leftbutton", wxCursor(wxCURSOR_LEFT_BUTTON));
	RegisterCursor("middlebutton", wxCursor(wxCURSOR_MIDDLE_BUTTON));
	RegisterCursor("rightbutton", wxCursor(wxCURSOR_RIGHT_BUTTON));
	RegisterCursor("magnifier", wxCursor(wxCURSOR_MAGNIFIER));
	RegisterCursor("noentry", wxCursor(wxCURSOR_NO_ENTRY));
	RegisterCursor("paintbrush", wxCursor(wxCURSOR_PAINT_BRUSH));
	RegisterCursor("pencil", wxCursor(wxCURSOR_PENCIL));
	RegisterCursor("pointleft", wxCursor(wxCURSOR_POINT_LEFT));
	RegisterCursor("pointright", wxCursor(wxCURSOR_POINT_RIGHT));
	RegisterCursor("questionarrow", wxCursor(wxCURSOR_QUESTION_ARROW));
	RegisterCursor("spraycan", wxCursor(wxCURSOR_SPRAYCAN));
	RegisterCursor("wait", wxCursor(wxCURSOR_WAIT));
	RegisterCursor("watch", wxCursor(wxCURSOR_WATCH));

	// Old 5L stock cursors.
	// PORTABILITY - We might want to turn these into PNGs eventually,
	// or use whatever portable resource system wxWindows decides on.
	RegisterCursor("left", wxCursor("IDC_LEFT_CURSOR"));
	RegisterCursor("right", wxCursor("IDC_RIGHT_CURSOR"));
	RegisterCursor("turnleft", wxCursor("IDC_TURN_LEFT_CURSOR"));
	RegisterCursor("turnright", wxCursor("IDC_TURN_RIGHT_CURSOR"));
}

CursorManager::~CursorManager()
{
	// Do nothing.
}

wxCursor CursorManager::FindCursor(const std::string inName)
{
	CursorMap::iterator found = mCursors.find(inName);
	if (found != mCursors.end())
		return found->second;
	else
	{
		gLog.Error("Cursor not registered: %s", inName.c_str());
		return wxCursor(wxCURSOR_HAND);
	}
}

void CursorManager::RegisterCursor(const std::string inName,
								   wxCursor &inCursor)
{
	// Delete any existing cursor with this name.
	CursorMap::iterator found = mCursors.find(inName);
	if (found != mCursors.end())
	{
		gDebugLog.Log("Redefining cursor: %s", inName.c_str());
		mCursors.erase(found);
	}
	
	// Insert the new cursor.
	mCursors.insert(CursorMap::value_type(inName, inCursor));
}
 
void CursorManager::RegisterImageCursor(const std::string inName,
										const std::string inPath,
										int inHotSpotX,
										int inHotSpotY)
{
	// Load the image.
    wxImage image;
    image.LoadFile(inPath.c_str());
    if (!image.Ok())
	{
		// Display an error message, and register a plausible substitute
		// cursor instead of the one we can't find.
		gLog.Error("Cannot open cursor file '%s'", inPath.c_str());
		RegisterCursor(inName, wxCursor(wxCURSOR_HAND));
		return;
	}

	// If the cursor doesn't have a hot spot, specify one.
	if (!image.HasOption(wxCUR_HOTSPOT_X))
		image.SetOption(wxCUR_HOTSPOT_X, (inHotSpotX == -1) ? 0 : inHotSpotX);
	if (!image.HasOption(wxCUR_HOTSPOT_Y))
		image.SetOption(wxCUR_HOTSPOT_Y, (inHotSpotY == -1) ? 0 : inHotSpotY);

	// Register the cursor.
	wxCursor cursor(image);
	RegisterCursor(inName, cursor);
}
