// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (CursorManager_H)
#define CursorManager_H

class CursorManager
{
    typedef std::map<std::string,wxCursor> CursorMap;

    CursorMap mCursors;

public:
    CursorManager();
    virtual ~CursorManager();

    wxCursor FindCursor(const std::string inName);

    void RegisterCursor(const std::string inName, wxCursor &inCursor);
    void RegisterImageCursor(const std::string inName,
							 const std::string inPath,
							 int inHotSpotX = -1,
							 int inHotSpotY = -1);
};

#endif // CursorManager_H


