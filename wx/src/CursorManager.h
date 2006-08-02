// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#if !defined (CursorManager_H)
#define CursorManager_H

/// Centralized manager class for cursor resources.
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


