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

#if !defined (Cursor_H)
#define Cursor_H

// We need to use a namespace here because of a name clash with QuickTime.
BEGIN_NAMESPACE_FIVEL

class Cursor;
typedef shared_ptr<Cursor> CursorPtr;

/// Abstract interface to cursors, including OS cursors and element-based
/// cursors.
///
/// Note that *arbitrary* Cursor objects should only be stored in two
/// places: Stage::mCurrentCursor and in the CursorManager.  This is so we
/// can track down all references to an element-based cursor when that
/// element is deleted.
///
/// SystemCursor objects can be created and stored anywhere.
class Cursor : boost::noncopyable {
protected:
    /// Set the cursor of the stage to the specified cursor (if possible).
    void SetStageCursorTo(wxCursor &cursor);

public:
    /// Destroy a cursor object.  This is virtual so that 'delete' can find
    /// our subclass destructors.
    virtual ~Cursor() {}

    /// Make this cursor the current cursor.
    virtual void SetStageCursor(const wxPoint &point) = 0;

    /// Called to notify the cursor that the mouse has moved.  This is
    /// only called between calls to SetStageCursor and UnsetStageCursor.
    ///
    /// This function is only be called when processing OS events, so it's
    /// possible to trigger a screen refresh without prematurely displaying
    /// partially-created elements.
    virtual void MoveCursor(const wxPoint &point) {}

    /// Called when changing from this cursor to a different one.
    virtual void UnsetStageCursor() {}

    /// Create a Cursor object from a wxWidgets cursor.
    static CursorPtr System(wxCursor inCursor);

    /// Create a Cursor object from a wxWidgets cursor ID value.
    static CursorPtr System(int inCursorId);
};

/// A standard OS cursor.
class SystemCursor : public Cursor {
    wxCursor mCursor;

public:
    /// Create a SystemCursor from a wxCursor.
    SystemCursor(wxCursor cursor) : mCursor(cursor) {}
    virtual void SetStageCursor(const wxPoint &point);
};

END_NAMESPACE_FIVEL

#endif // Cursor_H
