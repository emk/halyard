// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef DIRTYLIST_H
#define DIRTYLIST_H

/// A collection of screen rectangles which have been marked "dirty".  This
/// class does smart merging of dirty rectangles to minimize redraw.
class DirtyList {
	typedef std::vector<wxRect> RectVector;
	RectVector mRectangles;
	
public:
	typedef RectVector::iterator iterator;
	typedef RectVector::const_iterator const_iterator;

	DirtyList() {}
	
	bool empty() { return mRectangles.empty(); }
	void clear() { mRectangles.clear(); }

	iterator begin() { return mRectangles.begin(); }
	iterator end() { return mRectangles.end(); }

	const_iterator begin() const { return mRectangles.begin(); }
	const_iterator end() const { return mRectangles.end(); }

	void MergeRect(const wxRect &inRect);

    wxRect GetBounds() const;

    operator wxRegion();
};

#endif // DIRTYLIST_H
