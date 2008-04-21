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
#include "AppGlobals.h"
#include "DirtyList.h"

#define MERGE_PROXIMITY (20)
#define RECT_BOUNDS(R) \
	(R).GetLeft(), (R).GetTop(), (R).GetRight(), (R).GetBottom()

static bool first_is_larger(const wxRect &inFirst, const wxRect &inThan) {
	return (inFirst.x <= inThan.x &&
			inFirst.y <= inThan.y &&
			inFirst.GetRight() >= inThan.GetRight() &&
			inFirst.GetBottom() >= inThan.GetBottom());
}

static bool should_merge_rects(const wxRect &inR1, const wxRect &inR2) {
	wxRect tmp(inR1);
	tmp.Inflate(MERGE_PROXIMITY);
	return tmp.Intersects(inR2);
}

static wxRect merge_rects(const wxRect &inR1, const wxRect &inR2) {
	wxRect result(wxPoint(std::min(inR1.GetLeft(), inR2.GetLeft()),
						  std::min(inR1.GetTop(), inR2.GetTop())),
				  wxPoint(std::max(inR1.GetRight(), inR2.GetRight()),
						  std::max(inR1.GetBottom(), inR2.GetBottom())));
	wxLogTrace(TRACE_STAGE_DRAWING,
			   wxT("Merge %d %d %d %d + %d %d %d %d -> %d %d %d %d"),
			   RECT_BOUNDS(inR1), RECT_BOUNDS(inR2), RECT_BOUNDS(result));
	return result;
}

void DirtyList::MergeRect(const wxRect &inRect) {
	// Check to see if an existing rect contains inRect.
	for (iterator i = begin(); i != end(); ++i) {
		if (first_is_larger(*i, inRect)) {
			wxLogTrace(TRACE_STAGE_DRAWING, wxT("%d %d %d %d is already dirty"),
					   RECT_BOUNDS(inRect));
			return;
		}
	}

	// Attempt to merge rectangles.
	wxRect merged(inRect);
	bool need_another_pass;
	do {
		need_another_pass = false;

		wxLogTrace(TRACE_STAGE_DRAWING, wxT("Begin merge pass."));

		// We remove some elements from the vector as we go.  The
		// src/dst/erase idiom is the easiest way to do this.
		iterator src = begin();
		iterator dst = begin();
		for (; src != end(); ++src) {
			if (should_merge_rects(merged, *src)) {
				// Do the merge, and don't copy *src to our output.
				merged = merge_rects(merged, *src);
				need_another_pass = true;
			} else {
				// Skip the merge, and copy *src.
				*dst++ = *src;
			}
		}

		// Remove everything following the elements we copied.
		mRectangles.erase(dst, mRectangles.end());
	} while (need_another_pass);

	// Push the merged rectangle 
	mRectangles.push_back(merged);
}

/// Calculate the rectangle including all items in the dirty list.  Useful
/// for localized transitions, which can only run on a rectangular area.
wxRect DirtyList::GetBounds() const {
	wxLogTrace(TRACE_STAGE_DRAWING, wxT("Calculating bounds of dirty region."));

    wxRect result(0, 0, 0, 0);
    const_iterator i = begin();
    const_iterator end_i = end();
    if (i != end_i) {
        result = *i++;
        for (; i != end_i; ++i)
            result = merge_rects(result, *i);
    }
    
    wxLogTrace(TRACE_STAGE_DRAWING, wxT("Bounds of dirty region: %d %d %d %d"),
               RECT_BOUNDS(result));
    return result;
}

DirtyList::operator wxRegion() {
    wxRegion result;
    for (iterator i = begin(); i != end(); ++i)
        result.Union(*i);
    return result;
}
