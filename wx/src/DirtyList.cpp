// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
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
			   "Merge %d %d %d %d + %d %d %d %d -> %d %d %d %d",
			   RECT_BOUNDS(inR1), RECT_BOUNDS(inR2), RECT_BOUNDS(result));
	return result;
}

void DirtyList::MergeRect(const wxRect &inRect) {
	// Check to see if an existing rect contains inRect.
	for (iterator i = begin(); i != end(); ++i) {
		if (first_is_larger(*i, inRect)) {
			wxLogTrace(TRACE_STAGE_DRAWING, "%d %d %d %d is already dirty",
					   RECT_BOUNDS(inRect));
			return;
		}
	}

	// Attempt to merge rectangles.
	wxRect merged(inRect);
	bool need_another_pass;
	do {
		need_another_pass = false;

		wxLogTrace(TRACE_STAGE_DRAWING, "Begin merge pass.");

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
