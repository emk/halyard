// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef DIRTYLIST_H
#define DIRTYLIST_H

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

    operator wxRegion();
};

#endif // DIRTYLIST_H
