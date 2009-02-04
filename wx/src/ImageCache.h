// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#if !defined (ImageCache_H)
#define ImageCache_H

#include "TInterpreter.h"

/// A cache of recently displayed wxBitmap objects.
class ImageCache : public Halyard::TReloadNotified
{
	struct CachedImage {
		wxBitmap bitmap;
		time_t   last_used;
		int      count;
	};

	typedef std::map<std::string,CachedImage> Cache;
	size_t mMaxBytes;
	size_t mCurrentBytes;
	Cache mCache;

	size_t ImageSize(const wxBitmap &inBitmap);
	Cache::iterator BetterToPurge(Cache::iterator inA, Cache::iterator inB);
	void RequireFreeSpace(size_t inSpaceNeeded);

public:
	ImageCache();
	virtual ~ImageCache();

	size_t GetMaxCacheSize() { return mMaxBytes; }
	void SetMaxCacheSize(size_t inMaxBytes) { mMaxBytes = inMaxBytes; }

	wxBitmap GetBitmap(wxString inPath);

	void NotifyReloadScriptStarting();
};

#endif // ImageCache_H
