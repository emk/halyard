// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (ImageCache_H)
#define ImageCache_H

#include <string>
#include <map>

class ImageCache
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

	void NotifyScriptReload();
};

#endif // ImageCache_H
