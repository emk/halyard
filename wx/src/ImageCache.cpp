// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include <wx/image.h>

#include "ImageCache.h"

// Tuning parameters for our cache.
static const size_t DEFAULT_MAX_BYTES = 2 * 1024 * 1024; // 2 megabytes
static const time_t AGE_MARGIN = 120;                    // in seconds
static const size_t SIZE_FACTOR = 3;

ImageCache::ImageCache()
{
    mMaxBytes = DEFAULT_MAX_BYTES;
    mCurrentBytes = 0;
}

ImageCache::~ImageCache()
{
    // Do nothing, for now.
}

size_t ImageCache::ImageSize(const wxBitmap &inBitmap)
{
    // TODO - This is somewhat approximate.
    return inBitmap.GetWidth() * inBitmap.GetHeight() * 3;
}

ImageCache::Cache::iterator
ImageCache::BetterToPurge(Cache::iterator inA, Cache::iterator inB)
{
	size_t size_a = ImageSize(inA->second.bitmap);
	size_t size_b = ImageSize(inB->second.bitmap);

	// If one of the images is much older than the other, purge it.  It
	// doesn't take *too* long to load images.
	if (inA->second.last_used > inB->second.last_used + AGE_MARGIN)
		return inA;
	if (inB->second.last_used > inA->second.last_used + AGE_MARGIN)
		return inB;

	// If one of the images is much bigger than the other, purge it.  Our
	// cache benefits small images most, because there appears to be
	// a large constant time overhead on loading images, regardless of
	// size.  Curious.
	if (size_a > SIZE_FACTOR * size_b)
		return inA;
	if (size_b > SIZE_FACTOR * size_a)
		return inB;

	// Purge whichever image is least popular.
	return (inA->second.count > inB->second.count) ? inA : inB;
}

void ImageCache::RequireFreeSpace(size_t inSpaceNeeded)
{
	ASSERT(inSpaceNeeded <= GetMaxCacheSize());

	// Loop until we've got enough space.
	while (mCurrentBytes + inSpaceNeeded > mMaxBytes)
	{
		ASSERT(mCache.begin() != mCache.end());

		// Loop through the cache, looking for something to purge.
		Cache::iterator i = mCache.begin();
		Cache::iterator candidate = i++;
		for (; i != mCache.end(); ++i)
			candidate = BetterToPurge(candidate, i);

		// Purge it.
		gDebugLog.Log("Purging image: %s", candidate->first.c_str());
		mCurrentBytes -= ImageSize(candidate->second.bitmap);
		mCache.erase(candidate);
	}
}

wxBitmap ImageCache::GetBitmap(wxString inPath)
{
    std::string path = inPath.mb_str();
    
    // Look for the image in our cache.
    Cache::iterator found = mCache.find(path);
    if (found != mCache.end())
    {
		found->second.count++;
		found->second.last_used = ::wxGetUTCTime();
		return found->second.bitmap;
    }

    // We're going to have to load the image.
    wxImage image;
    image.LoadFile(inPath);
    if (!image.Ok())
	{
		// Load failed, return an empty bitmap.
		wxBitmap bitmap;
		ASSERT(!bitmap.Ok());
		return bitmap; 
	}
    // We MUST use an explicit bit depth here, because wxWindows gets
    // confused about mask colors when using DDBs instead of DIBs.
    // Symptom: all colors equivalent to 24-bit mask color using a 16-bit
    // comparison become transparent.
	wxBitmap bitmap(image, image.HasAlpha() ? 32 : 24);

    // If the image is too big to cache, just return it.
	size_t size = ImageSize(image);
	if (size > GetMaxCacheSize() / 2)
		return bitmap;

	// Make sure there's enough room in our cache to hold this image.
	RequireFreeSpace(size);

	// Store the image into our cache, and return it.
	mCurrentBytes += size;
	CachedImage cached;
	cached.bitmap    = bitmap;
	cached.last_used = ::wxGetUTCTime();
	cached.count     = 1;
	mCache.insert(Cache::value_type(path, cached));
	return bitmap;
}

void ImageCache::NotifyScriptReload()
{
    // Dump our entire cache when the script gets reloaded.
    mCache.clear();
	mCurrentBytes = 0;
}
