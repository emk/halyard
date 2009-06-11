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

#include "AppHeaders.h"
#include <wx/image.h>

#include "ImageCache.h"

using namespace Halyard;

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

size_t ImageCache::SurfaceSize(CairoSurfacePtr inSurface)
{
    // It wouldn't hurt to throw in an overhead factor for the
    // cairo_surface_t object, too.
    int height = cairo_image_surface_get_height(inSurface.get());
    int stride = cairo_image_surface_get_stride(inSurface.get());
    return height * stride;
}

ImageCache::Cache::iterator
ImageCache::BetterToPurge(Cache::iterator inA, Cache::iterator inB)
{
    size_t size_a = SurfaceSize(inA->second.surface);
    size_t size_b = SurfaceSize(inB->second.surface);

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
        gLog.Trace("halyard", "Purging image: %s", candidate->first.c_str());
        mCurrentBytes -= SurfaceSize(candidate->second.surface);
        mCache.erase(candidate);
    }
}

CairoSurfacePtr ImageCache::GetImage(wxString inPath)
{
    std::string path(inPath.mb_str());
    
    // Look for the image in our cache.
    Cache::iterator found = mCache.find(path);
    if (found != mCache.end()) {
        found->second.count++;
        found->second.last_used = ::wxGetUTCTime();
        return found->second.surface;
    }

    // We're going to have to load the image.
    wxImage image;
    image.LoadFile(inPath);
    if (!image.Ok()) {
        // Load failed, return a null surface.
        return CairoSurfacePtr();
    }
    CairoSurfacePtr surface(SurfaceFromImage(image));

    // If the image is too big to cache, just return it.
    size_t size = SurfaceSize(surface);
    if (size > GetMaxCacheSize() / 2)
        return surface;

    // Make sure there's enough room in our cache to hold this image.
    RequireFreeSpace(size);

    // Store the image into our cache, and return it.
    mCurrentBytes += size;
    CachedImage cached;
    cached.surface   = surface;
    cached.last_used = ::wxGetUTCTime();
    cached.count     = 1;
    mCache.insert(Cache::value_type(path, cached));
    return surface;
}

void ImageCache::NotifyReloadScriptStarting()
{
    // Dump our entire cache when the script gets reloaded.
    mCache.clear();
    mCurrentBytes = 0;
}

CairoSurfacePtr ImageCache::SurfaceFromImage(wxImage &inImage) {
    // If the image has a mask, convert it to an alpha channel.  This saves
    // us from needing an extra code path to handle masks.
    if (inImage.HasMask())
        inImage.InitAlpha();

    // Create a new Cairo surface to hold our image data.
    cairo_format_t format =
        inImage.HasAlpha() ? CAIRO_FORMAT_ARGB32 : CAIRO_FORMAT_RGB24;
    int width = inImage.GetWidth();
    int height = inImage.GetHeight();
    CairoSurfacePtr surface(cairo_image_surface_create(format, width, height));

    // Copy the data from our wxImage to our cairo_image_surface.
    unsigned char *image_data = inImage.GetData();
    unsigned char *alpha_data = inImage.GetAlpha();
    unsigned char *surface_data = cairo_image_surface_get_data(surface.get());
    int surface_stride = cairo_image_surface_get_stride(surface.get());
    for (int y = 0; y < height; y++) {
        unsigned char *image_row(image_data + 3*width*y);
        unsigned char *surface_row(surface_data + surface_stride*y);

        // Copy one scan line.  Note that we need to store pixels in the
        // native endianness.
        if (inImage.HasAlpha()) {
            ASSERT(alpha_data);
            unsigned char *alpha_row(alpha_data + width*y);
            for (int x = 0; x < width; x++) {
                // Premultiply each channel with the alpha data.  The
                // pre-multiplication function is adapted from
                // wxDIB::Create.
                unsigned char alpha = alpha_row[x];
                unsigned char red   = image_row[3*x+0];
                unsigned char green = image_row[3*x+1];
                unsigned char blue  = image_row[3*x+2];
                uint32 pixel =
                    alpha << 24 |
                    ((red   * alpha + 127) / 255) << 16 |
                    ((green * alpha + 127) / 255) <<  8 |
                    ((blue  * alpha + 127) / 255) <<  0;
                *reinterpret_cast<uint32 *>(surface_row + 4*x) = pixel;
            }
        } else {
            for (int x = 0; x < width; x++) {
                unsigned char red   = image_row[3*x+0];
                unsigned char green = image_row[3*x+1];
                unsigned char blue  = image_row[3*x+2];
                uint32 pixel = red << 16 | green << 8 | blue << 0;
                *reinterpret_cast<uint32 *>(surface_row + 4*x) = pixel;
            }
        }
    }

    return surface;
}
