// -*- Mode: C++; tab-width: 4; -*-

#include "CommonHeaders.h"
#include "GraphicsTools.h"

using namespace GraphicsTools;


//=========================================================================
// PixelMap Methods
//=========================================================================

template <class Pixel>
PixelMap<Pixel>::PixelMap(Distance inWidth, Distance inHeight)
    : width(inWidth), height(inHeight), pitch(inWidth),
      pixels(new Pixel[pitch * height])
{
    // All done.
}

template <class Pixel>
PixelMap<Pixel>::~PixelMap()
{
    delete [] pixels;
}

template <class Pixel>
void PixelMap<Pixel>::Clear(Pixel inColor)
{
	Pixel *end = pixels + pitch * height;
	for (Pixel *cursor = pixels; cursor < end; cursor++)
		*cursor = inColor;
}

template <class Pixel>
void PixelMap<Pixel>::ClipDrawOperation(Point inDrawAt,
										Distance inScreenWidth,
										Distance inScreenHeight,
										Point &outTopLeft,
										Point &outBottomRight)
{
	// Perform the clipping operation.
	Point begin = inDrawAt;
	begin.x = Max(0, Min(inScreenWidth, begin.x));
	begin.y = Max(0, Min(inScreenHeight, begin.y));
	begin = begin - inDrawAt;
	Point end = inDrawAt + Point(width, height);
	end.x = Max(0, Min(inScreenWidth, end.x));
	end.y = Max(0, Min(inScreenHeight, end.y));
	end = end - inDrawAt;
	
	// Do some sanity checks on our clipping boundaries.
	ASSERT(begin.x == end.x || // No drawing
		   (0 <= begin.x && begin.x < end.x && end.x <= width));
	ASSERT(begin.y == end.y || // No drawing
		   (0 <= begin.y && begin.y < end.y && end.y <= height));	

	outTopLeft = begin;
	outBottomRight = end;
}


//=========================================================================
// GreyMap Methods
//=========================================================================

void GreyMap::TransferToPixMap(Color inColor, PixMap *outPixMap) const
{
	ASSERT(outPixMap->width >= this->width);
	ASSERT(outPixMap->height >= this->height);

	// This is a good example of a moderately optimized PixelMap routine.
	// It uses pointer arithmetic and pitch values, avoids all
	// multiplication, and uses loops which count backwards to zero (which
	// can take advantage of special looping instructions on most
	// machines).  There are no conditionals, and all data access are in a
	// sequential order to improve the processor's caching behavior.
	Channel *grey_row_start = this->pixels;
	Color *color_row_start = outPixMap->pixels;
	for (int y = this->height; y > 0; y--)
	{
		Channel *grey_cursor = grey_row_start;
		Color *color_cursor = color_row_start;
		for (int x = this->width; x > 0; x--)
		{
			// We always do alpha-blending.  To optimize this routine
			// further, check for inColor.IsCompletelyOpaque(), and handle
			// it with a separate top-level loop.
			*color_cursor = Color::ApplyAlpha(inColor, *grey_cursor);
			grey_cursor++;
			color_cursor++;
		}
		grey_row_start += this->pitch;
		color_row_start += outPixMap->pitch;
	}
}


//=========================================================================
// Template Instantiations
//=========================================================================
// We use the full names here to avoid confusing MSVC++.

template class GraphicsTools::PixelMap<GraphicsTools::Color>;
template class GraphicsTools::PixelMap<GraphicsTools::Channel>;
