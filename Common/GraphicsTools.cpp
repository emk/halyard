// -*- Mode: C++; tab-width: 4; -*-

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
			// further, check for inColor.alpha == 0, and handle it with a
			// separate top-level loop.
			*color_cursor = Color::ApplyAlpha(inColor, 255 - *grey_cursor);
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

template class PixelMap<Color>;
template class PixelMap<Channel>;
