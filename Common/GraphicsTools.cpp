// -*- Mode: C++; tab-width: 4; -*-

#include "GraphicsTools.h"

using namespace GraphicsTools;

Pixmap::Pixmap(Distance inWidth, Distance inHeight)
    : width(inWidth), height(inHeight), pitch(inWidth),
      pixels(new Color[pitch * height])
{
    // All done.
}

Pixmap::~Pixmap()
{
    delete [] pixels;
}

void Pixmap::Clear(Color inColor /*= Color(255,255,255,255)*/)
{
    Color *end = pixels + pitch * height;
    for (Color *cursor = pixels; cursor < end; cursor++)
	*cursor = inColor;
}
