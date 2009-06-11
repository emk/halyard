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

FIRST: <#include <stdio.h>
>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <gd.h>

#include "Image.h"

PngImage::PngImage(int width, int height)
{
    m_image = gdImageCreateTrueColor(width, height);
    gdImageFilledRectangle(m_image, 0, 0, width, height,
               gdTrueColor(255, 240, 210));
    gdImageAlphaBlending(m_image, 1);
}

PngImage::~PngImage()
{
    gdImageDestroy(m_image);
}

void PngImage::DrawPixMap(GraphicsTools::Point inPoint,
              GraphicsTools::PixMap &inPixmap)
{
    for (int y = 0; y < inPixmap.height; y++)
    {
    for (int x = 0; x < inPixmap.width; x++)
    {
        GraphicsTools::Color color = inPixmap.At(x, y);
        gdImageSetPixel(m_image, inPoint.x + x, inPoint.y + y,
                gdTrueColorAlpha(color.red, color.green,
                         color.blue, color.alpha / 2));
    }
    }
}

void PngImage::save(const char *filename) {
    FILE *pngout = fopen(filename, "wb");
    gdImagePng(m_image, pngout);
    fclose(pngout);
}
