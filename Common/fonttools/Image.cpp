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

void PngImage::DrawPixmap(GraphicsTools::Point inPoint,
			  GraphicsTools::Pixmap &inPixmap)
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
