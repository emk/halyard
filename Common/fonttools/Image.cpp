#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <gd.h>

#include "Image.h"

Image::Image(int width, int height) {
    m_image = gdImageCreateTrueColor(width, height);
    gdImageFilledRectangle(m_image, 0, 0, width, height,
			   gdTrueColor(255, 240, 210));
    gdImageAlphaBlending(m_image, 1);
}

Image::~Image() {
    gdImageDestroy(m_image);
}

void Image::draw_bitmap(FT_Bitmap *bitmap, FT_Int xpos, FT_Int ypos) {
    assert(bitmap->pitch >= 0);

    if (bitmap->pixel_mode == ft_pixel_mode_grays) {
	/* Draw 8-bit greyscale characters. */
	assert(bitmap->num_grays == 256);
	for (int y = 0; y < bitmap->rows; y++) {
	    for (int x = 0; x < bitmap->width; x++) {
		unsigned char value = bitmap->buffer[x + bitmap->pitch * y];
		gdImageSetPixel(m_image, xpos + x, ypos + y,
				gdTrueColorAlpha(0, 0, 96, (255-value)/2));
	    }
	}
    } else {
	/* Draw 1-bit monochrome characters. */
	assert(bitmap->pixel_mode == ft_pixel_mode_mono);
	for (int y = 0; y < bitmap->rows; y++) {
	    for (int x = 0; x < bitmap->width; x++) {
		unsigned char byte = bitmap->buffer[(x/8) + bitmap->pitch * y];
		unsigned char value = ((1<<(7-(x%8))) & byte) ? 0 : 127; 
		gdImageSetPixel(m_image, xpos + x, ypos + y,
				gdTrueColorAlpha(0, 0, 96, value));
	    }
	}	
    }
}

void Image::save(const char *filename) {
    FILE *pngout = fopen(filename, "wb");
    gdImagePng(m_image, pngout);
    fclose(pngout);
}
