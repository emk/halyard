#include <gd.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

class Image {
private:
    gdImagePtr m_image;

public:
    Image(int width, int height);
    ~Image();

    void draw_bitmap(FT_Bitmap *bitmap, FT_Int xpos, FT_Int ypos);

    void save(const char *filename);
};

