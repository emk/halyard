#include <gd.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "GraphicsTools.h"

class PngImage : public GraphicsTools::Image {
private:
    gdImagePtr m_image;

public:
    PngImage(int width, int height);
    ~PngImage();

    virtual void DrawPixmap(GraphicsTools::Point inPoint,
			    GraphicsTools::Pixmap &inPixmap);

    void save(const char *filename);
};
