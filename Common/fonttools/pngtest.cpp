#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <wchar.h>
#include <assert.h>

#include "Image.h"
#include "../Typography.h"

#define ASSERT(x) assert(x)

using namespace Typography;

typedef struct {
    char *font;
    char *metrics;
} font_info;

font_info fonts[] =
    {{"../Fonts/n019003l.pfb", // Nimbus Sans Regular
      "../Fonts/n019003l.afm"}, 
     {"../Fonts/n021003l.pfb", // Nimbus Roman Regular
      "../Fonts/n021003l.afm"}, 
     {"../Fonts/a010013l.pfb", // URW Gothic Book
      "../Fonts/a010013l.afm"}, 
     {"../Fonts/b018012l.pfb", // URW Bookman Light
      "../Fonts/b018012l.afm"}, 
     {"../Fonts/c059013l.pfb", // Century Schlbk Roman
      "../Fonts/c059013l.afm"}, 
     {"../Fonts/c0648bt_.pfb", // Bitstream Charter Reg.
      "../Fonts/c0648bt_.afm"},
     {"../Fonts/timR14.pcf",   // Times 14 bitmap
      NULL},
     {"../Fonts/s050000l.pfb", // Symbol
      "../Fonts/s050000l.afm"}, 
     {"../Fonts/d050000l.pfb", // Dingbats
      "../Fonts/s050000l.afm"}
   };


//=========================================================================
//  ImageTextRenderingEngine
//=========================================================================
//  A subclass of TextRenderingEngine which draws into an Image (for
//  testing purposes).

class ImageTextRenderingEngine : public TextRenderingEngine {
    Image *mImage;

public:
    ImageTextRenderingEngine(const wchar_t *inTextBegin,
			     const wchar_t *inTextEnd,
			     AbstractFace *inFace,
			     Point inPosition,
			     Distance inLineLength,
			     Justification inJustification,
			     Image *inImage)
	: TextRenderingEngine(inTextBegin, inTextEnd, inFace, inPosition,
			      inLineLength, inJustification),
	  mImage(inImage) {}
    
protected:
    virtual void DrawBitmap(FT_Bitmap *inBitmap, Point inPosition);
};

void ImageTextRenderingEngine::DrawBitmap(FT_Bitmap *inBitmap,
					  Point inPosition)
{
    mImage->draw_bitmap(inBitmap, inPosition.x, inPosition.y);
}


//=========================================================================
//  Test Program
//=========================================================================

Library *gLibrary;
Image *gImage;

void show(const wchar_t *inText, int inFont, int inSize, Point inPos,
	  Distance inLength, Justification inJustification)
{
    Face face(*gLibrary, fonts[inFont].font, fonts[inFont].metrics, inSize);
    Face symbol(*gLibrary, fonts[7].font, fonts[7].metrics, inSize);
    Face dingbats(*gLibrary, fonts[8].font, fonts[8].metrics, inSize);
    FaceStack stack(&face);
    stack.AddSecondaryFace(&symbol);
    stack.AddSecondaryFace(&dingbats);
    ImageTextRenderingEngine engine(inText, inText + wcslen(inText), &stack,
				    inPos, inLength, inJustification, gImage);
    engine.RenderText();    
}

int main (int argc, char **argv) {
    try {
	// Allocate an image for our output.
	Image image(640, 480);
	gImage = &image;

	// Initialize a typography library.
	Library library;
	gLibrary = &library;

	// Dump all entries in the specified font.
#if 0
	Face sym(library, fonts[8].font, fonts[8].metrics, 24);
	for (unsigned int code = 0; code <= 0xFFFF; code++)
	    if (sym.GetGlyphIndex(code))
		printf("0x%X\n", code);
	exit(0);
#endif

	// Display a title.
	show(L"Font Engine Demo",
	     5, 36, Point(10, 50), 620, kCenterJustification);

	// Display some text samples.
	for (int fi = 0; fi < 7; fi++) {
	    wchar_t *str =
		wcsdup(L"The quick brown fox jumped over the lazy dog. DdT.");
	    str[wcslen(str)-4] = 0x2206;
	    str[wcslen(str)-3] = 0x03B4;
	    show(str, fi, (fi==6)?14:14, Point(10, 100 + fi * 20), 360,
		 kLeftJustification);
	}

	show(L"Font Drawing Demo (Fun, Fun!)",
	     5, 30, Point(10, 300), 360, kCenterJustification);

	wchar_t symbols[5];
	symbols[0] = 0x2206;
	symbols[1] = 0x03B4;
	symbols[2] = 0x2022;
	symbols[3] = L'a';
	symbols[4] = 0x0000;
	show(symbols, 5, 50, Point(10, 400), 360, kLeftJustification);

	Justification justifications[] =
	    {kLeftJustification, kRightJustification, kCenterJustification};
	for (int i = 0; i < 3; i++) {
	    show(L"Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
		 "sed diam nonumy eirmod tempor invidunt ut labore et dolore "
		 "magna aliquyam erat, sed diam voluptua. At vero eos et "
		 "accusam et justo duo dolores et ea rebum.",
		 5, 12, Point(370, 100 + i * 100), 260, justifications[i]);
	}

        image.save("visual-test.png");
#ifdef HAVE_EYE_OF_GNOME
	system("eog visual-test.png");
#endif
    } catch (Error &error) {
	printf("FreeType error #%d occurred.\n", error.GetErrorCode());
    }
}
