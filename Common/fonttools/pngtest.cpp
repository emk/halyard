// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
#include <string.h>
#include <wchar.h>

#include "Image.h"
#include "Typography.h"

using namespace Typography;

char *fonts[] = {
    "Nimbus Sans L",
    "Nimbus Roman No9 L",
    "URW Gothic L",
    "URW Bookman L",
    "Century Schoolbook L",
    "Bitstream Charter",
    "URW Chancery L",
    "Times",
    NULL
};
	

//=========================================================================
//  Test Program
//=========================================================================

Image *gImage;
FamilyDatabase *gFonts;

#define SYMBOL_FACE ("Standard Symbols L")
#define DINGBAT_FACE ("Dingbats")

static void show_with_style_info(const StyledText &inText,
				 Point inPos, Distance inLength,
				 Justification inJust,
				 Distance *outBottomBound)
{
    // Draw our text.
    TextRenderingEngine engine(inText, inPos, inLength, inJust, gImage);
    engine.RenderText();
    *outBottomBound = engine.GetBottomBound();
}

static void show(const wchar_t *inText, const Style &inStyle,
		 Point inPos, Distance inLength,
		 Justification inJust,
		 Distance *outBottomBound)
{
    // Build our style run.
    StyledText styledText(inStyle);
    styledText.AppendText(inText);
    styledText.EndConstruction();

    // Draw our text.
    show_with_style_info(styledText, inPos, inLength, inJust, outBottomBound);
}

int main (int argc, char **argv) {
    try {
	// Our resources are located relative to our parent directory.
	FileSystem::SetBaseDirectory(FileSystem::Path().AddParentComponent());

	// Allocate an image for our output.
	PngImage image(640, 480);
	gImage = &image;

#if 0
	// Dump all entries in the specified font.
	FamilyDatabase *db = FamilyDatabase::GetFamilyDatabase();
	Face sym = db->GetFace("Dingbats", kRegularFaceStyle, 24);
	for (unsigned int code = 0; code <= 0xFFFF; code++)
	    if (sym.GetGlyphIndex(code))
		printf("0x%X\n", code);
	exit(0);
#endif

	// Create our base style.
	Style baseStyle("Bitstream Charter", 12);
	std::list<std::string> backups;
	backups.push_back(SYMBOL_FACE);
	backups.push_back(DINGBAT_FACE);
	baseStyle.SetBackupFamilies(backups);
	baseStyle.SetColor(Color(0, 0, 96));
	baseStyle.SetShadowColor(Color(128, 128, 0));

	// Display a title.
	Distance bodyStart;
	show(L"Font Engine Demo",
	     Style(baseStyle).SetSize(36).SetFaceStyle(kShadowFaceStyle),
	     Point(10, 20), 620, kCenterJustification, &bodyStart);
	bodyStart += 20;

	// Display some text samples.
	Distance start = bodyStart;
	Style sampleStyle(baseStyle);
	sampleStyle.SetSize(14);
	for (int fi = 0; fonts[fi] != NULL; fi++) {
	    wchar_t *str =
		wcsdup(L"The quick brown fox jumped over the lazy dog. DdT.");
	    str[wcslen(str)-4] = 0x2206;
	    str[wcslen(str)-3] = 0x03B4;
	    show(str, Style(sampleStyle).SetFamily(fonts[fi]),
		 Point(10, start), 360, kLeftJustification, &start);
	    start += 10;
	    free(str);
	}
	start += 10;

	// Do an elaborate, multi-styled paragraph.
	Style paraStyle(baseStyle);
	paraStyle.SetSize(14);
	StyledText para(paraStyle);
	para.AppendText(L"Ms. Matthews' case raises several ");
	para.ChangeStyle(Style(paraStyle).SetFaceStyle(kBoldItalicFaceStyle));
	para.AppendText(L"interesting");
	para.ChangeStyle(paraStyle);
	para.AppendText(L" questions.  ");
	para.ChangeStyle(Style(paraStyle).SetColor(Color(255, 0, 0)));
	para.AppendText(L"R");
	para.ChangeStyle(Style(paraStyle).SetColor(Color(0, 255, 0)));
	para.AppendText(L"G");
	para.ChangeStyle(Style(paraStyle).SetColor(Color(0, 0, 255)));
	para.AppendText(L"B");
	para.ChangeStyle(paraStyle);
	para.AppendText(L" color is cool.  We like several cheeses! T");
	para.ChangeStyle(Style(paraStyle).SetColor(Color(255, 0, 0)));
	para.AppendText(L".");
	para.ChangeStyle(paraStyle);
	para.AppendText(L"\nWe also have ");
	para.ChangeStyle(Style(paraStyle).SetSize(paraStyle.GetSize() + 6));
	para.AppendText(L"big");
	para.ChangeStyle(paraStyle);
	para.AppendText(L" and ");
	para.ChangeStyle(Style(paraStyle).SetSize(paraStyle.GetSize() / 2));
	para.AppendText(L"small");
	para.ChangeStyle(paraStyle);
	para.AppendText(L" text.");
	para.EndConstruction();
	show_with_style_info(para, Point(10, start), 360, kLeftJustification,
			     &start);
	start += 20;

	wchar_t symbols[5];
	symbols[0] = 0x2206;
	symbols[1] = 0x03B4;
	symbols[2] = 0x2022;
	symbols[3] = L'a';
	symbols[4] = 0x0000;
	show(symbols, Style(baseStyle).SetSize(50),
	     Point(10, start), 360, kLeftJustification, &start);

	start = bodyStart;
	Justification justifications[] =
	    {kLeftJustification, kRightJustification, kCenterJustification};
	Style justStyle(baseStyle);
	for (int i = 0; i < 3; i++) {
	    show(L"Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
		 "sed diam nonumy eirmod tempor invidunt ut labore et dolore "
		 "magna aliquyam erat, sed diam voluptua. At vero eos et "
		 "accusam et justo duo dolores et ea rebum.",
		 justStyle, Point(370, start), 260, justifications[i], &start);
	    start += 20;
	    justStyle.SetLeading(justStyle.GetLeading() - 2);
	}

	Distance junk_start;
	show(L"Little tiny letters in the background.  "
	     "Little tiny letters in the background.",
	     Style(baseStyle).SetSize(8),
	     Point(370, start), 100, kCenterJustification, &junk_start);
	show(L"AA",
	     Style(baseStyle).SetSize(50).SetColor(Color(255, 0, 0, 0x32)),
	     Point(370, start), 100, kCenterJustification, &junk_start);

        image.save("visual-test.png");
#ifdef HAVE_EYE_OF_GNOME
	system("eog visual-test.png");
#endif
    }
    catch (std::exception &error)
    {
	std::cerr << std::endl << error.what() << std::endl;
	return 1;
    }
    catch (...)
    {
	std::cerr << std::endl
		  << "An unknown exception occurred!" << std::endl;
	return 1;
    }
}
