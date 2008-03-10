// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include "TCommon.h"
#include "TLogger.h"
#include "TStartup.h"
#include "lang/old5l/TParser.h"
#include "Typography.h"
#include "TStyleSheet.h"

using namespace Halyard;

using GraphicsTools::Point;
using GraphicsTools::PixMap;

//////////
// A do-nothing image.
//
class NullImage : public GraphicsTools::Image {
	void DrawPixMap(Point inPoint, PixMap &inPixmap) {}
};

NullImage gImage;

int main(int argc, char **argv)
{
	// Our resources are located relative to our parent directory.
	FileSystem::SetBaseDirectory(FileSystem::Path().AddParentComponent());

	// Set up the rest of libfivel.
	InitializeCommonCode();
	TPrimitiveTlfProcessor defstyle_processor("defstyle");
	TParser::RegisterTlfProcessor("defstyle", &defstyle_processor);
	gIndexFileManager.NewIndex("defstyle");

	const char *text = "The quick @brown@ fox jumped over the lazy dog.";
	const int repetitions = 1000;

	for (int i = 0; i < repetitions; i++)
		gStyleSheetManager.Draw("fontspeedStyle", text,
								Point(10, 10), 100, &gImage);

	std::cout << "Simulated drawing " << repetitions * strlen(text)
			  << " letters." << std::endl;

	exit(0);
}
