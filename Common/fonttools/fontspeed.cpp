// -*- Mode: C++; tab-width: 4; -*-

#include "TCommon.h"
#include "TLogger.h"
#include "TStartup.h"
#include "lang/old5l/TParser.h"
#include "Typography.h"
#include "TStyleSheet.h"

USING_NAMESPACE_FIVEL

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
