// -*- Mode: C++; tab-width: 4; -*-

#include <iostream>

#include "TCommon.h"
#include "FileSystem.h"
#include "Typography.h"

using namespace Typography;

int main(int argc, char **argv)
{
	try
	{
		// Our resources are located relative to our parent directory.
		FileSystem::SetBaseDirectory(FileSystem::Path().AddParentComponent());

		// Load our FamilyDatabase and dump it.
		FamilyDatabase familyDatabase;
		familyDatabase.ReadFromFontDirectory();
		familyDatabase.WriteToCache(std::cout);
	}
	catch (std::exception &error)
	{
		std::cerr << endl << error.what() << endl;
		return 1;
	}
	catch (...)
	{
		std::cerr << endl << "An unknown exception occurred!" << endl;
		return 1;
	}

	return 0;
}
