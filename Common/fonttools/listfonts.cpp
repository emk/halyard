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
	catch (FileSystem::Error &e)
	{
		std::cerr << e << endl;
		return 1;
	}
	catch (Error &e)
	{
		std::cerr << e << endl;
		return 1;
	}
	catch (...)
	{
		std::cerr << "An error of an unknown sort occurred." << endl;
		return 1;
	}

	return 0;
}
