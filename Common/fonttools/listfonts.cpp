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
		std::cerr << std::endl << error.what() << std::endl;
		return 1;
	}
	catch (...)
	{
		std::cerr << std::endl
				  << "An unknown exception occurred!" << std::endl;
		return 1;
	}

	return 0;
}
