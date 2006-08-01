// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#define BOOST_FILESYSTEM_SOURCE

#include <stdio.h>
#include <windows.h>
#include "boost/filesystem/path.hpp"

#include "CommandLine.h"
#include "UpdateInstaller.h"

using namespace boost::filesystem;

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("Usage: UpdateInstaller path [command ...]\n");
		exit(1);
	}

	try {
		install_update(path(argv[1]));
	} catch (std::exception &e) {
		printf("Error: %s\n", e.what());
		exit(1);
	} catch (...) {
		printf("Unknown error.\n");
		exit(1);
	}

	if (argc > 2) {
		/* PORTABILITY - needs to be factored to work on platforms other than
		 Windows. */
		CommandLine cl(argc-2, argv+2);
		STARTUPINFO si;
		ZeroMemory( &si, sizeof(si));
		si.cb = sizeof(si);
		PROCESS_INFORMATION pi;
		LPSTR command = _strdup(cl.WindowsQuotedString().c_str());
		BOOL ret = CreateProcess(NULL, /* Application name */
								 command, /* Command line */ 
								 NULL, /* Process attributes */
								 NULL, /* Thread attributes */
								 FALSE, /* Inherit handles? */
								 0, /* Creation Flags */
								 NULL, /* Environment */
								 NULL, /* Current directory */
								 &si, /* Startup info */
								 &pi); /* Process info */
		if (!ret) {
			printf("Error: Couldn't launch external process: %s\n",
				   cl.WindowsQuotedString().c_str());
			exit(1);
		}
	}

	return 0;
}

