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

#include <string.h>
#include <iostream.h>

#include "UpdateInstaller.h"

int main(int argc, char ** argv) {
	if (argc != 3) {
		cerr << "Incorrect number of parameters: " << argc - 1 << endl;
		cerr << "Usage:" << endl << "UpdateInstaller update_directory root_directory" << endl;
		exit(1);
	} 

	path update_dir(argv[1], native);
	path root_dir(argv[2], native);

	UpdatedInstaller inst(update_dir, root_dir);
	inst.install();
}

UpdateInstaller::UpdateInstaller(path update_dir, path root_dir) {
	
}