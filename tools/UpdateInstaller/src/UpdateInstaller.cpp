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

#include <string>
#include <vector>
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

#include "UpdateInstaller.h"
#include "Manifest.h"

using namespace boost::filesystem;

void install_update(const path &root_path) {
	Manifest diff(root_path / "Updates/temp/MANIFEST-DIFF");

	Manifest::EntryVector::const_iterator iter = diff.entries().begin();
	for (; iter != diff.entries().end(); ++iter) {
		path src_path = root_path / "Updates/pool" / iter->digest();
		path dst_path = root_path / iter->path();
		
		ensure_dir_exists(dst_path / "..");

		copy_overwriting(src_path, dst_path);
	}

	SpecFile spec(root_path / "Updates/release.spec");
	iter = spec.manifest().entries().begin();
	for(; iter != spec.manifest().entries().end(); ++iter) {
		path src_path = root_path / "Updates/manifests" / spec.build() 
			/ iter->path();
		path dst_path = root_path / iter->path();
		
		copy_overwriting(src_path, dst_path);
	}

	copy_overwriting(root_path / "Updates/release.spec", "release.spec");
}

void ensure_dir_exists(const path &dir) {
	path curr;
	
	for (path::iterator iter = dir.begin(); iter != dir.end(); ++iter) {
		curr = curr / *iter;
		if (!exists(curr)) 
			create_directory(curr);
	}
}

void copy_overwriting(const path &src, const path &dst) {
	if (exists(dst)) 
		remove(dst);
	copy_file(src, dst);
}
