// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
#include <windows.h>
#include <sys/utime.h>
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/convenience.hpp"

#include "UpdateInstaller.h"
#include "Manifest.h"

using namespace boost::filesystem;

static bool IsWriteable(const path &name);
static void TouchFile(const path &name);

UpdateInstaller::UpdateInstaller(const path &src_root, const path &dst_root) {
	Manifest diff(src_root / "Updates/temp/MANIFEST-DIFF");

	Manifest::EntryVector::const_iterator iter = diff.entries().begin();
	for (; iter != diff.entries().end(); ++iter) {
		path src_path = src_root / "Updates/pool" / iter->digest();
		path dst_path = dst_root / iter->path();
        mCopies.push_back(CopySpec(src_path, dst_path));
	}

	// TODO - add test case for subset of manifests installed
	SpecFile spec(src_root / "Updates/release.spec");
	directory_iterator dir_iter(dst_root);
	for(; dir_iter != directory_iterator(); ++dir_iter) {
		if (dir_iter->leaf().substr(0, 9) == "MANIFEST.") {
			path src_path = src_root / "Updates/manifests" / spec.build() 
				/ dir_iter->leaf();
			
			mCopies.push_back(CopySpec(src_path, *dir_iter));
		}
	}

	mCopies.push_back(CopySpec(src_root / "Updates/release.spec", 
							   dst_root / "release.spec"));
}

bool UpdateInstaller::IsUpdatePossible() {
	std::vector<CopySpec>::const_iterator copy = mCopies.begin();
	for (; copy != mCopies.end(); ++copy) {
		if (!copy->IsCopyPossible()) {
			return false;
		}
	}
	return true;
}

void UpdateInstaller::InstallUpdate() {
	std::vector<CopySpec>::const_iterator copy = mCopies.begin();
	for (; copy != mCopies.end(); ++copy) {
		create_directories(copy->dest.branch_path());
		copy->CopyOverwriting();
	}
}

bool UpdateInstaller::CopySpec::IsCopyPossible() const {
	return exists(source) & IsWriteable(dest);
}

void UpdateInstaller::CopySpec::CopyOverwriting() const {
	if (exists(dest)) 
		remove(dest);
	copy_file(source, dest);
	TouchFile(dest);
}

bool IsWriteable(const path &name) {
	if (exists(name)) {
		// If we can't open the file, keep trying every 1/5th of a second
		// for 10 seconds.
		for (int i = 0; i < 50; i++) {
			FILE *file = fopen(name.native_file_string().c_str(), "a");
			if (file != NULL) {
				fclose(file);
				return true;
			}
			Sleep(200);
		}
		return false;
	}

	return true;
}

void TouchFile(const path &name) {
	utime(name.native_file_string().c_str(), NULL);
}
