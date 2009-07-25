// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#ifndef SpecFile_H
#define SpecFile_H

#include <string>
#include <map>
#include "FileSet.h"

class SpecFile {
public:
    SpecFile(const boost::filesystem::path &path);

    std::string url() const { return mUrl; }
    std::string build() const { return mBuild; }
    const FileSet &manifest() const { return mManifest; }

private:
    typedef std::map<std::string, std::string> StringMap;

    // Parse the header, leaving mContents with just the manifest
    StringMap parseHeader();
    
    std::string mContents;
    StringMap mHeader;
    std::string mUrl, mBuild;
    FileSet mManifest;
};

#endif // FileSet_H
