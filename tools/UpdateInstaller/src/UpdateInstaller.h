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

#ifndef UpdateInstaller_H
#define UpdateInstaller_H

#include <boost/filesystem/path.hpp>
#include "FileSet.h"
#include "SpecFile.h"

using namespace boost::filesystem;

class UpdateInstaller {
public:
    UpdateInstaller(const path &src_root, const path &dst_root);
    
    void CalculatePoolToTreeCopiesNeeded();
    void PopulatePoolFromTree();
    void PrepareForUpdate();
    bool IsUpdatePossible();
    void InstallUpdate();

    static void DeleteLockFileForUninstall(const path &root);

private:
    struct CopySpec {
        CopySpec(path inSource, path inDest) 
            : source(inSource), dest(inDest) { }
        path source, dest;

        bool IsCopyPossible() const;
        void CopyOverwriting() const;
    };
    typedef std::vector<CopySpec> CopyVector;

    path mSrcRoot, mDestRoot;
    SpecFile mSpecFile;
    path mSrcManifestDir;
    FileSet mUpdateFiles, mExistingFiles;
    CopyVector mCopies;

    void LockDestinationDirectory();
    void UnlockDestinationDirectory();
};

#endif // UpdateInstaler_H
