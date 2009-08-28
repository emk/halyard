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

    // These are the inputs we get when we are launched, representing
    // the directory containing our update, and the directory our program
    // is installed in.
    path mSrcRoot, mDestRoot;

    // This is the parsed contents of the spec file for the new version
    // of the program (the update, read out of mSrcRoot).
    SpecFile mSpecFile;

    // This is the directory that we will be reading our new (update)
    // manifessts from.  Our old manifests should just be in mDestRoot.
    path mSrcManifestDir;

    // These are the unions of the manifests in our new (update) and old
    // (base) manifests.  These are the inputs to our algorithm that
    // determines all of the copies and moves we will need to do.
    FileSet mUpdateFiles, mExistingFiles;

    // This is the set of file copies that we will have to do, in the
    // order that we will need to do them.
    CopyVector mCopies;

    void LockDestinationDirectory();
    void UnlockDestinationDirectory();
};

#endif // UpdateInstaler_H
