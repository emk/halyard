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
#include <boost/shared_ptr.hpp>
#include "FileSet.h"
#include "SpecFile.h"

using namespace boost::filesystem;

class FileOperation {
public:
    virtual bool IsPossible() const = 0;
    virtual void Perform() const = 0;

    typedef boost::shared_ptr<FileOperation> Ptr;
    typedef std::vector<Ptr> Vector;
};

class FileDelete : public FileOperation {
public:
    FileDelete(path inFile) : file(inFile) { }
    virtual bool IsPossible() const;
    virtual void Perform() const;

protected:
    path file;
};

// Represents a copy or a move from the source to the dest, depending
// on the inMove flag passed in to the constructor.  inShouldExist
// indicates whether the file is expected to exist at the source location
// before we begin our update.
class FileTransfer : public FileOperation {
public:
    FileTransfer(const path &inSource, const path &inDest, 
                 bool inMove = false, bool inShouldExist = true) 
        : mSource(inSource), mDest(inDest), mMove(inMove),
          mShouldExist(inShouldExist) { }
    virtual bool IsPossible() const;
    virtual void Perform() const;

protected:
    path mSource, mDest;

    // Should we do this as a move?  Default is to copy.
    bool mMove;

    // True if we expect the source file to exist when checking if the
    // update is possible.  This will be false when we are depending on
    // a prior transfer to put the source file into place.
    bool mShouldExist;
};

class UpdateInstaller {
public:
    UpdateInstaller(const path &src_root, const path &dst_root);
    
    void PrepareForUpdate();
    bool IsUpdatePossible();
    void InstallUpdate();

    static void DeleteLockFileForUninstall(const path &root);

private:
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

    // This is the set of files that we need to add to our new tree.
    // Some of them may be in the pool, while some may be in the tree
    // already at different locations.
    FileSet mFilesNeededForNewTree;

    // This is the set of files that we need to copy from our tree
    // to our pool.  That is, it is the set of files that have contents
    // which are needed elsewhere in the tree, but which also need to
    // remain in place.
    FileSet mFilesToCopyFromTreeToPool;
    
    // This is the set of files that we need to move from our tree
    // to our pool.  It consists of files which no longer exist with
    // the same name and contents in the tree, but which contents should
    // exist elsewhere in the tree.
    FileSet mFilesToMoveFromTreeToPool;

    // This is the set of files which we are simply going to delete from
    // our tree; we do not need these files anywhere any more.
    FileSet mFilesToDeleteFromTree;

    // All of the file operations we have to do, in the order we need
    // to do them.  This contains the files we need to move from the
    // tree to the pool, the files we need to copy from the tree to
    // the pool, the files we need to delete from the tree, the files
    // we need to copy or move from the pool back to the tree, and the
    // extra files like the new manifests and the new "release.spec"
    // that aren't included in the normal manifest lists.
    FileOperation::Vector mOperations;

    // Is the update possible, to the best of our current knowledge?
    // This can be set to false at any time before we start updating to
    // indicate that something is screwy and we should abort before we
    // screw up even further.
    bool mUpdateIsPossible;

    void CalculateFileSetsForUpdates();
    void BuildFileOperationVector();
    void BuildTreeToPoolFileOperations();
    void BuildPoolToTreeFileOperations();
    void BuildUpdaterSpecialFileOperations();
    void BuildDirectoryCleanupFileOperations();
    bool BuildCleanupRecursive(const FileSet::FilenameSet &known_files, 
                               path dir, 
                               const FileSet::FilenameSet &directories_to_keep);

    void LockDestinationDirectory();
    void UnlockDestinationDirectory();

    bool FileShouldBeInPool(const FileSet::Entry &e);
    path PathInTree(const FileSet::Entry &e);
    path PathInPool(const FileSet::Entry &e);
    path PathInPool(const std::string &s);
    path PathRelativeToTree(const path &p);
};

#endif // UpdateInstaler_H
