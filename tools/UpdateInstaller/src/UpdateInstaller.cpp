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

#include <string>
#include <vector>
#include <cassert>
#include <windows.h>
#include <sys/utime.h>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/foreach.hpp>

#include "UpdateInstaller.h"
#include "FileSet.h"
#include "SpecFile.h"
#include "Interface.h"

using namespace boost::filesystem;

static const char LOCK_NAME[] = "UPDATE.LCK";
static bool IsWriteable(const path &name);
static void CopyFileWithRetries(const path &source, const path &dest, 
                                bool move);
static void TouchFile(const path &name);


//=========================================================================
//  UpdateInstaller setup
//=========================================================================

UpdateInstaller::UpdateInstaller(const path &src_root, const path &dst_root)
    : mSrcRoot(src_root), mDestRoot(dst_root), 
      mSpecFile(mSrcRoot / "Updates/release.spec"),
      mSrcManifestDir(mSrcRoot / "Updates/manifests" / mSpecFile.build()),
      mUpdateFiles(FileSet::FromManifestsInDir(mSrcManifestDir)),
      mExistingFiles(FileSet::FromManifestsInDir(mDestRoot)),
      mFilesNeededForNewTree(mUpdateFiles.MinusExactMatches(mExistingFiles)),
      mUpdateIsPossible(true)
{
}

void UpdateInstaller::PrepareForUpdate() {
    CalculateFileSetsForUpdates();
    BuildFileOperationVector();
}

void UpdateInstaller::CalculateFileSetsForUpdates() {
    FileSet::DigestSet::const_iterator digest_iter = 
        mFilesNeededForNewTree.Digests().begin();

    // For each of the digests that we need to put somewhere in the new tree...
    for (; digest_iter != mFilesNeededForNewTree.Digests().end(); ++digest_iter) {
        // If its already in the pool, we're all set, move on to the next file
        if (exists(PathInPool(*digest_iter)))
            continue;

        // Otherwise, there should be a file with this digest in the old tree
        FileSet::DigestMap::const_iterator entries_with_digest =
            mExistingFiles.DigestEntryMap().find(*digest_iter);
        if (entries_with_digest == mExistingFiles.DigestEntryMap().end()) {
            // Uh, oh! This doesn't appear to be in our tree or our pool.
            // Looks like something went wrong, and we can't update; flag
            // the update as impossible and return
            mUpdateIsPossible = false;
            return;
        }

        // Pick the first file with the given digest
        FileSet::Entry file(entries_with_digest->second);

        // If we still need this file at the same location with the
        // same contents
        if (mUpdateFiles.HasMatchingEntry(file)) {
            // Then add this to the list of files to copy into the pool
            mFilesToCopyFromTreeToPool.AddEntry(file);
        } else {
            // Otherwise, add it to the list of files to move to the pool
            mFilesToMoveFromTreeToPool.AddEntry(file);
        }
    }

    // The files left that we need to delete are the files that we have
    // in our current tree, minus the files we are going to move (since
    // by the time we do the deletes the moves will have happened), minus
    // the files that we do actually want exactly unchanged in our update.
    mFilesToDeleteFromTree = 
        mExistingFiles.MinusExactMatches(mFilesToMoveFromTreeToPool)
                      .MinusExactMatches(mUpdateFiles);
}

void UpdateInstaller::BuildFileOperationVector() {
    BuildTreeToPoolFileOperations();
    BuildPoolToTreeFileOperations();
    BuildUpdaterSpecialFileOperations();
    BuildDirectoryCleanupFileOperations();
}

void UpdateInstaller::BuildTreeToPoolFileOperations() {
    BOOST_FOREACH(FileSet::Entry file, mFilesToCopyFromTreeToPool.Entries()) {
        FileOperation::Ptr operation(new FileTransfer(PathInTree(file), 
                                                      PathInPool(file),
                                                      /* move? */ false));
        mOperations.push_back(operation);
    }
    BOOST_FOREACH(FileSet::Entry file, mFilesToMoveFromTreeToPool.Entries()) {
        FileOperation::Ptr operation(new FileTransfer(PathInTree(file),
                                                      PathInPool(file),
                                                      /* move? */ true));
        mOperations.push_back(operation);
    }
    BOOST_FOREACH(FileSet::Entry file, mFilesToDeleteFromTree.Entries()) {
        FileOperation::Ptr operation(new FileDelete(PathInTree(file)));
        mOperations.push_back(operation);
    }
}

void UpdateInstaller::BuildPoolToTreeFileOperations() {
    FileSet::DigestMap digest_tracker(mFilesNeededForNewTree.DigestEntryMap());
    BOOST_FOREACH(FileSet::Entry file, mFilesNeededForNewTree.Entries()) {
        FileSet::DigestMap::const_iterator 
            file_for_digest(digest_tracker.find(file.digest()));
        // If this is the last file with this digest, we should do a move
        if (digest_tracker.count(file.digest()) == 1) {
            assert(file_for_digest->second == file);
            digest_tracker.erase(file_for_digest);
            FileOperation::Ptr
                operation(new FileTransfer(PathInPool(file), PathInTree(file),
                                           /* move? */ true,
                                           FileShouldBeInPool(file)));
            mOperations.push_back(operation);
        } else {
            while (file_for_digest != digest_tracker.end() &&
                   !(file_for_digest->second == file)) {
                ++file_for_digest;
            }
            assert(file_for_digest != digest_tracker.end());
            digest_tracker.erase(file_for_digest);
            
            FileOperation::Ptr
                operation(new FileTransfer(PathInPool(file), PathInTree(file),
                                           /* move? */ false,
                                           FileShouldBeInPool(file)));
            mOperations.push_back(operation);
        }
    }
}

void UpdateInstaller::BuildUpdaterSpecialFileOperations() {
    // Add copy for all of the manifests we need to copy from
    // our update manifest dir to the root of the tree.
    // TODO - add test case for subset of manifests installed
    directory_iterator dir_iter(mDestRoot);
    for(; dir_iter != directory_iterator(); ++dir_iter) {
        if (dir_iter->leaf().substr(0, 9) == "MANIFEST.") {
            path src_path = mSrcManifestDir / dir_iter->leaf();
            FileOperation::Ptr operation(new FileTransfer(src_path, *dir_iter));
            mOperations.push_back(operation);
        }
    }

    // Add a copy for our updated release.spec
    FileOperation::Ptr 
        operation(new FileTransfer(mSrcRoot / "Updates/release.spec", 
                                   mDestRoot / "release.spec"));
    mOperations.push_back(operation);
}

void UpdateInstaller::BuildDirectoryCleanupFileOperations() {
    // Don't touch any files that we know about, in the existing files or
    // update files lists.  Those files will be dealt with by other portions
    // of the updater.
    FileSet::FilenameSet known_files(mExistingFiles.Filenames());
    known_files.insert(mUpdateFiles.Filenames().begin(), 
                       mUpdateFiles.Filenames().end());

    std::vector<path> dirs;
    dirs.push_back(mDestRoot / "scripts");
    // dirs.push_back(mDestRoot / "Scripts");
    dirs.push_back(mDestRoot / "collects");
    dirs.push_back(mDestRoot / "engine/win32/collects");
    dirs.push_back(mDestRoot / "engine/win32/plt");

    BOOST_FOREACH(path dir, dirs) {
        BuildCleanupRecursive(known_files, dir);
        if (!mUpdateIsPossible) return;
    }
}

void UpdateInstaller::BuildCleanupRecursive
    (const FileSet::FilenameSet &known_files, path dir) 
{
    if (!exists(dir)) return;

    directory_iterator dir_iter(dir);
    for (; dir_iter != directory_iterator(); ++dir_iter) {
        path full_path(dir_iter->path());

        path::iterator root_iter(mDestRoot.begin());
        path::iterator relative_iter(full_path.begin());

        while (++root_iter != mDestRoot.end())
            ++relative_iter;
        path relative_path;
        while (++relative_iter != full_path.end())
            relative_path /= *relative_iter;

        if (is_directory(dir_iter->status())) {
            BuildCleanupRecursive(known_files, full_path);
        } else if (known_files.count(relative_path.string()) == 0) {
            // This is not in our set of known files, so delete it if
            // it has one of our own file types (as it's assumed to be
            // junk left over from a previous update), or error
            // out if it is not (since in that case we assume that it's
            // some file that the user put there, and may be important
            // to them).
            std::string ext(relative_path.extension());
            if (ext == ".zo" || ext == ".ss" || ext == ".dep") {
                FileOperation::Ptr operation(new FileDelete(full_path));
                mOperations.push_back(operation);
            } else {
                mUpdateIsPossible = false;
                return;
            }
        }
    }
}

bool UpdateInstaller::FileShouldBeInPool(const FileSet::Entry &e) {
    bool found_in_copies = 
        mFilesToCopyFromTreeToPool.Digests().find(e.digest()) 
        != mFilesToCopyFromTreeToPool.Digests().end();
    bool found_in_moves = 
        mFilesToMoveFromTreeToPool.Digests().find(e.digest())
        != mFilesToMoveFromTreeToPool.Digests().end();
    return !found_in_copies && !found_in_moves;
}


//=========================================================================
//  UpdateInstaller sanity check
//=========================================================================

bool UpdateInstaller::IsUpdatePossible() {
    // Basic sanity check: Don't install to a target directory that lacks
    // a release.spec.  This reduces the chance the updater could
    // accidentally be used to mess up a user's system.
    if (!exists(mDestRoot / "release.spec"))
        return false;
    
    // If we ran across something weird earlier, bail.
    if (!mUpdateIsPossible) {
        return false;
    }

    // If one of our file operations appears to be impossible, because
    // the necessary files don't exist or are not readable or writable,
    // bail.
    FileOperation::Vector::const_iterator operation = mOperations.begin();
    for (; operation != mOperations.end(); ++operation) {
        if (!(*operation)->IsPossible()) {
            return false;
        }
    }

    // Otherwise, we're golden.
    return true;
}


//=========================================================================
//  UpdateInstaller installation
//=========================================================================

void UpdateInstaller::InstallUpdate() {
    LockDestinationDirectory();

    size_t total = mOperations.size();
    UpdateProgressRange(total);
    FileOperation::Vector::const_iterator operation = mOperations.begin();
    for (size_t i = 0; operation != mOperations.end(); ++operation, ++i) {
        UpdateProgress(i);
        (*operation)->Perform();
    }
    UpdateProgress(total);

    UnlockDestinationDirectory();
}

/// This function is called at uninstallation time to clean up any leftover
/// lock files.  Without this, a failed update would make it effectively
/// impossible for ordinary users to uninstall and reinstall the program.
void UpdateInstaller::DeleteLockFileForUninstall(const path &root) {
    path lock(root / LOCK_NAME);
    if (exists(lock))
        remove(lock);
}

/// Lock our destination directory.  This function actually contains a race
/// condition, because we don't make use of automatic create/open
/// operations.  But in practice, this is a sufficient level of robustness
/// for dealing with the failure modes we've observed so far.  For the
/// moment, we see this lock more as an advisory lock to improve the UI
/// than a correctness lock.
void UpdateInstaller::LockDestinationDirectory() {
    // If we have an existing lock, complain.
    path lock(mDestRoot / LOCK_NAME);
    if (exists(lock))
        throw std::exception("Destination directory locked by "
                             "previous update.");

    // Create the new lock file.  Race condition: If we go to sleep right
    // here, somebody else could create a lock file in between our check
    // above and the code below.
    FILE *f = fopen(lock.native_file_string().c_str(), "w");
    if (!f)
        throw std::exception("Can't create lock file.");
    fclose(f);
}

/// Unlock the destination directory.
void UpdateInstaller::UnlockDestinationDirectory() {
    path lock(mDestRoot / LOCK_NAME);
    if (!exists(lock))
        throw std::exception("Destination directory was unlocked "
                             "unexpectedly.");
    remove(lock);
}

/// The full path to where a file should be located in the tree.
path UpdateInstaller::PathInTree(const FileSet::Entry &e) { 
    return mDestRoot / e.path();
}

/// The full path to where a file is supposed to be located in the pool,
/// based on its digest.
path UpdateInstaller::PathInPool(const FileSet::Entry &e) {
    return PathInPool(e.digest());
}

/// The full path to a file located in the pool, based on the digest passed
/// in.
path UpdateInstaller::PathInPool(const std::string &s) {
    return mSrcRoot / "Updates/pool" / s;
}


//=========================================================================
//  FileOperation subclasses
//=========================================================================

bool FileDelete::IsPossible() const {
    return IsWriteable(file);
}

void FileDelete::Perform() const {
    if (exists(file))
        remove(file);
}

bool FileTransfer::IsPossible() const {
    // Bug #1107: We've been having trouble with mysterious locks on our
    // source files, on at least one machine.  In an effort to avoid this,
    // we're experimentally adding a "IsWriteable(source)" to this
    // condition, in hopes of detecting any such locks early enough to
    // abort.
    return (!mShouldExist || exists(mSource)) && 
        IsWriteable(mSource) && IsWriteable(mDest);
}

void FileTransfer::Perform() const {
    create_directories(mDest.branch_path());
    if (exists(mDest))
        remove(mDest);
    CopyFileWithRetries(mSource, mDest, mMove);
    TouchFile(mDest);
}


//=========================================================================
//  File system helper functions
//=========================================================================

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

/// Copy or move a file, retrying several times if the operation fails
void CopyFileWithRetries(const path &source, const path &dest, bool move) {
    // Bug #1107: We've been having trouble with mysterious locks on our
    // source files, on at least one machine.  In an effort to avoid this,
    // we try to copy files to their final destination repeatedly before
    // giving up.
    //
    // Since a failed copy_file will leave the program in an unusable and
    // corrupted state, we try several times before giving up, just in case
    // any failures are transient.  Note that we use a 1-based loop here
    // for clarity in the 'catch' statement.
    size_t max_retries = 5;
    bool succeeded = false;
    for (size_t i = 1; !succeeded && i <= max_retries; ++i) {
        try {
            if (move) {
                rename(source, dest);
            } else {
                copy_file(source, dest);
            }
            succeeded = true;
        } catch (filesystem_error &) {
            // If we've exhausted our last retry, then rethrow this error.
            // Otherwise, sleep and try again.
            if (i == max_retries)
                throw;
            else
                Sleep(500);
        }
    }
}

void TouchFile(const path &name) {
    utime(name.native_file_string().c_str(), NULL);
}
