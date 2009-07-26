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
#include <windows.h>
#include <sys/utime.h>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>

#include "UpdateInstaller.h"
#include "FileSet.h"
#include "SpecFile.h"
#include "Interface.h"

using namespace boost::filesystem;

static const char LOCK_NAME[] = "UPDATE.LCK";
static bool IsWriteable(const path &name);
static void CopyFileWithRetries(const path &source, const path &dest);
static void TouchFile(const path &name);

UpdateInstaller::UpdateInstaller(const path &src_root, const path &dst_root)
    : mSrcRoot(src_root), mDestRoot(dst_root), 
      mSpecFile(mSrcRoot / "Updates/release.spec"),
      mSrcManifestDir(mSrcRoot / "Updates/manifests" / mSpecFile.build())
{ 
    mUpdateFiles.InitFromManifestsInDir(mSrcManifestDir);
    mExistingFiles.InitFromManifestsInDir(mDestRoot);
}

void UpdateInstaller::PrepareForUpdate() {
    CalculatePoolToTreeCopiesNeeded();
    PopulatePoolFromTree();
}

void UpdateInstaller::CalculatePoolToTreeCopiesNeeded() {
    FileSet diff;
    diff.InitFilesToAdd(mExistingFiles, mUpdateFiles);

    // Add copy specs for all of files we need to copy from the pool to
    // the tree.
    FileSet::EntryVector::const_iterator iter = diff.Entries().begin();
    for (; iter != diff.Entries().end(); ++iter) {
        path src_path = mSrcRoot / "Updates/pool" / iter->digest();
        path dst_path = mDestRoot / iter->path();
        mCopies.push_back(CopySpec(src_path, dst_path));
    }

    // Add copy specs for all of the manifests we need to copy from
    // our update manifest dir to the root of the tree.
    // TODO - add test case for subset of manifests installed
    directory_iterator dir_iter(mDestRoot);
    for(; dir_iter != directory_iterator(); ++dir_iter) {
        if (dir_iter->leaf().substr(0, 9) == "MANIFEST.") {
            path src_path = mSrcManifestDir / dir_iter->leaf();
            mCopies.push_back(CopySpec(src_path, *dir_iter));
        }
    }

    // Add a copy spec for our updated release.spec
    mCopies.push_back(CopySpec(mSrcRoot / "Updates/release.spec", 
                               mDestRoot / "release.spec"));    
}

void UpdateInstaller::PopulatePoolFromTree() {
    CopyVector::const_iterator copy;
    for (copy = mCopies.begin(); copy != mCopies.end(); ++copy) {
        if (!exists(copy->source)) {
            FileSet::DigestMap map(mExistingFiles.DigestEntryMap());
            
            // XXX - a hopefully temporary hack, assumes that the file we
            // need is a pool file, not a manifest or release.spec.
            std::string digest_needed = copy->source.filename();

            // This is a multimap, since there can be more than one
            // file with the same digest, but we only need one of them.
            FileSet::DigestMap::const_iterator iter = map.find(digest_needed);
            if (iter != map.end()) {
                path file_from_tree = mSrcRoot / iter->second.path(); 
                CopyFileWithRetries(file_from_tree, copy->source);
            }
            
            // If we didn't find a file, we just punt, and let the 
            // IsUpdatePossible called later notice that we don't have
            // our source file and error out gracefully.
        }
    }
}

bool UpdateInstaller::IsUpdatePossible() {
    // Basic sanity check: Don't install to a target directory that lacks
    // a release.spec.  This reduces the chance the updater could
    // accidentally be used to mess up a user's system.
    if (!exists(mDestRoot / "release.spec"))
        return false;
    
    CopyVector::const_iterator copy = mCopies.begin();
    for (; copy != mCopies.end(); ++copy) {
        if (!copy->IsCopyPossible()) {
            return false;
        }
    }
    return true;
}

void UpdateInstaller::InstallUpdate() {
    LockDestinationDirectory();

    size_t total = mCopies.size();
    UpdateProgressRange(total);
    std::vector<CopySpec>::const_iterator copy = mCopies.begin();
    for (size_t i = 0; copy != mCopies.end(); ++copy, ++i) {
        UpdateProgress(i);
        create_directories(copy->dest.branch_path());
        copy->CopyOverwriting();
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

bool UpdateInstaller::CopySpec::IsCopyPossible() const {
    // Bug #1107: We've been having trouble with mysterious locks on our
    // source files, on at least one machine.  In an effort to avoid this,
    // we're experimentally adding a "IsWriteable(source)" to this
    // condition, in hopes of detecting any such locks early enough to
    // abort.
    return exists(source) & IsWriteable(source) && IsWriteable(dest);
}

void UpdateInstaller::CopySpec::CopyOverwriting() const {
    if (exists(dest))
        remove(dest);
    CopyFileWithRetries(source, dest);
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

/// Copy a file, retrying several times if the copy fails.
void CopyFileWithRetries(const path &source, const path &dest) {
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
            copy_file(source, dest);
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
