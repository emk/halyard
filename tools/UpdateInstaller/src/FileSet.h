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

#ifndef FileSet_H
#define FileSet_H

#include <vector>
#include <string>
#include <map>
#include <boost/filesystem/path.hpp>

std::string read_file(const boost::filesystem::path &path);

class FileSet {
public:
    class Entry {
    public:
        Entry(const std::string &inDigest, size_t inSize,
              const std::string &inPath)
            : mDigest(inDigest), mSize(inSize), mPath(inPath) {}

        bool operator==(const Entry &other) const;
        
        std::string digest() const { return mDigest; }
        size_t size() const { return mSize; }
        std::string path() const { return mPath; }

    private:
        std::string mDigest, mPath;
        size_t mSize;
    };

    typedef std::vector<Entry> EntryVector;
    
    FileSet() {}
    
    FileSet& InitFromManifestFile(const boost::filesystem::path &path);
    FileSet& InitFromContents(const std::string &contents);
    FileSet& InitFromManifestsInDir(const boost::filesystem::path &path);
    FileSet& InitFilesToAdd(const boost::filesystem::path &inBase,
                        const boost::filesystem::path &inUpdate);

    const EntryVector &entries() const { return mEntries; }
    bool has_matching_entry(const Entry &entry);

protected:
    void add_entry(const Entry &entry);

private: 
    EntryVector mEntries;

    typedef std::map<std::string, Entry> FileMap;
    FileMap mFileMap;
};

#endif // FileSet_H
