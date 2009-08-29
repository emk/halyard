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

#include <string>
// We're using boost::unordered_map and boost::unordered_set, rather
// than the non-standard stdext::hash_set and hash_map, or the TR1
// versions, because the Boost classes are better documented.  These
// should be fairly close to the TR1 versions, but the documentation I
// could find for boost was better, and I don't believe that Visual
// Studio 2005 implements TR1, so we would just be using the boost
// libraries anyhow.
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
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
        friend size_t hash_value(const FileSet::Entry &e);
        
        std::string digest() const { return mDigest; }
        size_t size() const { return mSize; }
        std::string path() const { return mPath; }

    private:
        std::string mDigest, mPath;
        size_t mSize;
    };

    typedef boost::unordered_set<Entry> EntrySet;
    typedef boost::unordered_set<std::string> DigestSet;
    typedef boost::unordered_multimap<std::string, Entry> DigestMap;
    
    FileSet() {}
    
    static FileSet FromManifestFile(const boost::filesystem::path &path);
    static FileSet FromContents(const std::string &contents);
    static FileSet FromManifestsInDir(const boost::filesystem::path &path);

    void AddEntry(const Entry &entry);
    FileSet MinusExactMatches(const FileSet &other) const;

    const EntrySet &Entries() const { return mEntries; }
    bool HasMatchingEntry(const Entry &entry) const;
    const DigestMap &DigestEntryMap() const { return mDigestMap; }
    const DigestSet &Digests() const { return mDigests; }

private: 
    EntrySet mEntries;
    DigestSet mDigests;

    typedef boost::unordered_map<std::string, Entry> FileMap;
    FileMap mFileMap;

    DigestMap mDigestMap;

    FileSet& ParseAndAddEntries(const std::string &contents);
};

#endif // FileSet_H
