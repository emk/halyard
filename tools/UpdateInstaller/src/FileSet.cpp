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

#include <stdio.h>
#include <string>
#include <boost/filesystem.hpp>
#include "FileSet.h"

using boost::filesystem::path;
using boost::filesystem::directory_iterator;

enum { BLOCK_SIZE = /*4096*/ 10 };

enum ParseState { DIGEST, SIZE, PATH };

bool FileSet::Entry::operator==(const Entry &other) const {
    return mDigest == other.mDigest && mSize == other.mSize
        && mPath == other.mPath;
}

size_t hash_value(const FileSet::Entry &e) { 
    size_t seed = 0;
    boost::hash_combine(seed, e.mDigest);
    boost::hash_combine(seed, e.mSize);
    boost::hash_combine(seed, e.mPath);
    return seed;
}

FileSet& FileSet::ParseAndAddEntries(const std::string &contents) {
    std::string digest_buf, size_buf, path_buf;
    ParseState state = DIGEST;
    std::string::const_iterator iter = contents.begin();
    for (; iter != contents.end(); ++iter) {
        switch (state) {
        case DIGEST:
            if (*iter == ' ')
                state = SIZE;
            else
                digest_buf += *iter;
            break;
                
        case SIZE:
            if (*iter == ' ')
                state = PATH;
            else 
                size_buf += *iter;
            break;

        case PATH:
            if (*iter == '\n') {
                Entry e(digest_buf, atoi(size_buf.c_str()), path_buf);
                AddEntry(e);

                digest_buf = size_buf = path_buf = "";
                state = DIGEST;
            } else {
                path_buf += *iter;
            }
        }
    }
    return *this;
}

FileSet FileSet::FromManifestFile(const boost::filesystem::path &path) {
    std::string data = read_file(path);

    return FileSet::FromContents(data);
}

FileSet FileSet::FromContents(const std::string &contents) {
    FileSet ret;
    return ret.ParseAndAddEntries(contents);
}

FileSet FileSet::FromManifestsInDir(const boost::filesystem::path &path) {
    FileSet ret;

    directory_iterator end_iter;
    for (directory_iterator iter(path); iter != end_iter; ++iter)
        if (!is_directory(iter->status()) && iter->path().stem() == "MANIFEST")
            ret.ParseAndAddEntries(read_file(iter->path()));

    return ret;
}

FileSet FileSet::MinusExactMatches(const FileSet &other) const
{
    FileSet diff;

    FileSet::EntrySet::const_iterator iter = Entries().begin();
    for(; iter != Entries().end(); ++iter)
        if (!other.HasMatchingEntry(*iter))
            diff.AddEntry(*iter);

    return diff;
}

bool FileSet::HasMatchingEntry(const FileSet::Entry &entry) const {
    return mEntries.find(entry) != mEntries.end();
}

void FileSet::AddEntry(const FileSet::Entry &entry) {
    mEntries.insert(entry);
    mDigests.insert(entry.digest());
    mDigestMap.insert(DigestMap::value_type(entry.digest(), entry));
}

std::string read_file(const boost::filesystem::path &path) {
    FILE *in = fopen(path.native_file_string().c_str(), "rb");
    if (in == NULL)
        throw std::exception("Cannot open manifest file");
    std::string buffer;
    while (!feof(in)) {
        size_t begin = buffer.size();
        buffer.resize(begin + BLOCK_SIZE);
        size_t read = fread(&buffer[begin], sizeof(char), BLOCK_SIZE, in);
        if (ferror(in))
            throw std::exception("Error reading manifest file");
        buffer.resize(begin + read);
    }
    fclose(in);
    if (ferror(in))
        throw std::exception("Error closing manifest file");
    return buffer;
}

