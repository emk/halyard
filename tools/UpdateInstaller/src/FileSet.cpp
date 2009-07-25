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
#include <vector>
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

FileSet FileSet::ReadManifestFile(const boost::filesystem::path &path) {
    std::string data = read_file(path);
    FileSet manifest;
    manifest.init(data);

    return manifest;
}

FileSet FileSet::FromContents(const std::string &contents) {
    FileSet f;
    f.init(contents);

    return f;
}

FileSet FileSet::ReadManifestsInDir(const boost::filesystem::path &path) {
    FileSet m;
    
    directory_iterator end_iter;
    for (directory_iterator iter(path); iter != end_iter; ++iter)
        if (!is_directory(iter->status()) && iter->path().stem() == "MANIFEST")
            m.init(read_file(iter->path()));

    return m;
}

FileSet FileSet::FilesToAdd(const boost::filesystem::path &inBase, 
                            const boost::filesystem::path &inUpdate) 
{
    FileSet base_manifest(FileSet::ReadManifestsInDir(inBase));
    FileSet update_manifest(FileSet::ReadManifestsInDir(inUpdate));
    
    FileSet ret;

    FileSet::EntryVector::const_iterator iter = 
        update_manifest.entries().begin();
    for(; iter != update_manifest.entries().end(); ++iter)
        if (!base_manifest.has_matching_entry(*iter))
            ret.add_entry(*iter);

    return ret;
}


bool FileSet::has_matching_entry(const FileSet::Entry &entry) {
    FileMap::iterator iter(mFileMap.find(entry.path()));

    return iter != mFileMap.end() && iter->second == entry;
}

void FileSet::init(const std::string &contents) {
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
                add_entry(e);

                digest_buf = size_buf = path_buf = "";
                state = DIGEST;
            } else {
                path_buf += *iter;
            }
        }
    }
}

void FileSet::add_entry(const FileSet::Entry &entry) {
    mEntries.push_back(entry);
    mFileMap.insert(FileMap::value_type(entry.path(), entry));
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

