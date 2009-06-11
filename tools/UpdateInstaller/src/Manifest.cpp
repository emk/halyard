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

#define BOOST_FILESYSTEM_SOURCE

#include <stdio.h>
#include <vector>
#include <string>
#include "boost/filesystem/path.hpp"
#include "Manifest.h"

enum { BLOCK_SIZE = /*4096*/ 10 };

enum ParseState { DIGEST, SIZE, PATH };

Manifest::Manifest(const boost::filesystem::path &path) {
    std::string data = read_file(path);

    init(data);
}

Manifest::Manifest(const std::string &contents) {
    init(contents);
}

void Manifest::init(const std::string &contents) {
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
                mEntries.push_back(Entry(digest_buf, 
                                         atoi(size_buf.c_str()),
                                         path_buf));
                digest_buf = size_buf = path_buf = "";
                state = DIGEST;
            } else {
                path_buf += *iter;
            }
        }
    }
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

SpecFile::SpecFile(const boost::filesystem::path &path) 
    : mContents(read_file(path)), mHeader(parseHeader()), 
      mUrl(mHeader["Update-URL"]), mBuild(mHeader["Build"]), 
      mManifest(mContents) 
{ }

enum HeaderParseState { NEWLINE, KEY, VALUE };

SpecFile::StringMap SpecFile::parseHeader() {
    HeaderParseState state = NEWLINE;
    std::string key_buf, val_buf;
    StringMap ret;

    std::string::iterator iter = mContents.begin();
    for (; iter != mContents.end(); ++iter) {
        switch(state) {
        case NEWLINE: 
            if (*iter == '\n') {
                // We have a blank line, so we're done with the header
                mContents = std::string(++iter, mContents.end());
                return ret;
            } else { 
                key_buf += *iter;
                state = KEY;
            }
            break;
        case KEY:
            if (*iter == ':') {
                // check if we have ": "
                if (++iter == mContents.end()) {
                    mContents = "";
                    return ret;
                } else if (*iter = ' ') {
                    state = VALUE;
                } else {
                    key_buf += ':';
                    key_buf += *iter;
                }
            } else {
                key_buf += *iter;
            }
            break;
        case VALUE:
            if (*iter == '\n') {
                ret[key_buf] = val_buf;
                key_buf = val_buf = "";
                state = NEWLINE;
            } else {
                val_buf += *iter;
            }
        }
    }

    mContents = "";
    return ret;
}
