// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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
#include "Manifest.h"

enum { BLOCK_SIZE = /*4096*/ 10 };

enum ParseState { DIGEST, SIZE, PATH };

Manifest::Manifest(const std::string &path) {
    std::string data = read_file(path);

    std::string digest_buf, size_buf, path_buf;
    ParseState state = DIGEST;
    std::string::iterator iter = data.begin();
    for (; iter != data.end(); ++iter) {
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

std::string Manifest::read_file(const std::string &path) {
    FILE *in = fopen(path.c_str(), "rb");
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
