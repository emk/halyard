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
#include <map>
#include "SpecFile.h"

SpecFile::SpecFile(const boost::filesystem::path &path)
    : mContents(read_file(path)), mHeader(parseHeader()), 
      mUrl(mHeader["Update-URL"]), mBuild(mHeader["Build"]), 
      mManifest(FileSet::FromContents(mContents))
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
