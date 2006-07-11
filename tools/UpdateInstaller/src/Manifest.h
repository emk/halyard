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

#ifndef Manifest_H
#define Manifest_H

class Manifest {
public:
    class Entry {
    public:
        Entry(const std::string &inDigest, size_t inSize,
              const std::string &inPath)
            : mDigest(inDigest), mSize(inSize), mPath(inPath) {}

        std::string digest() const { return mDigest; }
        size_t size() const { return mSize; }
        std::string path() const { return mPath; }

    private:
        std::string mDigest, mPath;
        size_t mSize;
    };

    typedef std::vector<Entry> EntryVector;

    Manifest(const std::string &path);
    EntryVector &entries() { return mEntries; }

private:
    EntryVector mEntries;

    std::string read_file(const std::string &path);
};

#endif Manifest_H
