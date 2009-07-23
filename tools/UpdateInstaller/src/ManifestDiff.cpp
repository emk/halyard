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

#include "ManifestDiff.h"

ManifestDiff::ManifestDiff(const boost::filesystem::path &inBase, 
                           const boost::filesystem::path &inUpdate) 
    : mBaseDir(inBase), mUpdateDir(inUpdate) 
{
    Manifest base_manifest(Manifest::all_manifests_in_dir(mBaseDir));
    Manifest update_manifest(Manifest::all_manifests_in_dir(mUpdateDir));

    Manifest::EntryVector::const_iterator iter = 
        update_manifest.entries().begin();
    for(; iter != update_manifest.entries().end(); ++iter)
        if (!base_manifest.has_matching_entry(*iter))
            add_entry(*iter);
}
