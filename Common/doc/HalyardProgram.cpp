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

#include "CommonHeaders.h"

#include <boost/lexical_cast.hpp>

#include <ctime>
#include "HalyardProgram.h"

using namespace Halyard;
using namespace model;

IMPLEMENT_MODEL_CLASS(HalyardProgram);

void HalyardProgram::Initialize() {
    time_t t = time(0);
    std::string year =
        boost::lexical_cast<std::string>(localtime(&t)->tm_year + 1900);

    SetString("name", "");
    //SetSize("stageSize", ...);
    SetString("copyright",
              "Copyright " + year + " ???.  All rights reserved.");
    SetString("version", "0.0");
    SetString("comments", "");

    Set("cards", new List());
    Set("backgrounds", new List());

    // Add any new fields in the migration code, not in Initialize(), so we
    // can just go ahead and migrate objects to the latest version.
    Migrate();
}

void HalyardProgram::Migrate() {
    // Please see the format version information in Document.cpp and the
    // class ModelFormat for more information on how this works.

    // Migrate from format 0 to format 1.
    if (!DoFind("dbgreporturl"))
        SetString("dbgreporturl", "");

    // Migrate from format 1 to format 2.
    if (!DoFind("sourcefilecount"))
        SetInteger("sourcefilecount", 1);

    // Migrate from format 2 to format 3.
    if (!DoFind("datadirname"))
        SetString("datadirname", "");

    // Migrate from fromat 3 to format 4.
    // We don't have any way to remove a key, so instead we just set it
    // to 0.  This should be backwards compatible with old versions, since
    // it will just cause them to have an incorrect source file count on
    // the first load.
    SetInteger("sourcefilecount", 0);
}

/// This script name is displayed on the splash screen.
std::string HalyardProgram::GetName() {
    return GetString("name");
}

/// This copyright notice is displayed on the splash screen.
std::string HalyardProgram::GetCopyright() {
    return GetString("copyright");
}

/// This script name is displayed on the splash screen.
std::string HalyardProgram::GetDataDirectoryName() {
    std::string datadirname(GetString("datadirname"));
    if (!datadirname.empty())
        return datadirname;
    else
        return GetString("name");
}

/// The URL to which we should submit debug reports about script errors.
std::string HalyardProgram::GetDebugReportURL() {
    return GetString("dbgreporturl");
}
