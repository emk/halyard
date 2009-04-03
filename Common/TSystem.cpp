// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
#include "TSystem.h"
#include <string.h>

using namespace Halyard;

void Halyard::SetEnvVar(const std::string &inVar, const std::string &inValue) {
    // Use putenv to copy a value into our environment.  We'd like to use
    // setenv, which is much nicer, but it isn't available on Windows.
    // Unfortunately, putenv (on some platforms) takes ownership of the
    // string we pass it, and it may leak memory if another call is made to
    // putenv or setenv with the same variable in the future.  According to
    // the MacOS X man pages, we can't really do anything about this.
    //
    // Note that we need to use putenv on Windows, and not the
    // Windows-specific environment variable function, because the latter
    // doesn't actually make the new value readable using getenv.
    std::string assignment(inVar + "=" + inValue);
    char *buffer(static_cast<char*>(malloc(assignment.size()+1)));
    strncpy(buffer, assignment.c_str(), assignment.size()+1);
    putenv(buffer);
}
