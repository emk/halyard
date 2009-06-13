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

#ifndef TStartup_H
#define TStartup_H

#include "TInterpreter.h"

BEGIN_NAMESPACE_HALYARD

class CrashReporter;

/// Initialize the various modules of the Common/ library.
/// If you want to call FileSystem::SetBaseDirectory, do it
/// before calling this function.
extern void InitializeCommonCode(CrashReporter *inReporter);

/// Create a Scheme interpreter manager.
extern TInterpreterManager *
GetSchemeInterpreterManager(TInterpreterManager::SystemIdleProc inIdleProc);

/// If the current program looks like a Scheme script, attempt
/// to start a Scheme interpreter.  Otherwise, return NULL.  This function
/// can be used to support two languages in a single engine.
extern TInterpreterManager *
MaybeGetSchemeInterpreterManager(
    TInterpreterManager::SystemIdleProc inIdleProc);

END_NAMESPACE_HALYARD

#endif // TStartup_H
