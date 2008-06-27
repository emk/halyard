// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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
#include "TStartup.h"
#include "CrashReporter.h"
#include "TDeveloperPrefs.h"
#include "TCommonPrimitives.h"
#include "lang/scheme/TSchemeInterpreter.h"
#include "ScriptEditorDB.h"

using namespace Halyard;

void Halyard::InitializeCommonCode(CrashReporter *inReporter)
{
    // Initialize our CrashReporter.
    CrashReporter::InitializeCrashReporting(inReporter);

    // Parse our developer preferences, if we have any.
    gDeveloperPrefs.ParsePrefs();

    // Open up our logfiles.
    bool want_log = (gDeveloperPrefs.GetPref(DEBUG_LOG) == DEBUG_LOG_ON);
    TLogger::OpenStandardLogs(want_log);
    
    // Register our portable interpreter primitives.
    RegisterCommonPrimitives();
    RegisterScriptEditorDBPrimitives();
}

TInterpreterManager *Halyard::
GetSchemeInterpreterManager(TInterpreterManager::SystemIdleProc inIdleProc)
{
	// Create and return a new Scheme interpreter manager.
	return new TSchemeInterpreterManager(inIdleProc);
}

TInterpreterManager *Halyard::
MaybeGetSchemeInterpreterManager(
    TInterpreterManager::SystemIdleProc inIdleProc)
{
	// If we can't find ./Scripts/start.ss, assume this is a legacy script.
	if (!FileSystem::GetScriptsDirectory().DoesExist() ||
		!FileSystem::GetScriptFilePath("start.ss").DoesExist())
		return NULL;

	// Create and return a new Scheme interpreter manager.
	return GetSchemeInterpreterManager(inIdleProc);
}
