// -*- Mode: C++; tab-width: 4; -*-

#include "TStartup.h"
#include "TDeveloperPrefs.h"
#include "TLogger.h"
#include "TCommonPrimitives.h"
#include "lang/scheme/TSchemeInterpreter.h"

USING_NAMESPACE_FIVEL

void FIVEL_NS InitializeCommonCode()
{
    // Parse our developer preferences, if we have any.
    gDeveloperPrefs.ParsePrefs();

    // Open up our logfiles.
    bool want_log = (gDeveloperPrefs.GetPref(DEBUG_LOG) == DEBUG_LOG_ON);
    TLogger::OpenStandardLogs(want_log);
    
    // Register our portable interpreter primitives.
    RegisterCommonPrimitives();
}

TInterpreterManager *
GetSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc)
{
	// Create and return a new Scheme interpreter manager.
	return new TSchemeInterpreterManager(inIdleProc);
}

TInterpreterManager *FIVEL_NS
MaybeGetSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc)
{
	// If we can't find ./Scripts/start.ss, assume this is a legacy script.
	if (!FileSystem::GetScriptsDirectory().DoesExist() ||
		!FileSystem::GetScriptFilePath("start.ss").DoesExist())
		return NULL;

	// Create and return a new Scheme interpreter manager.
	return GetSchemeInterpreterManager(inIdleProc);
}
