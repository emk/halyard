// -*- Mode: C++; tab-width: 4; -*-

#include "TStartup.h"
#include "TDeveloperPrefs.h"
#include "TLogger.h"
#include "TCommonPrimitives.h"
#include "T5LPrimitives.h"

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

	// Register our 5L-only portable interpreter primitives.
	Register5LPrimitives();
}
