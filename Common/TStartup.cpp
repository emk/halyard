// -*- Mode: C++; tab-width: 4; -*-

#include "TStartup.h"
#include "TDeveloperPrefs.h"
#include "TLogger.h"

USING_NAMESPACE_FIVEL

void FIVEL_NS InitializeCommonCode()
{
    // Parse our developer preferences, if we have any.
    gDeveloperPrefs.ParsePrefs();

    // Open up our logfiles.
    bool want_log = (gDeveloperPrefs.GetPref(DEBUG_LOG) == DEBUG_LOG_ON);
    TLogger::OpenStandardLogs(want_log);
}
