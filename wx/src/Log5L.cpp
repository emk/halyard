// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TLogger.h"
#include "Log5L.h"

Log5L::Log5L()
{
	gDebugLog.Log("WX INITIALIZING: Sending wx log messages to 5L logs.");
}
	
void Log5L::DoLog(wxLogLevel inLevel, const wxChar *inMsg,
				  time_t inTimeStamp)
{
    const char *label = "MESSAGE";
    bool log = true;
    bool high_priority = false;

    switch (inLevel)
    {
		case wxLOG_FatalError:
			label = "FATAL ERROR";
			high_priority = true;
            break;
			
        case wxLOG_Error:
			label = "ERROR";
			high_priority = true;
            break;
			
        case wxLOG_Warning:
			label = "WARNING";
			high_priority = true;
            break;
			
        case wxLOG_Info:
			label = "INFO";
			break;

        case wxLOG_Message:
			label = "MESSAGE";
			break;
			
        case wxLOG_Status:
			log = false;
			break;
			
        case wxLOG_Trace:
			label = "TRACE";
			break;
			
        case wxLOG_Debug:
			label = "DEBUG";
			break;
    }
	
    if (log)
    {
		// Format our timestamp.
		char buffer[256];
        wxStrftime(buffer, WXSIZEOF(buffer), "%H:%M:%S",
				   localtime(&inTimeStamp));

		// Print our log message to the appropriate logs.
		gDebugLog.Log("WX %s: %s [%s]", label, inMsg, buffer);
		if (high_priority)
			gLog.Log("WX %s: %s [%s]", label, inMsg, buffer);
    }
}
