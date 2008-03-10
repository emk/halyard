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

#include "AppHeaders.h"
#include "Log5L.h"

using namespace Halyard;

Log5L::Log5L()
    : mShouldSilentlyLogNonFatalErrors(false)

{
	gDebugLog.Log("WX INITIALIZING: Sending wx log messages to 5L logs.");
}

void Log5L::SilentlyLogNonFatalErrors()
{
    mShouldSilentlyLogNonFatalErrors = true;
}

void Log5L::DoLog(wxLogLevel inLevel, const wxChar *inMsg,
				  time_t inTimeStamp)
{
    const char *label = "MESSAGE";
    bool log = true;

    switch (inLevel)
    {
		case wxLOG_FatalError:
			label = "FATAL ERROR";
            break;
			
        case wxLOG_Error:
			label = "ERROR";
            break;
			
        case wxLOG_Warning:
			label = "WARNING";
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
		switch (inLevel) 
		{ 
			case wxLOG_FatalError:
				gDebugLog.FatalError("WX %s: %s [%s]", label, inMsg, buffer);
				gLog.FatalError("WX %s: %s [%s]", label, inMsg, buffer);
				break;

		    case wxLOG_Error:
                if (mShouldSilentlyLogNonFatalErrors) {
                    gDebugLog.Log("WX %s: %s [%s]", label, inMsg, buffer);
                    gLog.Log("WX %s: %s [%s]", label, inMsg, buffer);
                } else {
                    gDebugLog.Error("WX %s: %s [%s]", label, inMsg, buffer);
                    gLog.Error("WX %s: %s [%s]", label, inMsg, buffer);
                }
				break;

		    case wxLOG_Warning:
				gDebugLog.Caution("WX %s: %s [%s]", label, inMsg, buffer);
				gLog.Caution("WX %s: %s [%s]", label, inMsg, buffer);
				break;
				
		    default:
				gDebugLog.Log("WX %s: %s [%s]", label, inMsg, buffer);
				break;
		}
    }
}
