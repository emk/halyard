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

#include "AppHeaders.h"
#include "AppLog.h"
#include "CommonWxConv.h"

using namespace Halyard;

AppLog::AppLog()
    : mShouldSilentlyLogNonFatalErrors(false)

{
	gLog.Debug("halyard", "WX INITIALIZING: Sending wx log messages to Halyard logs.");
}

void AppLog::SilentlyLogNonFatalErrors()
{
    mShouldSilentlyLogNonFatalErrors = true;
}

void AppLog::DoLog(wxLogLevel inLevel, const wxChar *inMsg,
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
		wxChar buffer[256];
        wxStrftime(buffer, WXSIZEOF(buffer), wxT("%H:%M:%S"),
				   localtime(&inTimeStamp));
        std::string timestamp(ToStdString(buffer));
        std::string message(ToStdString(inMsg));

		// Print our log message to the appropriate logs.
		switch (inLevel) 
		{ 
			case wxLOG_FatalError:
				gLog.Fatal("halyard", "WX %s: %s [%s]", label, message.c_str(),
                                timestamp.c_str());
				break;

		    case wxLOG_Error:
                if (mShouldSilentlyLogNonFatalErrors) {
                    gLog.Debug("halyard", "WX %s: %s [%s]", label, message.c_str(),
                                  timestamp.c_str());
                    gLog.Info("halyard", "WX %s: %s [%s]", label, message.c_str(),
                             timestamp.c_str());
                } else {
                    gLog.Error("halyard", "WX %s: %s [%s]", label, message.c_str(),
                               timestamp.c_str());
                }
				break;

		    case wxLOG_Warning:
				gLog.Warn("halyard", "WX %s: %s [%s]", label, message.c_str(),
                             timestamp.c_str());
				break;
				
		    default:
				gLog.Debug("halyard", "WX %s: %s [%s]", label, message.c_str(),
                              timestamp.c_str());
				break;
		}
    }
}
