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

#include <fstream>
#include <stdio.h>

#include "TDeveloperPrefs.h"
#include "CrashReporter.h"

using namespace Halyard;

TDeveloperPrefs Halyard::gDeveloperPrefs;

// Create a prefs object with default values.
TDeveloperPrefs::TDeveloperPrefs()
{
	UseDefaultPrefs();
}

// Use defaults for user preferences
void TDeveloperPrefs::UseDefaultPrefs()
{
	for (int i=0; i<PREFS_SIZE; i++)
		userPrefs[i] = DEFAULT_PREF_VALUE;
}

// Parses user preferences file
void TDeveloperPrefs::ParsePrefs(const FileSystem::Path &inPrefsFile)
{
	std::ifstream prefsFile;			// user preferences
	bool		errFlag = false;	// error flag
	char		errString[256];		// used to print error message

	// Open the file
	bool could_open = false;
	std::string filename = inPrefsFile.ToNativePathString();
	if (inPrefsFile.DoesExist())
	{
		prefsFile.open(filename.c_str(), std::ios::in);
		if (!prefsFile.fail())
			could_open = true;
	}
	if (!could_open)
	{
		gDebugLog.Log("Could not open user preferences file \"%s\", using defaults.", filename.c_str());
		gVariableManager.Set("_debug", "0");
		UseDefaultPrefs();
		return;	
	}
	
    // We might as well include our preferences in crash dumps, too.
    CrashReporter::GetInstance()->AddDiagnosticFile(filename.c_str(),
                                                    "developer preferences");

	// _debug is set in the presence of developer.prefs
	gVariableManager.Set("_debug", "1");
	
	// Parse the file
	while (!prefsFile.eof()) 
	{
		std::string key, value;
		char	lineBuf[128];

		prefsFile.getline(lineBuf, 128, '\n');
		if (GetPrefsKeyValue(lineBuf, key, value))
		{
			 if (StringIComp(key, "db_type")) 
			 {
			     if (StringIComp(value, "encrypted"))
                     userPrefs[DB_TYPE] = DB_TYPE_ENCRYPTED;
				 else if (StringIComp(value, "clear"))
                     userPrefs[DB_TYPE] = DB_TYPE_CLEAR;
				 else
                     errFlag = true;	
			}
			else if (StringIComp(key, "db_writes")) 
			{
				if (StringIComp(value, "exit"))
					userPrefs[DB_WRITES] = DB_WRITES_EXIT;
				else if (StringIComp(value, "close"))
					userPrefs[DB_WRITES] = DB_WRITES_CLOSE;
				else if (StringIComp(value, "write"))
					userPrefs[DB_WRITES] = DB_WRITES_WRITE;			
			}
			else if (StringIComp(key, "mode")) 
			{
				if (StringIComp(value, "fullscreen"))
					userPrefs[MODE] = MODE_FULLSCREEN;
				else if (StringIComp(value, "window"))
					userPrefs[MODE] = MODE_WINDOW;
			}
			else if (StringIComp(key, "multiple_instances"))
			{
				if (StringIComp(value, "no"))
					userPrefs[MULTIPLE_INSTANCES] = MULTIPLE_INSTANCES_NO;
				else if (StringIComp(value, "yes"))
					userPrefs[MULTIPLE_INSTANCES] = MULTIPLE_INSTANCES_YES;
			}
            else if (StringIComp(key, "redoscript"))
			{
				if (StringIComp(value, "off"))
					userPrefs[REDOSCRIPT] = REDOSCRIPT_OFF;
				else if (StringIComp(value, "on"))
					userPrefs[REDOSCRIPT] = REDOSCRIPT_ON;
			}
			else if (StringIComp(key, "debug_log"))
			{
				if (StringIComp(value, "off"))
					userPrefs[DEBUG_LOG] = DEBUG_LOG_OFF;
				else if (StringIComp(value, "on"))
					userPrefs[DEBUG_LOG] = DEBUG_LOG_ON;
			}
			else if (StringIComp(key, "devtools"))
			{
				if (StringIComp(value, "disabled"))
					userPrefs[DEVTOOLS] = DEVTOOLS_DISABLED;
				else if (StringIComp(value, "enabled"))
					userPrefs[DEVTOOLS] = DEVTOOLS_ENABLED;
			}
			else
			{
				errFlag = true;
				sprintf(errString, "Unknown option in user preferences file \"%s\", reverting to defaults", key.c_str());
			}
		}
	}
	prefsFile.close();

	if (errFlag)
	{
		gLog.Error("%s", errString);
		UseDefaultPrefs();
		return;
	}
}

bool TDeveloperPrefs::GetPrefsKeyValue(char *line,
									   std::string &key,
									   std::string &value)
{	
	size_t pos1, pos2;

	std::string sLine = line;
	StringLTrim(sLine);
	
	key = "";
	value = "";

	// First check for comment chars
	if (StringStartsWith(sLine, "#") ||
		StringStartsWith(sLine, ";") ||
		StringStartsWith(sLine, "//"))
		return false;

	// Parse the key
	if ((pos1 = sLine.find(' ')) == std::string::npos)
		 return false;
	key = sLine.substr(0, pos1);

	// Parse the value
	pos2 = sLine.find('"', pos1);		  // set pos2 to 1st quote
	pos1 = sLine.find('"', pos2 + 1);	  // set pos1 to 2nd quote
	if (pos2 == std::string::npos || 
		pos1 == std::string::npos)
		return false;
	value = sLine.substr(pos2 + 1, pos1 - pos2 - 1);  

	// Make sure a key and value are not empty 
	if (key.empty() || value.empty())
		return false;
	
	return true;
}
