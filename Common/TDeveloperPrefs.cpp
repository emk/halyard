// -*- Mode: C++; tab-width: 4; -*-

#include "CommonHeaders.h"

#include <fstream>
#include <stdio.h>

#include "TDeveloperPrefs.h"

USING_NAMESPACE_FIVEL

TDeveloperPrefs FIVEL_NS gDeveloperPrefs;

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
	
	// _debug is set in the presence of 5L.prefs
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
	int pos1, pos2;

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
