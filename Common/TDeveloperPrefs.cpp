// -*- Mode: C++; tab-width: 4; -*-

#include <iostream.h>
#include <fstream.h>

#include "TDeveloperPrefs.h"
#include "TLogger.h"
#include "TVariable.h"

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
	ifstream	prefsFile;			// user preferences
	bool		errFlag = false;	// error flag
	char		errString[256];		// used to print error message

	// Open the file
	std::string filename = inPrefsFile.ToNativePathString();
	prefsFile.open(filename.c_str(), ios::in | ios::nocreate);
	if (prefsFile.fail())
	{
		gDebugLog.Log("Could not open user preferences file \"%s\", using defaults.", filename.c_str());
		gVariableManager.SetString("_debug", "0");
		UseDefaultPrefs();
		return;	
	}
	
	// _debug is set in the presence of 5L.prefs
	gVariableManager.SetString("_debug", "1");
	
	// Parse the file
	while (!prefsFile.eof()) 
	{
		TString key, 
				value;
		char	lineBuf[128];

		prefsFile.getline(lineBuf, 128, '\n');
		if (GetPrefsKeyValue(lineBuf, key, value))
		{
			if (key.Equal("db_type", false)) 
			{
				if (value.Equal("encrypted", false))
					userPrefs[DB_TYPE] = DB_TYPE_ENCRYPTED;
				else if (value.Equal("clear", false))
					userPrefs[DB_TYPE] = DB_TYPE_CLEAR;
				else
					errFlag = true;	
			}
			else if (key.Equal("db_writes", false)) 
			{
				if (value.Equal("exit", false))
					userPrefs[DB_WRITES] = DB_WRITES_EXIT;
				else if (value.Equal("close", false))
					userPrefs[DB_WRITES] = DB_WRITES_CLOSE;
				else if (value.Equal("write", false))
					userPrefs[DB_WRITES] = DB_WRITES_WRITE;			
			}
			else if (key.Equal("mode", false)) 
			{
				if (value.Equal("fullscreen", false))
					userPrefs[MODE] = MODE_FULLSCREEN;
				else if (value.Equal("window", false))
					userPrefs[MODE] = MODE_WINDOW;
			}
			else if (key.Equal("multiple_instances", false))
			{
				if (value.Equal("no"))
					userPrefs[MULTIPLE_INSTANCES] = MULTIPLE_INSTANCES_NO;
				else if (value.Equal("yes"))
					userPrefs[MULTIPLE_INSTANCES] = MULTIPLE_INSTANCES_YES;
			}
			else if (key.Equal("redoscript", false))
			{
				if (value.Equal("off", false))
					userPrefs[REDOSCRIPT] = REDOSCRIPT_OFF;
				else if (value.Equal("on", false))
					userPrefs[REDOSCRIPT] = REDOSCRIPT_ON;
			}
			else if (key.Equal("debug_log", false))
			{
				if (value.Equal("off", false))
					userPrefs[DEBUG_LOG] = DEBUG_LOG_OFF;
				else if (value.Equal("on", false))
					userPrefs[DEBUG_LOG] = DEBUG_LOG_ON;
			}
			else
			{
				errFlag = true;
				sprintf(errString, "Unknown option in user preferences file \"%s\", reverting to defaults", key.GetString());
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
									   TString &key,
									   TString &value)
{	
	int pos1, pos2;

	TString sLine = line;
	sLine.LTrim();

	key = "";
	value = "";

	// First check for comment chars
	if (sLine.StartsWith("#") ||
		sLine.StartsWith(";") ||
		sLine.StartsWith("//"))
		return false;

	// Parse the key
	if ((pos1 = sLine.Find(' ')) < 0)
		return false;
	key = sLine.Mid(0, pos1);

	// Parse the value
	pos2 = sLine.Find('"', pos1);					// set pos2 to 1st quote
	pos1 = sLine.Find('"', pos2 + 1);				// set pos1 to 2nd quote
	if (pos2 < 0 || pos1 < 0)
		return false;
	value = sLine.Mid(pos2 + 1, pos1 - pos2 - 1);  

	// Make sure a key and value are not empty 
	if (key.IsEmpty() || value.IsEmpty())
		return false;
	
	return true;
}
