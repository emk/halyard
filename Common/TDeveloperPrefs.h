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

#if !defined (_TDeveloperPrefs_h_)
#define _TDeveloperPrefs_h_

#include "FileSystem.h"

BEGIN_NAMESPACE_FIVEL

// The name of our prefs file.
#define DEVELOPER_PREFS_FILE "5L.prefs"

// The names of our preferences.
enum PrefName {
	DB_TYPE = 0,
	DB_WRITES,
	MODE,
	MULTIPLE_INSTANCES,
	DEBUG_LOG,
	REDOSCRIPT,
    DEVTOOLS,

	// The total number of preferences we have (C++ will assign
	// a value equal to the index of the last option above plus one).
	PREFS_SIZE
};

// Options for the above user preferences (first option is default)
enum PrefValue {
	DEFAULT_PREF_VALUE = 0,

	DB_TYPE_ENCRYPTED = 0,
	DB_TYPE_CLEAR = 1,

	DB_WRITES_EXIT = 0,
	DB_WRITES_CLOSE = 1,
	DB_WRITES_WRITE = 2,

	MODE_FULLSCREEN = 0,
	MODE_WINDOW = 1,

	MULTIPLE_INSTANCES_NO = 0,
	MULTIPLE_INSTANCES_YES = 1,

	DEBUG_LOG_OFF = 0,
	DEBUG_LOG_ON = 1,

	REDOSCRIPT_OFF = 0,
	REDOSCRIPT_ON = 1,

    DEVTOOLS_DISABLED = 0,
    DEVTOOLS_ENABLED = 1
};

//////////
/// This class parses the 5L.prefs file and keeps track of various
/// developer preferences.
///
class TDeveloperPrefs
{
public:
	TDeveloperPrefs();

	//////////
	/// Use default user preferences.
	///
	void UseDefaultPrefs();
	
	//////////
	/// Parse the specified user preferences file.
	///
	/// \param inPrefsFile  The path to the preferences file.
	///
	void ParsePrefs(const FileSystem::Path &inPrefsFile);
	
	//////////
	/// Parse default user preferences file.
	///
	void ParsePrefs()
		{ ParsePrefs(FileSystem::Path(DEVELOPER_PREFS_FILE)); }

	//////////
	/// Get a user preference.
	/// 
	/// \param thePref  the preference to get
	/// \return  the value of this preference
	///
	PrefValue GetPref(PrefName thePref)
		{ return userPrefs[thePref]; }

private:
	//////////
	/// User preferences.
	///
	PrefValue userPrefs[PREFS_SIZE];
	
	//////////
	/// Parses a single line in the user preferences file.
	///
	/// \param line  a line from the input file
	/// \param key  (in/out) the key parsed from this line
	/// \param value  (in/out) the value parsed from this line
	/// \return  true a key and value were parsed, false otherwise
	///
	bool GetPrefsKeyValue(char *line, std::string &key, std::string &value); 
};

extern TDeveloperPrefs gDeveloperPrefs;

END_NAMESPACE_FIVEL

#endif // TDeveloperPrefs
