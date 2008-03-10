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

#ifndef TVariableManager_H
#define TVariableManager_H

#include <typeinfo>

BEGIN_NAMESPACE_HALYARD

// Forward declarations.
class TValue;


//=========================================================================
// TVariableManager
//=========================================================================

/// Centralized manager for special variables shared between the Halyard
/// engine and a script.  The variables store TValue objects.
class TVariableManager {
	typedef std::map<std::string, TValue> TValueMap;

	TValueMap mMap;

public:
	TVariableManager() {}
	virtual ~TVariableManager() {}

	void Set(const std::string &inName, const TValue &inVal);
	TValue Get(const std::string &inName);
    bool VariableExists(const std::string &inName);

	TValue::Type GetType(const char *inName);
	void MakeNull(const char *inName);
	bool IsNull(const char *inName);	

};

extern TVariableManager gVariableManager;

END_NAMESPACE_HALYARD

#endif // TVariableManager_H
