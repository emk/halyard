// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TVariableManager_H
#define TVariableManager_H

#include <typeinfo>

BEGIN_NAMESPACE_FIVEL

// Forward declarations.
class TValue;


//=========================================================================
// TVariableManager
//=========================================================================
// A wrapper class for an STL map that stores variable names and their 
// corresponding variables as TValues. 

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

#endif // TVariableManager_H
