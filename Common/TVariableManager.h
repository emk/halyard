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

	const char *GetString(const char *inName);
	const char *GetSymbol(const char *inName);
	int32 GetLong(const char *inName);
	uint32 GetULong(const char *inName);
	double GetDouble(const char *inName);
	bool GetBoolean(const char *inName);
	TPoint GetPoint(const char *inName);
	TRect GetRect(const char *inName);
	GraphicsTools::Color GetColor(const char *inName);
	void SetString(const char *inName, const char *inValue);
	void SetSymbol(const char *inName, const char *inValue);
	void SetLong(const char *inName, const int32 inValue);
	void SetULong(const char *inName, const uint32 inValue);
	void SetDouble(const char *inName, const double inValue);
	void SetBoolean(const char *inName, const bool inValue);
	void SetPoint(const char *inName, const TPoint &inValue);
	void SetRect(const char *inName, const TRect &inValue);
	void SetColor(const char *inName,
				  const GraphicsTools::Color &inValue);
};

extern TVariableManager gVariableManager;

#endif // TVariableManager_H
