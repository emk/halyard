// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TestCase.h"
#include "TInterpreter.h"
#include "TVariableManager.h"

USING_NAMESPACE_FIVEL

TVariableManager FIVEL_NS gVariableManager;

REGISTER_TEST_CASE_FILE(TVariableManager);


//=========================================================================
//  TVariableManager Methods
//=========================================================================

TValue TVariableManager::Get(const std::string &inName) {
	TValueMap::iterator iter = mMap.find(MakeStringLowercase(inName));
	if (iter != mMap.end()) {
		return iter->second; 
    } else {
        std::string err = "Tried to read uninitialized variable: " + inName;
		THROW(err.c_str());
    }
}

void TVariableManager::Set(const std::string &inName, const TValue &inValue) {
	std::pair<std::string, TValue> p(MakeStringLowercase(inName), inValue); 
	
	std::pair<TValueMap::iterator, bool> result;
	result = mMap.insert(p);

    // If variable already exists, replace it with inValue
    // and notify caller that a replacement has been made.
	if (!result.second) {
		mMap.erase(result.first);
		mMap.insert(p);
	}
}

bool TVariableManager::VariableExists(const std::string &inName) {
	TValueMap::iterator iter = mMap.find(MakeStringLowercase(inName));
	return iter != mMap.end();
}
		
TValue::Type TVariableManager::GetType(const char *inName) {
	return Get(inName).GetType();
}

void TVariableManager::MakeNull(const char *inName) {
	Set(inName, TValue(TNull()));
}

bool TVariableManager::IsNull(const char *inName) {
    return (Get(inName).GetType() == TValue::TYPE_NULL);
}

const char *TVariableManager::GetString(const char *inName) {
	return std::string(Get(inName)).c_str();
}

const char *TVariableManager::GetSymbol(const char *inName) {
	return TSymbol(Get(inName)).GetName().c_str();
}

int32 TVariableManager::GetLong(const char *inName) {
	return Get(inName);
}

uint32 TVariableManager::GetULong(const char *inName) {
	return Get(inName);
}

double TVariableManager::GetDouble(const char *inName) {
	return Get(inName);
}

bool TVariableManager::GetBoolean(const char *inName) {
	return Get(inName);
}

TPoint TVariableManager::GetPoint(const char *inName) {
	return Get(inName);
}

TRect TVariableManager::GetRect(const char *inName) {
	return Get(inName);
}

GraphicsTools::Color TVariableManager::GetColor(const char *inName) {
	return Get(inName);
}

void TVariableManager::SetString(const char *inName, const char *inValue) {
	Set(inName, std::string(inValue));
}

void TVariableManager::SetSymbol(const char *inName, const char *inValue) {
	Set(inName, TSymbol(inValue));
}

void TVariableManager::SetLong(const char *inName, const int32 inValue) {
	Set(inName, inValue);
}

void TVariableManager::SetULong(const char *inName, const uint32 inValue) {
	Set(inName, inValue);
}

void TVariableManager::SetDouble(const char *inName, const double inValue) {
	Set(inName, inValue);
}

void TVariableManager::SetBoolean(const char *inName, const bool inValue) {
	Set(inName, inValue);
}


void TVariableManager::SetPoint(const char *inName, const TPoint &inValue) {
	Set(inName, inValue);
}

void TVariableManager::SetRect(const char *inName, const TRect &inValue) {
	Set(inName, inValue);
}

void TVariableManager::SetColor(const char *inName, 
								const GraphicsTools::Color &inValue) 
{
	Set(inName, inValue);
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

BEGIN_TEST_CASE(TestTVariableManager, TestCase) {
	TVariableManager manager;

	manager.SetString("foo", "bar");
	CHECK_EQ(std::string(manager.GetString("foo")), 
			 std::string("bar"));
	manager.SetString("foo", "bar");
	CHECK_EQ(manager.GetType("foo"), TValue::TYPE_STRING); 

	// check extracting a non existent variable
	//CHECK_THROWN(std::exception,
	//			   std::string(manager.GetString("bar")));

	manager.SetString("CASETEST", "foo");
	CHECK_EQ(strcmp(manager.GetString("casetest"), "foo"), 0);

	manager.SetSymbol("hello", "world");
	CHECK_EQ(std::string(manager.GetSymbol("hello")),
			 std::string("world"));
	CHECK_EQ(manager.GetType("hello"), TValue::TYPE_SYMBOL); 

	manager.SetLong("longVar", MAX_INT32);
	CHECK_EQ(manager.GetLong("longVar"), MAX_INT32);
	CHECK_EQ(manager.GetType("longVar"), TValue::TYPE_LONG); 

	manager.SetULong("ulongVar", MAX_UINT32);
	CHECK_EQ(manager.GetULong("ulongVar"), MAX_UINT32);
	CHECK_EQ(manager.GetType("ulongVar"), TValue::TYPE_ULONG); 

	manager.SetDouble("doubleVar", 10.0);
	CHECK_EQ(manager.GetDouble("doubleVar"), 10.0);
	CHECK_EQ(manager.GetType("doubleVar"), TValue::TYPE_DOUBLE);

	manager.SetBoolean("boolVar", true);
	CHECK_EQ(manager.GetBoolean("boolVar"), true);
	manager.SetBoolean("boolVar", false);
	CHECK_EQ(manager.GetBoolean("boolVar"), false);
	CHECK_EQ(manager.GetType("boolVar"), TValue::TYPE_BOOLEAN);

	manager.SetPoint("pointVar", TPoint(0, 0));
	CHECK_EQ(manager.GetPoint("pointVar"), TPoint(0, 0));
	CHECK_EQ(manager.GetType("pointVar"), TValue::TYPE_POINT);

	manager.SetRect("rectVar", TRect(0, 1, 2, 3));
	CHECK_EQ(manager.GetRect("rectVar"),TRect(0, 1, 2, 3));
	CHECK_EQ(manager.GetType("rectVar"), TValue::TYPE_RECT);

	manager.SetColor("colorVar", 
					 GraphicsTools::Color(0, 1, 2, 3));
	CHECK_EQ(manager.GetColor("colorVar"), 
			 GraphicsTools::Color(0, 1, 2, 3));
	CHECK_EQ(manager.GetType("colorVar"), TValue::TYPE_COLOR);
	
    // check resetting TValues
	CHECK_EQ(manager.IsNull("longVar"), false);
	manager.MakeNull("longVar");
	CHECK_EQ(manager.IsNull("longVar"), true);

} END_TEST_CASE(TestTVariableManager);


#endif // BUILD_TEST_CASES
