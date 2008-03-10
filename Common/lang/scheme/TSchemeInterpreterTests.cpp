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

#include "CommonHeaders.h"

#include <limits.h>

#include "ImlUnit.h"
#include "TSchemeInterpreter.h"
#include "TSchemeScriptEditorDB.h"

// XXX - Hack to make REGISTER_5L_PRIMITIVE work correctly.  It needs to be
// called from a function in the FiveL:: namespace, which is silly.
BEGIN_NAMESPACE_FIVEL
extern void RegisterSchemeTestPrimitives();
END_NAMESPACE_FIVEL

USING_NAMESPACE_FIVEL

// Make sure we have single-token names for all our types.
using std::string;
using GraphicsTools::Color;

extern void test_TSchemeInterpreter (void);

static bool gTestingPause = false;
static int gPauseCount = 0;

// The idle function called periodically by our Scheme interpreter.
static void TestIdleFunc(bool inBlock)
{
	if (gTestingPause && --gPauseCount <= 0)
	{
		gTestingPause = false;
		gPauseCount = 0;

		TEST(TInterpreter::GetInstance()->Paused());
		TInterpreter::GetInstance()->WakeUp();
		TEST(!TInterpreter::GetInstance()->Paused());
	}
}

DEFINE_5L_PRIMITIVE(TestStop)
{
	std::string next_card;
	inArgs >> SymbolName(next_card);

	TEST(!TInterpreter::GetInstance()->IsStopped());
	TInterpreter::GetInstance()->Stop();
	TEST(TInterpreter::GetInstance()->IsStopped());
	TInterpreter::GetInstance()->Go(next_card.c_str());
	TEST(!TInterpreter::GetInstance()->IsStopped());
}

DEFINE_5L_PRIMITIVE(TestPause)
{
	gTestingPause = true;
	gPauseCount = 10;

	TEST(!TInterpreter::GetInstance()->Paused());
	TInterpreter::GetInstance()->Pause();
	TEST(TInterpreter::GetInstance()->Paused());
}

DEFINE_5L_PRIMITIVE(TestCallback)
{
	TCallbackPtr callback;
	inArgs >> callback;
	callback->Run();
}

DEFINE_5L_PRIMITIVE(TestCallbackArgs)
{
	TCallbackPtr callback;
	inArgs >> callback;

	TValueList args, nested;
	args.push_back("hello");
	args.push_back(TSymbol("world"));
	nested.push_back("foo");
	nested.push_back(TSymbol("bar"));
	args.push_back(nested);
	callback->Run(args);
}

DEFINE_5L_PRIMITIVE(TestScriptEditorDB)
{
    // Get our script editor database.
    TInterpreterManager *manager = TInterpreterManager::GetInstance();
    ScriptEditorDB *db = manager->GetScriptEditorDB();
    TEST(db != NULL);

    // Scan our script directories and process all files.
    db->UpdateDatabase();
    
    // TODO - Test for the presence of some known definitions?
}

#define DEFINE_TYPE_TEST_PRIMITIVES(TYPE, COUNT) \
	static TYPE TYPE##_test_values[COUNT]; \
    static uint32 TYPE##_index = 0; \
	DEFINE_5L_PRIMITIVE(Set_Wanted_##TYPE) { \
		inArgs >> TYPE##_index; \
		ASSERT(TYPE##_index < COUNT); \
	} \
	DEFINE_5L_PRIMITIVE(Test_Check_##TYPE) { \
		TYPE arg; \
		inArgs >> arg; \
		TEST(TYPE##_test_values[TYPE##_index] == arg); \
	}

#define REGISTER_TYPE_TEST_PRIMITIVES(TYPE) \
    do { \
		REGISTER_5L_PRIMITIVE(Set_Wanted_##TYPE); \
		REGISTER_5L_PRIMITIVE(Test_Check_##TYPE); \
	} while (0)

DEFINE_TYPE_TEST_PRIMITIVES(string, 2)
DEFINE_TYPE_TEST_PRIMITIVES(int32, 3)
DEFINE_TYPE_TEST_PRIMITIVES(uint32, 3)
DEFINE_TYPE_TEST_PRIMITIVES(bool, 2)
DEFINE_TYPE_TEST_PRIMITIVES(double, 3)
DEFINE_TYPE_TEST_PRIMITIVES(TPoint, 1)
DEFINE_TYPE_TEST_PRIMITIVES(TRect, 1)
DEFINE_TYPE_TEST_PRIMITIVES(TPolygon, 2)
DEFINE_TYPE_TEST_PRIMITIVES(Color, 1)

void FIVEL_NS RegisterSchemeTestPrimitives()
{
	REGISTER_5L_PRIMITIVE(TestStop);
	REGISTER_5L_PRIMITIVE(TestPause);
	REGISTER_5L_PRIMITIVE(TestCallback);
	REGISTER_5L_PRIMITIVE(TestCallbackArgs);
    REGISTER_5L_PRIMITIVE(TestScriptEditorDB);

	string_test_values[0] = "";
	string_test_values[1] = "hello";
	REGISTER_TYPE_TEST_PRIMITIVES(string);

	int32_test_values[0] = INT_MIN;
	int32_test_values[1] = 0;
	int32_test_values[2] = INT_MAX;
	REGISTER_TYPE_TEST_PRIMITIVES(int32);

	uint32_test_values[0] = 0;
	uint32_test_values[1] = 1;
	uint32_test_values[2] = UINT_MAX;
	REGISTER_TYPE_TEST_PRIMITIVES(uint32);

	bool_test_values[0] = true;
	bool_test_values[1] = false;
	REGISTER_TYPE_TEST_PRIMITIVES(bool);

	double_test_values[0] = -1.0;
	double_test_values[1] = 0.0;
	double_test_values[2] = 1.0;
	REGISTER_TYPE_TEST_PRIMITIVES(double);

	TPoint_test_values[0] = TPoint(1, 2);
	REGISTER_TYPE_TEST_PRIMITIVES(TPoint);

	TRect_test_values[0] = TRect(1, 2, 3, 4);
	REGISTER_TYPE_TEST_PRIMITIVES(TRect);

	std::vector<TPoint> pts;
	pts.push_back(TPoint(0, 0));
	pts.push_back(TPoint(2, 0));
	pts.push_back(TPoint(1, 2));
	TPolygon_test_values[0] = TPolygon();
	TPolygon_test_values[1] = TPolygon(pts);
	REGISTER_TYPE_TEST_PRIMITIVES(TPolygon);

	Color_test_values[0] = Color(1, 2, 3, 4);
	REGISTER_TYPE_TEST_PRIMITIVES(Color);
}

void test_TSchemeInterpreter (void)
{
	RegisterSchemeTestPrimitives();
	
	// Boot the interpreter.
	TSchemeInterpreterManager scheme(&TestIdleFunc);
	scheme.BeginScript();
	scheme.Run();

	// Make sure we visited all the right cards.
	TEST(gVariableManager.Get("seen-start") == std::string("1"));
	TEST(gVariableManager.Get("seen-test-1") == std::string("1"));
	TEST(gVariableManager.Get("seen-test-2") == std::string("1"));
}
