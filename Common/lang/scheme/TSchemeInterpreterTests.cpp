// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <limits.h>
#include <boost/scoped_ptr.hpp>

#include "ImlUnit.h"
#include "TSchemeInterpreter.h"

// Make sure we have single-token names for all our types.
using std::string;
using GraphicsTools::Color;

extern void test_TSchemeInterpreter (void);

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
DEFINE_TYPE_TEST_PRIMITIVES(Color, 1)

void test_TSchemeInterpreter (void)
{
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

	TRect_test_values[0] = TRect(2, 1, 4, 3);
	REGISTER_TYPE_TEST_PRIMITIVES(TRect);

	Color_test_values[0] = Color(1, 2, 3, 4);
	REGISTER_TYPE_TEST_PRIMITIVES(Color);
	
    boost::scoped_ptr<TInterpreter> interp(new TSchemeInterpreter());
}
