// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"
#include "TValue.h"

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(TSchemeConv);

Scheme_Object *TValueToScheme(TValue inVal) {
	switch (inVal.GetType())
	{
	case TValue::TYPE_NULL:
		return scheme_null;
		
	default:
		THROW("Unhandled TValue conversion");
	}

	ASSERT(false); // We should never get here.
	return scheme_null;
}

//=========================================================================
//  Tests
//=========================================================================

/*
 Type {
        TYPE_NULL,      // No value.
        TYPE_STRING,    // Regular string.
        TYPE_SYMBOL,    // A symbol, as in Scheme.
        TYPE_LONG,      // A 32-bit signed integer.
        TYPE_ULONG,     // A 32-bit unsigned integer.
        TYPE_DOUBLE,    // A floating point number.
        TYPE_BOOLEAN,   // A boolean value.
        TYPE_POINT,     // A point.
        TYPE_RECT,      // A rectangle, right-bottom exclusive.
        TYPE_COLOR,     // An RGB color.
        TYPE_LIST,      // A list of TValues
        TYPE_POLYGON,   // A TPolygon
        TYPE_CALLBACK,  // A scripting language callback
        TYPE_PERCENT    // A TPercent
*/

#if BUILD_TEST_CASES

BEGIN_TEST_CASE(TestTSchemeConv, TestCase) {
	CHECK_EQ(TValueToScheme(TValue(TNull())), scheme_null);
	//CHECK_EQ(TValueToScheme("Hello"), scheme_make_string("Hello"));
} END_TEST_CASE(TestTSchemeConv);

#endif // BUILD_TEST_CASES


