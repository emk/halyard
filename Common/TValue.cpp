// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TValue.h"
#include "TestCase.h"

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(TValue);


//=========================================================================
//  Tests
//=========================================================================

/*
*/

#if BUILD_TEST_CASES

template <typename Type>
CHECK_TVALUE_TYPE(TValue::Type inType, const Type &v1, const Type &v2) {
    TValue value(v1);
    CHECK_EQ(value.IsInitialized(), true);
    CHECK_EQ(value.GetType(), inType);
    CHECK_EQ(value, TValue(v1));
    CHECK_NE(value, TValue(v2));
}

template <typename Type>
CHECK_TVALUE_GET(const Type &inVal) {
    TValue value(inVal);
    CHECK_EQ(Type(value), inVal);
}

BEGIN_TEST_CASE(TestTSymbol, TestCase) {
    CHECK_EQ(TSymbol("foo").GetName(), std::string("foo"));
    CHECK_EQ(TSymbol("foo"), TSymbol("foo"));
    CHECK_NE(TSymbol("foo"), TSymbol("bar"));
} END_TEST_CASE(TestTSymbol);

BEGIN_TEST_CASE(TestTPercent, TestCase) {
    CHECK_EQ(TPercent(10.0).GetValue(), 10.0);
    CHECK_EQ(TPercent(10.0), TPercent(10.0));
    CHECK_NE(TPercent(10.0), TPercent(20.5));
} END_TEST_CASE(TestTPercent);

BEGIN_TEST_CASE(TestTValue, TestCase) {
    // Uninitialized values.
    TValue v1;
	CHECK_EQ(v1.IsInitialized(), false);
    CHECK_THROWN(std::exception, v1.GetType());

    // Check Null.
    TValue value(TNull());
    CHECK_EQ(value.IsInitialized(), true);
    CHECK_EQ(value.GetType(), TValue::TYPE_NULL);
    CHECK_EQ(value, TValue(TNull()));

    // Check simple types.
    CHECK_TVALUE_TYPE(TValue::TYPE_LONG, 10, 20);      // int
    CHECK_TVALUE_TYPE(TValue::TYPE_LONG, MIN_INT32, MAX_INT32);
    CHECK_TVALUE_GET(MIN_INT32);
    CHECK_TVALUE_GET(MAX_INT32);

    CHECK_TVALUE_TYPE(TValue::TYPE_ULONG, MAX_UINT32, MAX_UINT32 - 1);
    CHECK_TVALUE_GET(MAX_UINT32);

    CHECK_TVALUE_TYPE(TValue::TYPE_DOUBLE, 0.5, 0.25);   // double
    CHECK_TVALUE_GET(0.75);

    CHECK_TVALUE_TYPE(TValue::TYPE_BOOLEAN, true, false); // bool
    CHECK_TVALUE_GET(true);
    CHECK_TVALUE_GET(false);

    CHECK_TVALUE_TYPE(TValue::TYPE_POINT, TPoint(0, 0), TPoint(0, 1));
    CHECK_TVALUE_GET(TPoint(5, 6));

    CHECK_TVALUE_TYPE(TValue::TYPE_RECT, TRect(0, 0, 0, 0), TRect(0, 1, 2, 3));
    CHECK_TVALUE_GET(TRect(5, 6, 7, 8));

    CHECK_TVALUE_TYPE(TValue::TYPE_COLOR,
                      GraphicsTools::Color(0, 0, 0, 0),
                      GraphicsTools::Color(0, 1, 2, 3));
    CHECK_TVALUE_GET(GraphicsTools::Color(5, 6, 7, 8));

    CHECK_TVALUE_TYPE(TValue::TYPE_STRING,
                      std::string("foo"), std::string("bar"));
    CHECK_TVALUE_TYPE(TValue::TYPE_STRING, "foo", "bar");
    CHECK_TVALUE_GET(std::string("foo"));
    
    CHECK_TVALUE_TYPE(TValue::TYPE_SYMBOL, TSymbol("foo"), TSymbol("bar"));
    CHECK_TVALUE_GET(TSymbol("moby"));

    CHECK_TVALUE_TYPE(TValue::TYPE_PERCENT, TPercent(10.0), TPercent(30.0));
    CHECK_TVALUE_GET(TPercent(75.0));

    // Check TPolygon.
    std::vector<TPoint> poly1, poly2;
    poly1.push_back(TPoint(0, 0));
    poly1.push_back(TPoint(0, 10));
    poly1.push_back(TPoint(10, 0));
    poly2.push_back(TPoint(0, 0));
    poly2.push_back(TPoint(0, 5));
    poly2.push_back(TPoint(5, 0));
    CHECK_TVALUE_TYPE(TValue::TYPE_POLYGON, TPolygon(poly1), TPolygon(poly2));
    CHECK_TVALUE_GET(TPolygon(poly1));

    // Check TValueList.
    TValueList list1, list2;
    list1.push_back(0);
    list1.push_back("hello");
    list2.push_back(1);
    list2.push_back("hello world!");
    CHECK_TVALUE_TYPE(TValue::TYPE_LIST, list1, list2);
    CHECK_TVALUE_GET(list1);

    // Conversion operator sanity checks.
    CHECK_THROWN(std::exception, double(TValue(10)));
    CHECK_THROWN(std::exception, double(TValue()));

    // operator== special cases
    CHECK_THROWN(std::exception, TValue() == TValue());
    CHECK_THROWN(std::exception, TValue() == TValue(10));
    CHECK_THROWN(std::exception, TValue(10) == TValue());
    CHECK_NE(TValue(10), TValue(10.0));
    CHECK_NE(TValue(10.0), TValue(10));
    CHECK_NE(TValue(1), TValue(true));
    CHECK_NE(TValue(0), TValue(false));
    
} END_TEST_CASE(TestTValue);

#endif // BUILD_TEST_CASES
