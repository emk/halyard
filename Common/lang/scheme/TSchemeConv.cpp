// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"
#include "TSchemeConv.h"

USING_NAMESPACE_FIVEL
using GraphicsTools::Color;

REGISTER_TEST_CASE_FILE(TSchemeConv);


//=========================================================================
//  TValueToScheme
//=========================================================================

static Scheme_Object *MakeSchemeList(const TValueList &inList) {
	Scheme_Object *result = scheme_null;

	TValueList::const_reverse_iterator i = inList.rbegin();
	for (; i != inList.rend(); ++i) {
		result = scheme_make_pair(TValueToScheme(*i), result);

	}
	return result;
}


Scheme_Object *TValueToScheme(TValue inVal) {
	switch (inVal.GetType())
	{
		case TValue::TYPE_NULL:
			return scheme_null;
			
		case TValue::TYPE_STRING:
			return scheme_make_string(std::string(inVal).c_str());
			
		case TValue::TYPE_SYMBOL:
			return scheme_intern_symbol(TSymbol(inVal).GetName().c_str());
			
		case TValue::TYPE_LONG:
			return scheme_make_integer_value(int32(inVal));
			
		case TValue::TYPE_ULONG:
			return scheme_make_integer_value_from_unsigned(uint32(inVal));
			
		case TValue::TYPE_DOUBLE:
			return scheme_make_double(inVal);
			
		case TValue::TYPE_BOOLEAN:
			return bool(inVal) ? scheme_true : scheme_false;
			
		case TValue::TYPE_POINT:
			return TSchemeInterpreter::MakeSchemePoint(inVal);
			
		case TValue::TYPE_RECT:
			return TSchemeInterpreter::MakeSchemeRect(inVal);
			
		case TValue::TYPE_COLOR:
			return TSchemeInterpreter::MakeSchemeColor(inVal);
			
		case TValue::TYPE_LIST:
			return MakeSchemeList(inVal);
			
		case TValue::TYPE_POLYGON:
			return TSchemeInterpreter::MakeSchemePolygon(inVal);
	
		// TValue::TYPE_CALLBACK goes here.

		case TValue::TYPE_PERCENT:
			return TSchemeInterpreter::MakeSchemePercent(inVal);
			
		default:
			THROW("Unhandled TValue conversion");
	}
	
	ASSERT(false); // We should never get here.
	return scheme_null;
}


//=========================================================================
//  SchemeToTValue
//=========================================================================

/*
static TPoint SchemeToTPoint(Scheme_Object *inVal) {
	
	return TPoint(TSchemeInterpreter::GetInt32Member("point-x", inVal),
				  TSchemeInterpreter::GetInt32Member("point-y", inVal));
}


static TValue SchemeStructToTValue(Scheme_Object *inVal) {
	// Our first test is for a point
	Scheme_Object *b = TSchemeInterpreter::CallScheme("point?", 1, &inVal);
	if (SCHEME_TRUEP(b))
		return TValue(SchemeToTPoint(inVal));

	return TValue();

}
*/

TValue SchemeToTValue(Scheme_Object *inVal) {
	Scheme_Type type = SCHEME_TYPE(inVal);
	
	switch (type)
	{
		case scheme_string_type:
			return TValue(std::string(SCHEME_STR_VAL(inVal), 
									  SCHEME_STRLEN_VAL(inVal)));

		case scheme_symbol_type:
			return TValue(std::string(SCHEME_SYM_VAL(inVal)));

		case scheme_double_type:
			return TValue(scheme_real_to_double(inVal));

		case scheme_true_type:
			return TValue(true);

		case scheme_false_type:
			return TValue(false);

//		case scheme_structure_type:
//			return SchemeStructToTValue(inVal);
									  
		default:
			THROW("Unhandled Scheme Object conversion");
	}

	ASSERT(false);  // We should never get here.
	return TValue();
}

//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

// This function compares two scheme objects for equality.
// Note: We are using the scheme EQUALS? since we might be
// dealing with swindle classes.
static bool SchemeEquals(const Scheme_Object *inObj1,
						 const Scheme_Object *inObj2)
{
	Scheme_Object *args[2];
	args[0] = const_cast<Scheme_Object *>(inObj1);
	args[1] = const_cast<Scheme_Object *>(inObj2);
	return SCHEME_TRUEP(TSchemeInterpreter::CallScheme("%kernel-equals?",
													   2, args));
}


#define CHECK_SCHEME_EQUALS(expr1, expr2) \
    CheckFuncHelper(__FILE__, __LINE__, "SchemeEquals", #expr1, #expr2, \
                    SchemeEquals, expr1, expr2)

void CHECK_TVALUE_CONV(TValue inVal, const Scheme_Object *inResult) {
	CHECK_SCHEME_EQUALS(TValueToScheme(inVal), inResult);
}

BEGIN_TEST_CASE(TestTValueToScheme, TestCase) {
	CHECK_EQ(SchemeEquals(scheme_make_string("foo"),
						  scheme_make_string("foo")),
			 true);
	CHECK_EQ(SchemeEquals(scheme_make_string("foo"),
						  scheme_make_string("bar")),
			 false);

	// Test simple types.
	CHECK_TVALUE_CONV(TNull(), scheme_null);
	CHECK_TVALUE_CONV("Hello", scheme_make_string("Hello"));
	CHECK_TVALUE_CONV(TSymbol("foo"), scheme_intern_symbol("foo"));
	CHECK_TVALUE_CONV(MAX_INT32, scheme_make_integer_value(MAX_INT32));
	CHECK_TVALUE_CONV(MIN_INT32, scheme_make_integer_value(MIN_INT32));
	CHECK_TVALUE_CONV(MAX_UINT32, 
					  scheme_make_integer_value_from_unsigned(MAX_UINT32));
	CHECK_TVALUE_CONV(10.0, scheme_make_double(10.0));
	CHECK_TVALUE_CONV(true, scheme_true);
	CHECK_TVALUE_CONV(false, scheme_false);
	CHECK_TVALUE_CONV(TPoint(0, 1),
					  TSchemeInterpreter::MakeSchemePoint(TPoint(0, 1)));

	// Test more complicated types
	CHECK_TVALUE_CONV(TRect(0, 1, 2, 3), 
					  TSchemeInterpreter::MakeSchemeRect(TRect(0, 1, 2, 3)));
	CHECK_TVALUE_CONV(Color(0, 1, 2, 3), 
					  TSchemeInterpreter::MakeSchemeColor(Color(0, 1, 2, 3)));

	// Test lists.
	TValueList list;
	list.push_back(1);
	list.push_back("foo");
	Scheme_Object *result =
		scheme_make_pair(scheme_make_integer_value(1),
						 scheme_make_pair(scheme_make_string("foo"),
										  scheme_null));
	CHECK_TVALUE_CONV(list, result);

	// Test polygons.
	std::vector<TPoint> poly;
	poly.push_back(TPoint(0, 0));
    poly.push_back(TPoint(0, 10));
    poly.push_back(TPoint(10, 0));
	CHECK_TVALUE_CONV(TPolygon(poly), 
					  TSchemeInterpreter::MakeSchemePolygon(TPolygon(poly)));

	// Test other types.
	// callback - Will be interesting.  Do we actually need it?
	//	CHECK_THROWN(std::exception, 

	// percent
	CHECK_TVALUE_CONV(TPercent(72.0), 
					  TSchemeInterpreter::MakeSchemePercent(TPercent(72.0)));
} END_TEST_CASE(TestTValueToScheme);


void CHECK_SCHEME_CONV(Scheme_Object *inVal, const TValue inResult) {
	CHECK_EQ(SchemeToTValue(inVal), inResult);
}


BEGIN_TEST_CASE(TestSchemeToTValue, TestCase) {
/*
    enum Type {
        TYPE_NULL,      // No value.
/       TYPE_STRING,    // Regular string.
/       TYPE_SYMBOL,    // A symbol, as in Scheme.
        TYPE_LONG,      // A 32-bit signed integer.
        TYPE_ULONG,     // A 32-bit unsigned integer.
/       TYPE_DOUBLE,    // A floating point number.
/       TYPE_BOOLEAN,   // A boolean value.
        TYPE_POINT,     // A point.
        TYPE_RECT,      // A rectangle, right-bottom exclusive.
        TYPE_COLOR,     // An RGB color.
        TYPE_LIST,      // A list of TValues
        TYPE_POLYGON,   // A TPolygon
        TYPE_CALLBACK,  // A scripting language callback
        TYPE_PERCENT    // A TPercent
*/

	// We will not be converting scheme_nulls to TNulls
	
	// Simple types
	CHECK_SCHEME_CONV(scheme_make_string("hello"), TValue("hello"));
    CHECK_SCHEME_CONV(scheme_intern_symbol("foo"), 
					  TValue("foo"));
	// Long
	// ULong
	CHECK_SCHEME_CONV(scheme_make_double(32.0), 
					  TValue(32.0));
	CHECK_SCHEME_CONV(scheme_true, TValue(true));
	CHECK_SCHEME_CONV(scheme_false, TValue(false));
	
	// More complex types
	//CHECK_EQ(SchemeToTValue(TSchemeInterpreter::MakeSchemePoint(TPoint(0, 1))),
	//TValue(TPoint(0, 1)));
	
	//Scheme_Object *obj = TSchemeInterpreter::MakeSchemePoint(TPoint(0, 1));
    //CHECK_EQ(SchemeStructToTValue(obj), TValue(TPoint(0, 1)));
	
} END_TEST_CASE(TestSchemeToTValue);

#endif // BUILD_TEST_CASES


