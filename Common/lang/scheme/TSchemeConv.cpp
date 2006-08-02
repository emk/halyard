// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
#include "TSchemeInterpreter.h"
#include "TSchemeConv.h"
#include "TSchemeRtCallback.h"

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

Scheme_Object *FIVEL_NS TValueToScheme(TValue inVal) {
	switch (inVal.GetType())
	{	
		case TValue::TYPE_NULL:
			return scheme_void;

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

static void SchemeTypeCheckFail() {
	throw TException(__FILE__, __LINE__, "Argument type mismatch");
}

static void SchemeTypeCheck(Scheme_Type inType, Scheme_Object *inValue) {
	if (inType != SCHEME_TYPE(inValue))
		SchemeTypeCheckFail();
}

static void SchemeTypeCheckStruct(const char *inPredicate,
								  Scheme_Object *inVal)
{
	// We want to verify that inVal is an instance of a Swindle class 
	// class specified by inPredicate. If the object if of the right 
	// class, CallScheme returns true.
	Scheme_Object *b = TSchemeInterpreter::CallScheme(inPredicate, 1, &inVal);
	if (SCHEME_FALSEP(b))
		SchemeTypeCheckFail();
}

static Scheme_Object *SchemeGetMember(const char *inName,
                                      Scheme_Object *inVal)
{
    return TSchemeInterpreter::CallScheme(inName, 1, &inVal);
}

static int32 SchemeGetInt32Member(const char *inName,
								  Scheme_Object *inVal)
{
	Scheme_Object *val = SchemeGetMember(inName, inVal);
	if (!SCHEME_EXACT_INTEGERP(val))
		SchemeTypeCheckFail();
	long result;
	if (!scheme_get_int_val(val, &result))
		SchemeTypeCheckFail();
	return result;
}

static double SchemeGetRealMember(const char *inName,
								  Scheme_Object *inVal)
{
	Scheme_Object *val = SchemeGetMember(inName, inVal);
	if (!SCHEME_REALP(val))
		SchemeTypeCheckFail();
	return scheme_real_to_double(val);
}

static TValue SchemeLongOrULongToTValue(Scheme_Object *inVal) {
	if (!SCHEME_EXACT_INTEGERP(inVal))
		SchemeTypeCheckFail();
	
	// Since there is no distinction in Scheme between a signed
	// and an unsigned integer, we will assume that inVal is a
	// signed integer, unless it cannot fit into a C++ signed 
	// long.
	int32 result;
	if (scheme_get_int_val(inVal, &result)) 
		return TValue(result);

	uint32 uresult;
	if (scheme_get_unsigned_int_val(inVal, &uresult))
		return TValue(uresult);

	SchemeTypeCheckFail();

	// we should never get this far
	ASSERT(false);
	return TValue();
}

static TValue SchemeToTPoint(Scheme_Object *inVal) {	
	return TValue(TPoint(SchemeGetInt32Member("point-x", inVal),
						 SchemeGetInt32Member("point-y", inVal)));
}

static TValue SchemeToTRect(Scheme_Object *inVal) {	
	return TValue(TRect(SchemeGetInt32Member("rect-left", inVal),
						SchemeGetInt32Member("rect-top", inVal),
						SchemeGetInt32Member("rect-right", inVal),
						SchemeGetInt32Member("rect-bottom", inVal)));
}

static TValue SchemeToColor(Scheme_Object *inVal) {
	using GraphicsTools::Channel;
	return TValue(Color(SchemeGetInt32Member("color-red", inVal),
						SchemeGetInt32Member("color-green", inVal),
						SchemeGetInt32Member("color-blue", inVal),
						SchemeGetInt32Member("color-alpha", inVal)));
}

static TValue SchemeListToTValue(Scheme_Object *inVal) {
	TValueList list;
	while (SCHEME_PAIRP(inVal)) {
		list.push_back(SchemeToTValue(SCHEME_CAR(inVal)));
		inVal = SCHEME_CDR(inVal);
	}
	if (!SCHEME_NULLP(inVal))
		THROW("Cannot pass dotted lists to primitives");

	return TValue(list);
}

static TValue SchemeToTPolygon(Scheme_Object *inVal) {	
	std::vector<TPoint> pts;
	Scheme_Object *scheme_pts = 
		TSchemeInterpreter::CallScheme("polygon-vertices", 1, &inVal);
	if (!(SCHEME_PAIRP(scheme_pts) || SCHEME_NULLP(scheme_pts)))
		SchemeTypeCheckFail();
	
	Scheme_Object *current;
	while (SCHEME_PAIRP(scheme_pts)) {
		current = SCHEME_CAR(scheme_pts);
		SchemeTypeCheckStruct("point?", current);
		pts.push_back(TPoint(SchemeGetInt32Member("point-x", current),
							 SchemeGetInt32Member("point-y", current)));
		scheme_pts = SCHEME_CDR(scheme_pts);
		if (!(SCHEME_PAIRP(scheme_pts) || SCHEME_NULLP(scheme_pts)))
			SchemeTypeCheckFail();
	}

	return TValue(TPolygon(pts));	
}

static TValue SchemeToTPercent(Scheme_Object *inVal) {
	return TValue(TPercent(SchemeGetRealMember("percent-value", inVal)));
}

static TValue SchemeToRtCallback(Scheme_Object *inVal) {
    Scheme_Object *getter =
        SchemeGetMember("realtime-state-db-listener-getter-name", inVal);
    Scheme_Object *bindings =
        SchemeGetMember("realtime-state-db-listener-bindings", inVal);
    Scheme_Object *code =
        SchemeGetMember("realtime-state-db-listener-code", inVal);

    TSchemeRtCallback *ptr =
        new TSchemeRtCallback(TSymbol(SchemeToTValue(getter)).GetName(),
                              SchemeToTValue(bindings),
                              SchemeToTValue(code));
    return TCallbackPtr(ptr);
}

/// TypeInfo encapsulates the name of a Scheme predicate to check
/// the type of a Scheme object, and the conversion function to 
/// convert the Scheme object into a TValue if the predicate
/// returns true.
struct TypeInfo {
    const char *predicate;
	TValue (*conv)(Scheme_Object *);
};

/// Remember to add TypeInfo data for any new supported Swindle 
/// classes that TSchemeConv can support.
static TypeInfo gTypeInfo[] = {
	{"point?", &SchemeToTPoint},
	{"rect?", &SchemeToTRect},
	{"color?", &SchemeToColor},
	{"polygon?", &SchemeToTPolygon},
	{"percent?", &SchemeToTPercent},
    {"realtime-state-db-listener?", &SchemeToRtCallback},
	{NULL, NULL}
};

static TValue SchemeClosureToTValue(Scheme_Object *inVal) {
	return TValue(TCallbackPtr(new TSchemeCallback(inVal)));
}

static TValue SchemeStructToTValue(Scheme_Object *inVal) {
	// Loop through the array of all supported SchemeToTValue 
	// conversions for struct types.
	int i = 0;
	while (gTypeInfo[i].predicate != NULL) {
		Scheme_Object *b = 
			TSchemeInterpreter::CallScheme(gTypeInfo[i].predicate,
										   1, &inVal);
		if (SCHEME_TRUEP(b))
			return gTypeInfo[i].conv(inVal);
		i++;
	}
	return TValue();
}

TValue FIVEL_NS SchemeToTValue(Scheme_Object *inVal) {
	Scheme_Type type = SCHEME_TYPE(inVal);
	
	switch (type) {
   	    case scheme_void_type:
			return TValue(TNull());

	    case scheme_string_type:
			return TValue(std::string(SCHEME_STR_VAL(inVal), 
									  SCHEME_STRLEN_VAL(inVal)));

		case scheme_symbol_type:
			return TValue(TSymbol(std::string(SCHEME_SYM_VAL(inVal))));

		// We will convert both 16bit and 32bit numbers to C++
		// longs or ulongs.
		case scheme_integer_type:
		case scheme_bignum_type:
			return SchemeLongOrULongToTValue(inVal);
			
		case scheme_rational_type:
		case scheme_double_type:
			return TValue(scheme_real_to_double(inVal));

		case scheme_true_type :
			return TValue(true);

		case scheme_false_type:
			return TValue(false);

		case scheme_null_type:
		case scheme_pair_type:
			return SchemeListToTValue(inVal);

		case scheme_proc_struct_type:
			return SchemeStructToTValue(inVal);
									  
		default:
			if (SCHEME_PROCP(inVal))
				return SchemeClosureToTValue(inVal); // for callbacks!!
			else
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

	// We don't test callback because we're not sure we need to support
	// returning it to Scheme.

	// Test percent.
	CHECK_TVALUE_CONV(TPercent(72.0), 
					  TSchemeInterpreter::MakeSchemePercent(TPercent(72.0)));
} END_TEST_CASE(TestTValueToScheme);


void CHECK_SCHEME_CONV(Scheme_Object *inVal, const TValue inResult) {
	CHECK_EQ(SchemeToTValue(inVal), inResult);
}


BEGIN_TEST_CASE(TestSchemeToTValue, TestCase) {
	
	// Simple types
	CHECK_SCHEME_CONV(scheme_void, TValue(TNull()));
	CHECK_SCHEME_CONV(scheme_make_string("hello"), TValue("hello"));
    CHECK_SCHEME_CONV(scheme_intern_symbol("foo"), 
					  TValue(TSymbol("foo")));
	CHECK_SCHEME_CONV(scheme_make_integer_value(MAX_INT32),
					  TValue(MAX_INT32));
	CHECK_SCHEME_CONV(scheme_make_integer_value(MIN_INT32),
					  TValue(MIN_INT32));		
	CHECK_SCHEME_CONV(scheme_make_integer_value_from_unsigned(MAX_UINT32),
					  TValue(MAX_UINT32));
	
	CHECK_SCHEME_CONV(scheme_make_double(32.0), 
					  TValue(32.0));
	CHECK_SCHEME_CONV(scheme_true, TValue(true));
	CHECK_SCHEME_CONV(scheme_false, TValue(false));
	
	// More complex types
	CHECK_SCHEME_CONV(TSchemeInterpreter::MakeSchemePoint(TPoint(0, 1)),
					  TValue(TPoint(0, 1)));

	CHECK_SCHEME_CONV(TSchemeInterpreter::MakeSchemeRect(TRect(0, 1, 2, 3)),
					  TValue(TRect(0, 1, 2, 3)));
	
	CHECK_SCHEME_CONV(TSchemeInterpreter::MakeSchemeColor(Color(0, 1, 2, 3)),
					  TValue(Color(0, 1, 2, 3)));

	// Test lists.
	CHECK_SCHEME_CONV(scheme_null, TValueList());
	TValueList list;
	list.push_back(1);
	list.push_back("foo");
	Scheme_Object *result =
		scheme_make_pair(scheme_make_integer_value(1),
						 scheme_make_pair(scheme_make_string("foo"),
										  scheme_null));
	CHECK_SCHEME_CONV(result, list);

	// Test polygons.
	std::vector<TPoint> poly;
	poly.push_back(TPoint(0, 0));
    poly.push_back(TPoint(0, 10));
    poly.push_back(TPoint(10, 0));
	CHECK_SCHEME_CONV(TSchemeInterpreter::MakeSchemePolygon(TPolygon(poly)),
					  TPolygon(poly));

	// Test percent.
	CHECK_SCHEME_CONV(TSchemeInterpreter::MakeSchemePercent(TPercent(72.0)),
					  TPercent(72.0));
	
} END_TEST_CASE(TestSchemeToTValue);

#endif // BUILD_TEST_CASES


