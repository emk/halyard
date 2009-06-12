// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

using namespace Halyard;
using GraphicsTools::Color;

REGISTER_TEST_CASE_FILE(TSchemeConv);


//=========================================================================
//  TValueToScheme
//=========================================================================

static Scheme_Object *MakeSchemePoint(const TPoint &inPoint) {
    TSchemeArgs<2> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();
    
    args[0] = scheme_make_integer_value(inPoint.X());
    args[1] = scheme_make_integer_value(inPoint.Y());
    return TSchemeInterpreter::CallSchemeStatic("point", args.size(),
                                                args.get());
}

static Scheme_Object *MakeSchemeRect(const TRect &inRect) {
    TSchemeArgs<4> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();

    args[0] = scheme_make_integer_value(inRect.Left());
    args[1] = scheme_make_integer_value(inRect.Top());
    args[2] = scheme_make_integer_value(inRect.Right());
    args[3] = scheme_make_integer_value(inRect.Bottom());
    return TSchemeInterpreter::CallSchemeStatic("rect", args.size(),
                                                args.get());
}

static Scheme_Object *MakeSchemeColor(const GraphicsTools::Color &inColor) {
    TSchemeArgs<4> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();

    args[0] = scheme_make_integer_value(inColor.red);
    args[1] = scheme_make_integer_value(inColor.green);
    args[2] = scheme_make_integer_value(inColor.blue);
    args[3] = scheme_make_integer_value(inColor.alpha);
    return TSchemeInterpreter::CallSchemeStatic("color", args.size(),
                                                args.get());
}

static Scheme_Object *MakeSchemePolygon(const TPolygon &inPoly) {
    // See http://www.parashift.com/c++-faq-lite/containers.html#faq-34.3
    // (in the standard C++ FAQ), which contains a section "Is the storage
    // for a std::vector<T> guaranteed to be contiguous?".  The answer is
    // "yes," assuming we don't resize the vector.  So we can use &args[0]
    // and get a C pointer.
    std::vector<TPoint> vertices(inPoly.Vertices());
    size_t sz = vertices.size();
    std::vector<Scheme_Object*> args(sz, NULL); 

    TSchemeReg<0,1> reg;
    // XXX - We need to store the pointer to args in a real variable, so
    // that local_array can take a reference to that variable and register
    // it appropriately with the GC API.  This needs to be better
    // encapsulated.
    Scheme_Object **args_ptr = &args[0];
    reg.local_array(args_ptr, args.size());
    reg.done();

    std::vector<TPoint>::iterator i = vertices.begin();
    for (int j = 0; i != vertices.end(); ++j, ++i)
        args[j] = MakeSchemePoint(*i);
    ASSERT(args.size() == sz);
    return TSchemeInterpreter::CallSchemeStatic("polygon", args.size(),
                                                &args[0]);
}

static Scheme_Object *MakeSchemePercent(const TPercent &inPercent) {
    TSchemeArgs<1> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();

    args[0] = scheme_make_double(inPercent.GetValue());
    return TSchemeInterpreter::CallSchemeStatic("percent", args.size(),
                                                args.get());
}

static Scheme_Object *MakeSchemeList(const TValueList &inList) {
    Scheme_Object *result = NULL, *car = NULL;

    TSchemeReg<2> reg;
    reg.local(result);
    reg.local(car);
    reg.done();

    result = scheme_null;
    TValueList::const_reverse_iterator i = inList.rbegin();
    for (; i != inList.rend(); ++i) {
        car = TValueToScheme(*i);
        result = scheme_make_pair(car, result);

    }
    return result;
}

Scheme_Object *Halyard::TValueToScheme(TValue inVal) {
    // MANUAL GC PROOF REQUIRED - Any Scheme objects we allocate in this
    // function are immediately returned.  So for now, we don't need to use
    // TSchemeReg here.
    switch (inVal.GetType()) {
        case TValue::TYPE_NULL: {
            return scheme_void;
        }

        case TValue::TYPE_STRING: {
            std::string str(tvalue_cast<std::string>(inVal));
            return scheme_make_utf8_string(str.c_str());
        }
            
        case TValue::TYPE_SYMBOL: {
            TSymbol sym(tvalue_cast<TSymbol>(inVal));
            return scheme_intern_symbol(sym.GetName().c_str());
        }
            
        case TValue::TYPE_LONG: {
            return scheme_make_integer_value(tvalue_cast<int32>(inVal));
        }
            
        case TValue::TYPE_ULONG: {
            uint32 u(tvalue_cast<uint32>(inVal));
            return scheme_make_integer_value_from_unsigned(u);
        }
            
        case TValue::TYPE_DOUBLE: {
            return scheme_make_double(tvalue_cast<double>(inVal));
        }
            
        case TValue::TYPE_BOOLEAN: {
            return tvalue_cast<bool>(inVal) ? scheme_true : scheme_false;
        }
            
        case TValue::TYPE_POINT: {
            TPoint p(tvalue_cast<TPoint>(inVal));
            return MakeSchemePoint(p);
        }
            
        case TValue::TYPE_RECT: {
            TRect r(tvalue_cast<TRect>(inVal));
            return MakeSchemeRect(r);
        }
            
        case TValue::TYPE_COLOR: {
            GraphicsTools::Color c(tvalue_cast<GraphicsTools::Color>(inVal));
            return MakeSchemeColor(c);
        }
            
        case TValue::TYPE_LIST: {
            return MakeSchemeList(tvalue_cast<TValueList>(inVal));
        }
            
        case TValue::TYPE_POLYGON: {
            TPolygon poly(tvalue_cast<TPolygon>(inVal));
            return MakeSchemePolygon(poly);
        }
    
        // TValue::TYPE_CALLBACK goes here.

        case TValue::TYPE_PERCENT: {
            TPercent pc(tvalue_cast<TPercent>(inVal));
            return MakeSchemePercent(pc);
        }
            
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

static void SchemeTypeCheckStruct(const char *inPredicate,
                                  Scheme_Object *inVal)
{
    Scheme_Object *b = NULL;
    TSchemeArgs<1> args;
    
    TSchemeReg<2,1> reg;
    reg.param(inVal);
    reg.local(b);
    reg.args(args);
    reg.done();

    // We want to verify that inVal is an instance of a Swindle class 
    // class specified by inPredicate. If the object if of the right 
    // class, CallSchemeStatic returns true.
    args[0] = inVal;
    b = TSchemeInterpreter::CallSchemeStatic(inPredicate, args.size(),
                                             args.get());
    if (SCHEME_FALSEP(b))
        SchemeTypeCheckFail();
}

static Scheme_Object *SchemeGetMember(const char *inName,
                                      Scheme_Object *inVal)
{
    TSchemeArgs<1> args;

    TSchemeReg<1,1> reg;
    reg.param(inVal);
    reg.args(args);
    reg.done();

    args[0] = inVal;
    return TSchemeInterpreter::CallSchemeStatic(inName, args.size(),
                                                args.get());
}

static int32 SchemeGetInt32Member(const char *inName,
                                  Scheme_Object *inVal)
{
    Scheme_Object *val = NULL;

    TSchemeReg<2> reg;
    reg.param(inVal);
    reg.local(val);
    reg.done();

    val = SchemeGetMember(inName, inVal);
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
    Scheme_Object *val = NULL;
    
    TSchemeReg<2> reg;
    reg.param(inVal);
    reg.local(val);
    reg.done();

    val = SchemeGetMember(inName, inVal);
    if (!SCHEME_REALP(val))
        SchemeTypeCheckFail();
    return scheme_real_to_double(val);
}

static TValue SchemeLongOrULongToTValue(Scheme_Object *inVal) {
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();

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
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();
    
    // Note that SchemeGetInt32Member returns an integer, so we don't need to
    // register it with the GC.
    return TValue(TPoint(SchemeGetInt32Member("point-x", inVal),
                         SchemeGetInt32Member("point-y", inVal)));
}

static TValue SchemeToTRect(Scheme_Object *inVal) {
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();

    return TValue(TRect(SchemeGetInt32Member("rect-left", inVal),
                        SchemeGetInt32Member("rect-top", inVal),
                        SchemeGetInt32Member("rect-right", inVal),
                        SchemeGetInt32Member("rect-bottom", inVal)));
}

static TValue SchemeToColor(Scheme_Object *inVal) {
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();

    using GraphicsTools::Channel;
    return TValue(Color(SchemeGetInt32Member("color-red", inVal),
                        SchemeGetInt32Member("color-green", inVal),
                        SchemeGetInt32Member("color-blue", inVal),
                        SchemeGetInt32Member("color-alpha", inVal)));
}

static TValue SchemeListToTValue(Scheme_Object *inVal) {
    Scheme_Object *car = NULL;

    TSchemeReg<2> reg;
    reg.param(inVal);
    reg.local(car);
    reg.done();

    TValueList list;
    while (SCHEME_PAIRP(inVal)) {
        car = SCHEME_CAR(inVal);
        list.push_back(SchemeToTValue(car));
        inVal = SCHEME_CDR(inVal);
    }
    if (!SCHEME_NULLP(inVal))
        THROW("Cannot pass dotted lists to primitives");
    return TValue(list);
}

static TValue SchemeToTPolygon(Scheme_Object *inVal) {
    Scheme_Object *scheme_pts = NULL, *current = NULL;
    TSchemeArgs<1> args;

    TSchemeReg<3,1> reg;
    reg.param(inVal);
    reg.local(scheme_pts);
    reg.local(current);
    reg.args(args);
    reg.done();

    std::vector<TPoint> pts;
    args[0] = inVal;
    scheme_pts = TSchemeInterpreter::CallSchemeStatic("polygon-vertices",
                                                      args.size(), args.get());
    if (!(SCHEME_PAIRP(scheme_pts) || SCHEME_NULLP(scheme_pts)))
        SchemeTypeCheckFail();
    
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
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();

    return TValue(TPercent(SchemeGetRealMember("percent-value", inVal)));
}

/// TypeInfo encapsulates the name of a Scheme predicate to check
/// the type of a Scheme object, and the conversion function to 
/// convert the Scheme object into a TValue if the predicate
/// returns true.
struct TypeInfo {
    const char *predicate;
    TValue (*conv)(Scheme_Object *);
};

/// Remember to add TypeInfo data for any new supported Scheme structs that
/// TSchemeConv can support.
static TypeInfo gTypeInfo[] = {
    {"point?", &SchemeToTPoint},
    {"rect?", &SchemeToTRect},
    {"color?", &SchemeToColor},
    {"polygon?", &SchemeToTPolygon},
    {"percent?", &SchemeToTPercent},
    {NULL, NULL}
};

static TValue SchemeClosureToTValue(Scheme_Object *inVal) {
    TSchemeReg<1> reg;
    reg.param(inVal);
    reg.done();

    return TValue(TCallbackPtr(new TSchemeCallback(inVal)));
}

static TValue SchemeStructToTValue(Scheme_Object *inVal) {
    Scheme_Object *b = NULL;
    TSchemeArgs<1> args;

    TSchemeReg<2,1> reg;
    reg.param(inVal);
    reg.local(b);
    reg.args(args);
    reg.done();

    // Loop through the array of all supported SchemeToTValue 
    // conversions for struct types.
    int i = 0;
    while (gTypeInfo[i].predicate != NULL) {
        args[0] = inVal;
        b =  TSchemeInterpreter::CallSchemeStatic(gTypeInfo[i].predicate, 
                                                  args.size(), args.get());
        if (SCHEME_TRUEP(b)) {
            TValue result(gTypeInfo[i].conv(inVal));
            ASSERT(result.IsInitialized());
            return result;
        }
        i++;
    }
    THROW("Unknown Scheme structure type");
    return TValue();
}

TValue Halyard::SchemeToTValue(Scheme_Object *inVal) {
    Scheme_Object *byte_str = NULL;
    char *str_val = NULL;

    TSchemeReg<3> reg;
    reg.param(inVal);
    reg.local(byte_str);
    reg.local(str_val);
    reg.done();
    
    Scheme_Type type = SCHEME_TYPE(inVal);
    
    switch (type) {
        case scheme_void_type:
            return TValue(TNull());

        case scheme_char_string_type:
            byte_str = scheme_char_string_to_byte_string(inVal);
            return SchemeToTValue(byte_str);

        case scheme_byte_string_type:
            str_val = SCHEME_BYTE_STR_VAL(inVal);
            return TValue(std::string(str_val, SCHEME_BYTE_STRLEN_VAL(inVal)));

        case SCHEME_PLATFORM_PATH_KIND:
            str_val = SCHEME_PATH_VAL(inVal);
            return TValue(std::string(str_val, SCHEME_PATH_LEN(inVal)));
            
        case scheme_symbol_type:
            // MANUAL GC PROOF REQUIRED - SCHEME_SYM_VAL here returns an
            // interior pointer, so we can't pass it TSchemeReg.  Please
            // manually verify that there are no allocations between the
            // time we call SCHEME_SYM_VAL and the time we make a safe copy
            // of it.
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
static bool SchemeEquals(Scheme_Object *inObj1, Scheme_Object *inObj2) {
    Scheme_Object *b = NULL;
    TSchemeArgs<2> args;

    TSchemeReg<3,1> reg;
    reg.param(inObj1);
    reg.param(inObj2);
    reg.local(b);
    reg.args(args);
    reg.done();

    args[0] = inObj1;
    args[1] = inObj2;
    b = TSchemeInterpreter::CallSchemeStatic("%kernel-equals?",
                                             args.size(), args.get());
    return SCHEME_TRUEP(b);
}

// This is a manually-instantiated version of TestCase.h's CheckFuncHelper,
// but with proper calls to TSchemeReg.
void SchemeEqualsHelper(const char *inErrorFile, int inErrorLine,
                        const char *inExpr1, const char *inExpr2,
                        Scheme_Object *inVal1, Scheme_Object *inVal2) {
    TSchemeReg<2> reg;
    reg.param(inVal1);
    reg.param(inVal2);
    reg.done();

    if (!SchemeEquals(inVal1, inVal2)) {
        std::ostringstream out;
        out << "expected (equals? (" << inExpr1 << ") (" << inExpr2
            << ")), got: " << inVal1 << ", " << inVal2;
        throw TestFailed(inErrorFile, inErrorLine, out.str());
    }
}

// MANUAL GC PROOF REQUIRED - This is a fairly complicated helper, but only
// because we want it to deal with a lot of GC-related issues for us.
// Here, VALUE will be converted to Scheme value, registered properly with
// the GC, and tested against RESULT.  The value returned by RESULT will
// also be registered correctly with the GC, so it's safe to write
// something like:
//
//   CHECK_TVALUE_CONV("Hello", scheme_make_utf8_string("Hello"));
//
// But note that you can't nest two allocating calls on the right-hand side:
//
//   // WILL NOT WORK!
//   CHECK_TVALUE_CONV(..., scheme_make_pair(car, scheme_make_pair(...));
//
// In this case, you must unwrap the inner call and assign it to a
// registered local variable, as usual.
#define CHECK_TVALUE_CONV(VALUE, RESULT) do { \
    Scheme_Object *value = NULL; \
    Scheme_Object *result = NULL; \
    \
    TSchemeReg<2> reg; \
    reg.local(value); \
    reg.local(result); \
    reg.done(); \
    \
    value = TValueToScheme(VALUE); \
    result = (RESULT); \
    \
    SchemeEqualsHelper(__FILE__, __LINE__, #VALUE, #RESULT, value, result); \
} while (0)

BEGIN_TEST_CASE(TestTValueToScheme, TestCase) {
    Scheme_Object *foo_str = NULL, *bar_str = NULL;
    Scheme_Object *car = NULL, *cdr = NULL;

    TSchemeReg<4> reg;
    reg.local(foo_str);
    reg.local(bar_str);
    reg.local(car);
    reg.local(cdr);
    reg.done();

    foo_str = scheme_make_utf8_string("foo");
    bar_str = scheme_make_utf8_string("bar");
    CHECK_EQ(SchemeEquals(foo_str, foo_str), true);
    CHECK_EQ(SchemeEquals(foo_str, bar_str), false);

    // Test simple types.
    CHECK_TVALUE_CONV("Hello", scheme_make_utf8_string("Hello"));
    CHECK_TVALUE_CONV(TSymbol("foo"), scheme_intern_symbol("foo"));
    CHECK_TVALUE_CONV(MAX_INT32, scheme_make_integer_value(MAX_INT32));
    CHECK_TVALUE_CONV(MIN_INT32, scheme_make_integer_value(MIN_INT32));
    CHECK_TVALUE_CONV(MAX_UINT32, 
                      scheme_make_integer_value_from_unsigned(MAX_UINT32));
    CHECK_TVALUE_CONV(10.0, scheme_make_double(10.0));
    CHECK_TVALUE_CONV(true, scheme_true);
    CHECK_TVALUE_CONV(false, scheme_false);
    CHECK_TVALUE_CONV(TPoint(0, 1),
                      MakeSchemePoint(TPoint(0, 1)));

    // Test more complicated types
    CHECK_TVALUE_CONV(TRect(0, 1, 2, 3), 
                      MakeSchemeRect(TRect(0, 1, 2, 3)));
    CHECK_TVALUE_CONV(Color(0, 1, 2, 3), 
                      MakeSchemeColor(Color(0, 1, 2, 3)));

    // Test lists.
    TValueList list;
    list.push_back(1);
    list.push_back("foo");
    car = scheme_make_integer_value(1);
    cdr = scheme_make_pair(foo_str, scheme_null);
    CHECK_TVALUE_CONV(list, scheme_make_pair(car, cdr));

    // Test polygons.  (Not all that useful; we're basically checking the
    // result of the conversion against the result of the function that it
    // calls to perform the conversion!)
    std::vector<TPoint> poly;
    poly.push_back(TPoint(0, 0));
    poly.push_back(TPoint(0, 10));
    poly.push_back(TPoint(10, 0));
    CHECK_TVALUE_CONV(TPolygon(poly), 
                      MakeSchemePolygon(TPolygon(poly)));

    // We don't test callback because we're not sure we need to support
    // returning it to Scheme.

    // Test percent.
    CHECK_TVALUE_CONV(TPercent(72.0), 
                      MakeSchemePercent(TPercent(72.0)));
} END_TEST_CASE(TestTValueToScheme);

// MANUAL GC PROOF REQUIRED - As with CHECK_TVALUE_CONV, we handle GC
// registration for VALUE so our caller doesn't have to.  But again, no
// nesting of allocating functions in VALUE is permitted.
#define CHECK_SCHEME_CONV(SCHEME_VALUE, RESULT) do { \
    Scheme_Object *scheme_value = NULL; \
    \
    TSchemeReg<1> reg; \
    reg.local(scheme_value); \
    reg.done(); \
    \
    scheme_value = (SCHEME_VALUE); \
    CHECK_EQ(SchemeToTValue(scheme_value), (RESULT));   \
} while (0)

BEGIN_TEST_CASE(TestSchemeToTValue, TestCase) {
    Scheme_Object *foo_str = NULL, *car = NULL, *cdr = NULL;

    TSchemeReg<3> reg;
    reg.local(foo_str);
    reg.local(car);
    reg.local(cdr);
    reg.done();

    foo_str = scheme_make_utf8_string("foo");
        
    // Simple types
    CHECK_SCHEME_CONV(scheme_void, TValue(TNull()));
    CHECK_SCHEME_CONV(scheme_make_utf8_string("hello"), TValue("hello"));
    CHECK_SCHEME_CONV(scheme_make_path("foo"), TValue("foo"));
    CHECK_SCHEME_CONV(scheme_intern_symbol("foo"), 
                      TValue(TSymbol("foo")));
    CHECK_SCHEME_CONV(scheme_make_integer_value(MAX_INT32),
                      TValue(MAX_INT32));
    CHECK_SCHEME_CONV(scheme_make_integer_value(MIN_INT32),
                      TValue(MIN_INT32));       
    CHECK_SCHEME_CONV(scheme_make_integer_value_from_unsigned(MAX_UINT32),
                      TValue(MAX_UINT32));
    
    CHECK_SCHEME_CONV(scheme_make_double(32.0), TValue(32.0));
    CHECK_SCHEME_CONV(scheme_true, TValue(true));
    CHECK_SCHEME_CONV(scheme_false, TValue(false));
    
    // More complex types
    CHECK_SCHEME_CONV(MakeSchemePoint(TPoint(0, 1)),
                      TValue(TPoint(0, 1)));

    CHECK_SCHEME_CONV(MakeSchemeRect(TRect(0, 1, 2, 3)),
                      TValue(TRect(0, 1, 2, 3)));
    
    CHECK_SCHEME_CONV(MakeSchemeColor(Color(0, 1, 2, 3)),
                      TValue(Color(0, 1, 2, 3)));

    // Test lists.
    CHECK_SCHEME_CONV(scheme_null, TValueList());
    TValueList list;
    list.push_back(1);
    list.push_back("foo");
    car = scheme_make_integer_value(1);
    cdr = scheme_make_pair(foo_str, scheme_null);
    CHECK_SCHEME_CONV(scheme_make_pair(car, cdr), list);

    // Test polygons.
    std::vector<TPoint> poly;
    poly.push_back(TPoint(0, 0));
    poly.push_back(TPoint(0, 10));
    poly.push_back(TPoint(10, 0));
    CHECK_SCHEME_CONV(MakeSchemePolygon(TPolygon(poly)),
                      TPolygon(poly));

    // Test percent.
    CHECK_SCHEME_CONV(MakeSchemePercent(TPercent(72.0)),
                      TPercent(72.0));
    
} END_TEST_CASE(TestSchemeToTValue);

#endif // BUILD_TEST_CASES


