// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TestCase.h"
#include "TInterpreter.h"

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(TValue);


//=========================================================================
//  TSymbol and TPercent Methods
//=========================================================================

bool FIVEL_NS operator==(const TSymbol &inS1, const TSymbol &inS2) {
    return inS1.GetName() == inS2.GetName();
}

bool FIVEL_NS operator!=(const TSymbol &inS1, const TSymbol &inS2) {
    return !(inS1 == inS2);
}

std::ostream &FIVEL_NS operator<<(std::ostream &out, const TSymbol &inSym) {
    out << "'" << inSym.GetName();
    return out;
}

bool FIVEL_NS operator==(const TPercent &inP1, const TPercent &inP2) {
    return inP1.GetValue() == inP2.GetValue();
}

bool FIVEL_NS operator!=(const TPercent &inP1, const TPercent &inP2) {
    return !(inP1 == inP2);
}

std::ostream &FIVEL_NS operator<<(std::ostream &out, const TPercent &inPercent)
{
    out << inPercent.GetValue() << "%";
    return out;
}


//=========================================================================
//  TCallbackPtr Methods
//=========================================================================

std::ostream &FIVEL_NS operator<<(std::ostream &out,
								  const TCallbackPtr &inCallback)
{
	out << inCallback->PrintableRepresentation();
	return out;
}


//=========================================================================
//  TValue Methods
//=========================================================================

TValue::TValue(const TNull &inValue)
    : mPtr(new TemplateImpl<TNull>(TNull())) {}
TValue::TValue(int inValue)
    : mPtr(new TemplateImpl<int32>(inValue)) {}
TValue::TValue(int32 inValue)
    : mPtr(new TemplateImpl<int32>(inValue)) {}
TValue::TValue(uint32 inValue)
    : mPtr(new TemplateImpl<uint32>(inValue)) {}
TValue::TValue(double inValue)
    : mPtr(new TemplateImpl<double>(inValue)) {}
TValue::TValue(bool inValue)
    : mPtr(new TemplateImpl<bool>(inValue)) {}
TValue::TValue(const TPoint &inValue)
    : mPtr(new TemplateImpl<TPoint>(inValue)) {}
TValue::TValue(const TRect &inValue)
    : mPtr(new TemplateImpl<TRect>(inValue)) {}
TValue::TValue(const GraphicsTools::Color &inValue)
    : mPtr(new TemplateImpl<GraphicsTools::Color>(inValue)) {}
TValue::TValue(const std::string &inValue)
    : mPtr(new TemplateImpl<std::string>(inValue)) {}
TValue::TValue(const TSymbol &inValue)
    : mPtr(new TemplateImpl<TSymbol>(inValue)) {}
TValue::TValue(const TPercent &inValue)
    : mPtr(new TemplateImpl<TPercent>(inValue)) {}
TValue::TValue(const char *inValue)
    : mPtr(new TemplateImpl<std::string>(inValue)) {}
TValue::TValue(const TPolygon &inValue)
    : mPtr(new TemplateImpl<TPolygon>(inValue)) {}
TValue::TValue(const TValueList &inValue)
    : mPtr(new TemplateImpl<TValueList>(inValue)) {}
TValue::TValue(const TCallbackPtr &inValue)
    : mPtr(new TemplateImpl<TCallbackPtr>(inValue)) {}

TValue::operator TNull() const { TNull r; return Get(r); }
TValue::operator std::string() const { std::string r; return Get(r); }
TValue::operator TSymbol() const { TSymbol r; return Get(r); }
TValue::operator bool() const { bool r; return Get(r); }
TValue::operator TPoint() const { TPoint r; return Get(r); }
TValue::operator TRect() const { TRect r; return Get(r); }
TValue::operator GraphicsTools::Color() const
	{ GraphicsTools::Color r; return Get(r); }
TValue::operator const TValueList &() const { TValueList r; return Get(r); }
TValue::operator TPolygon() const { TPolygon r; return Get(r); }
TValue::operator TPercent() const { TPercent r; return Get(r); }

TValue::operator int32() const { 
	int32 r; 
	// Convert to int32 if TValue is a unint32
	// but only if the unint32 is less than MAX_INT32.
	if (GetType() == TYPE_ULONG) {
		uint32 rUInt;
		rUInt = Get(rUInt); 
		if (rUInt <= MAX_INT32)
			return rUInt;
		THROW("Type mismatch fetching TValue"); 
	}
	return Get(r); 
}

TValue::operator uint32() const { 
	uint32 r;
	// Convert to uint32 if TValue is an int32
	// but only if the int32 is non-negative.
	if (GetType() == TValue::TYPE_LONG) {
		int32 rInt;
		rInt = Get(rInt); 
		if (rInt >= 0)
			return rInt;
		THROW("Type mismatch fetching TValue"); 
	}
	return Get(r); 
}

TValue::operator double() const {
	double r;
	// Convert to a double if TValue is an int32
	// or a unint32.
	if (GetType() == TValue::TYPE_LONG) {
		int32 rInt;
		return Get(rInt);
	}
	if (GetType() == TValue::TYPE_ULONG) {
		uint32 rUInt;
		return Get(rUInt);
	}
	return Get(r); 
}

TCallbackPtr TValue::GetCallbackPtr() {
	TCallbackPtr ptr;
	return Get(ptr);
}

TValue::Type TValue::GetType() const {
    if (!IsInitialized())
		THROW("Cannot get type of uninitialized TValue");
    return mPtr->GetType();
}

bool FIVEL_NS operator==(const TValue &inV1, const TValue &inV2) {
    // Check for uninitialized values.
    if (!inV1.IsInitialized() || !inV2.IsInitialized())
        THROW("Cannot compare uninitialized TValue");
    
    // If the Impl classes aren't the same, the values can't be equal.
    if (typeid(*inV1.mPtr.get()) != typeid(*inV2.mPtr.get()))
        return false;

    // Delegate the comparison to our implementation class.
    return inV1.mPtr->Equals(inV2.mPtr.get());
}

bool FIVEL_NS operator!=(const TValue &inV1, const TValue &inV2) {
    return !(inV1 == inV2);
}

std::ostream &FIVEL_NS operator<<(std::ostream &out, const TValue &inV) {
    if (!inV.IsInitialized())
        out << "#<TValue: uninitialized>";
    else
        inV.mPtr->Write(out);
    return out;
}

std::ostream &FIVEL_NS operator<<(std::ostream &out, const TValueList &l) {
	out << "(list";
	TValueList::const_iterator i = l.begin();
	for (; i != l.end(); ++i)
		out << " " << *i;
    out << ")";
	return out;
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

template <typename Type>
void CHECK_TVALUE_TYPE(TValue::Type inType, const Type &v1, const Type &v2) {
    TValue value(v1);
    CHECK_EQ(value.IsInitialized(), true);
    CHECK_EQ(value.GetType(), inType);
    CHECK_EQ(value, TValue(v1));
    CHECK_NE(value, TValue(v2));
	
	// Check to make sure this runs without crashing or infinitely
	// recursing.  We don't care what it actually returns.
	std::ostringstream out;
	out << value;
}

template <typename Type>
void CHECK_TVALUE_GET(const Type &inVal) {
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
    CHECK_THROWN(std::exception, double(TValue()));
    CHECK_THROWN(std::exception, double(TValue("foo")));

    // operator== special cases
    CHECK_THROWN(std::exception, TValue() == TValue());
    CHECK_THROWN(std::exception, TValue() == TValue(10));
    CHECK_THROWN(std::exception, TValue(10) == TValue());
    CHECK_NE(TValue(10), TValue(10.0));
    CHECK_NE(TValue(10.0), TValue(10));
    CHECK_NE(TValue(1), TValue(true));
    CHECK_NE(TValue(0), TValue(false));

	// TValue autoconversions.
	CHECK_EQ(uint32(TValue(MAX_INT32)), uint32(MAX_INT32));
	CHECK_THROWN(std::exception, uint32(TValue(-1)));
	CHECK_EQ(int32(TValue(uint32(MAX_INT32))), MAX_INT32);
	CHECK_THROWN(std::exception, int32(TValue(uint32(MAX_INT32) + 1)));
    CHECK_EQ(double(TValue(1)), 1.0); 
	CHECK_EQ(double(TValue(MAX_UINT32)), double(MAX_UINT32)); 
	CHECK_EQ(double(TValue(MIN_INT32)), double(MIN_INT32)); 
	CHECK_EQ(double(TValue(-1)), -1.0); 
	
} END_TEST_CASE(TestTValue);

#endif // BUILD_TEST_CASES
