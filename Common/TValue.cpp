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
#include "TestCase.h"
#include "TInterpreter.h"

using namespace Halyard;

REGISTER_TEST_CASE_FILE(TValue);


//=========================================================================
//  TSymbol and TPercent Methods
//=========================================================================

bool Halyard::operator==(const TSymbol &inS1, const TSymbol &inS2) {
    return inS1.GetName() == inS2.GetName();
}

bool Halyard::operator!=(const TSymbol &inS1, const TSymbol &inS2) {
    return !(inS1 == inS2);
}

std::ostream &Halyard::operator<<(std::ostream &out, const TSymbol &inSym) {
    out << "'" << inSym.GetName();
    return out;
}

bool Halyard::operator==(const TPercent &inP1, const TPercent &inP2) {
    return inP1.GetValue() == inP2.GetValue();
}

bool Halyard::operator!=(const TPercent &inP1, const TPercent &inP2) {
    return !(inP1 == inP2);
}

std::ostream &Halyard::operator<<(std::ostream &out, const TPercent &inPercent)
{
    out << inPercent.GetValue() << "%";
    return out;
}


//=========================================================================
//  TCallbackPtr Methods
//=========================================================================

std::ostream &Halyard::operator<<(std::ostream &out,
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

template <> TNull Halyard::tvalue_cast(const TValue &v)
    { TNull r; return v.Get(r, "a null value"); }
template <> std::string Halyard::tvalue_cast(const TValue &v)
    { std::string r; return v.Get(r, "a string"); }
template <> TSymbol Halyard::tvalue_cast(const TValue &v)
    { TSymbol r; return v.Get(r, "a symbol"); }
template <> bool Halyard::tvalue_cast(const TValue &v)
    { bool r; return v.Get(r, "a Boolean value"); }
template <> TPoint Halyard::tvalue_cast(const TValue &v)
    { TPoint r; return v.Get(r, "a point"); }
template <> TRect Halyard::tvalue_cast(const TValue &v)
    { TRect r; return v.Get(r, "a rectangle"); }
template <> GraphicsTools::Color Halyard::tvalue_cast(const TValue &v)
	{ GraphicsTools::Color r; return v.Get(r, "a color"); }
template <> TValueList Halyard::tvalue_cast(const TValue &v)
    { TValueList r; return v.Get(r, "a list"); }
template <> TPolygon Halyard::tvalue_cast(const TValue &v)
    { TPolygon r; return v.Get(r, "a polygon"); }
template <> TPercent Halyard::tvalue_cast(const TValue &v)
    { TPercent r; return v.Get(r, "a percent"); }

template <> int32 Halyard::tvalue_cast(const TValue &v) {
	int32 r; 
	// Convert to int32 if TValue is a unint32
	// but only if the unint32 is less than MAX_INT32.
	if (v.GetType() == TValue::TYPE_ULONG) {
		uint32 rUInt;
		rUInt = v.Get(rUInt, "<SHOULD NOT HAPPEN: int32>"); 
		if (rUInt <= MAX_INT32)
			return rUInt;
		THROW("Expected a signed 32-bit integer, got <" + 
              v.ToDisplayValue() + ">");
	}
	return v.Get(r, "a signed 32-bit integer");
}

template <> uint32 Halyard::tvalue_cast(const TValue &v) { 
	uint32 r;
	// Convert to uint32 if TValue is an int32
	// but only if the int32 is non-negative.
	if (v.GetType() == TValue::TYPE_LONG) {
		int32 rInt;
		rInt = v.Get(rInt, "<SHOULD NOT HAPPEN: uint32>");
		if (rInt >= 0)
			return rInt;
		THROW("Expected an unsigned 32-bit integer, got <" + 
              v.ToDisplayValue() + ">");
	}
	return v.Get(r, "an unsigned 32-bit integer");
}

template <> double Halyard::tvalue_cast(const TValue &v) {
	double r;
	// Convert to a double if TValue is an int32
	// or a unint32.
	if (v.GetType() == TValue::TYPE_LONG) {
		int32 rInt;
		return v.Get(rInt, "<SHOULD NOT HAPPEN: double>");
	}
	if (v.GetType() == TValue::TYPE_ULONG) {
		uint32 rUInt;
		return v.Get(rUInt, "<SHOULD NOT HAPPEN: double>");
	}
	return v.Get(r, "a floating-point number");
}

std::string TValue::ToDisplayValue() const {
    std::ostringstream out;
    out << *this;
    return out.str();
}

TCallbackPtr TValue::GetCallbackPtr() {
	TCallbackPtr ptr;
	return Get(ptr, "a callback");
}

TValue::Type TValue::GetType() const {
    if (!IsInitialized())
		THROW("Cannot get type of uninitialized TValue");
    return mPtr->GetType();
}

bool Halyard::operator==(const TValue &inV1, const TValue &inV2) {
    // Check for uninitialized values.
    if (!inV1.IsInitialized() || !inV2.IsInitialized())
        THROW("Cannot compare uninitialized TValue");
    
    // If the Impl classes aren't the same, the values can't be equal.
    if (typeid(*inV1.mPtr.get()) != typeid(*inV2.mPtr.get()))
        return false;

    // Delegate the comparison to our implementation class.
    return inV1.mPtr->Equals(inV2.mPtr.get());
}

bool Halyard::operator!=(const TValue &inV1, const TValue &inV2) {
    return !(inV1 == inV2);
}

std::ostream &Halyard::operator<<(std::ostream &out, const TValue &inV) {
    if (!inV.IsInitialized())
        out << "#<TValue: uninitialized>";
    else
        inV.mPtr->Write(out);
    return out;
}

std::ostream &Halyard::operator<<(std::ostream &out, const TValueList &l) {
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
    CHECK_EQ(tvalue_cast<Type>(value), inVal);
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

    // Check Null.  We use assignment notation here, because the constructor
    // notation would be mistaken for a function declaration.  Strange.
	TValue value = TNull();
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
	double foo;
	CHECK_THROWN(std::exception, foo = tvalue_cast<double>(TValue()));
    CHECK_THROWN(std::exception, tvalue_cast<double>(TValue("foo")));

    // operator== special cases
    CHECK_THROWN(std::exception, TValue() == TValue());
    CHECK_THROWN(std::exception, TValue() == TValue(10));
    CHECK_THROWN(std::exception, TValue(10) == TValue());
    CHECK_NE(TValue(10), TValue(10.0));
    CHECK_NE(TValue(10.0), TValue(10));
    CHECK_NE(TValue(1), TValue(true));
    CHECK_NE(TValue(0), TValue(false));

	// TValue autoconversions.
	CHECK_EQ(tvalue_cast<uint32>(TValue(MAX_INT32)), uint32(MAX_INT32));
	CHECK_THROWN(std::exception, tvalue_cast<uint32>(TValue(-1)));
	CHECK_EQ(tvalue_cast<int32>(TValue(uint32(MAX_INT32))), MAX_INT32);
	CHECK_THROWN(std::exception,
                 tvalue_cast<int32>(TValue(uint32(MAX_INT32) + 1)));
    CHECK_EQ(tvalue_cast<double>(TValue(1)), 1.0); 
	CHECK_EQ(tvalue_cast<double>(TValue(MAX_UINT32)), double(MAX_UINT32)); 
	CHECK_EQ(tvalue_cast<double>(TValue(MIN_INT32)), double(MIN_INT32)); 
	CHECK_EQ(tvalue_cast<double>(TValue(-1)), -1.0);
	
} END_TEST_CASE(TestTValue);

BEGIN_TEST_CASE(TestTValueTypeChecks, TestCase) {
    CHECK_THROWN_MESSAGE(std::exception, "Expected a string, got <32>",
                         tvalue_cast<std::string>(TValue(32)));
    CHECK_THROWN_MESSAGE(std::exception, "Expected a symbol, got <hello>",
                         tvalue_cast<TSymbol>(TValue("hello")));
    CHECK_THROWN_MESSAGE(std::exception,
                         "Expected a signed 32-bit integer, got <2147483648>",
                         tvalue_cast<int32>(TValue(uint32(2147483648U))));
    CHECK_THROWN_MESSAGE(std::exception,
                         "Expected an unsigned 32-bit integer, got <-1>",
                         tvalue_cast<uint32>(TValue(-1)));
    
} END_TEST_CASE(TestTValueTypeChecks);

#endif // BUILD_TEST_CASES
