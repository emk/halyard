#include "TString.h"
#include "ImlUnit.h"

USING_NAMESPACE_FIVEL

extern void test_TString (void);

void test_TString (void) 
{
	
	// Test conversions from integers to strings.
	// The large values below are the maximum and minimum values
	// allowed for each type.  We're careful to never to type a
	// literal value larger than INT_MAX into the source code to
	// avoid confusing dumb compilers.
	TString s1;
	s1 = (int32) 2147483647;
	TEST(s1 == "2147483647");
	s1 = (int32) (-2147483647-1);
	TEST(s1 == "-2147483648");
	s1 = (uint32) (2147483647 * 2U + 1);
	TEST(s1 == "4294967295");
	s1 = (int16) 32767;
	TEST(s1 == "32767");
	s1 = (int16) -32768;
	TEST(s1 == "-32768");
	s1 = (uint16) 65535;
	TEST(s1 == "65535");
	s1 = "Foo: ";
	s1 += (int32) 2147483647;
	TEST(s1 == "Foo: 2147483647");
	s1 = "Foo: ";
	s1 += (int32) (-2147483647-1);
	TEST(s1 == "Foo: -2147483648");

	// Test conversions from doubles to strings.
	// 10e+308 is the maximum IEEE double, according to Dave Peticolas,
	// but some C libraries round it to positive infinity.  So we check
	// 10e+307 instead, in hopes of flushing out any snprintf buffer
	// overflows.
	TString s2;
	s2 = (double) 0.0;
	TEST(s2 == "0.000000");
	s2 = (double) 1.5;
	TEST(s2 == "1.500000");
	s2 = (double) -1.5;
	TEST(s2 == "-1.500000");
	s2 = (double) 10e+307;
	TEST(s2.Length() == 316); // '10' + 307 digits + '.' + six digits

	// Test conversion to uppercase and lowercase.
	TString s3;
	s3 = "aBcD123";
	s3.MakeLower();
	TEST(s3 == "abcd123");
	s3 = "aBcD123";
	s3.MakeUpper();
	TEST(s3 == "ABCD123");
	s3 = "";
	s3.MakeLower();
	TEST(s3 == "");

	// Test case-insensitive string comparison.
	TEST(TString("").Compare("a", false)   < 0);
	TEST(TString("a").Compare("", false)   > 0);
	TEST(TString("a").Compare("A", false)  == 0);
	TEST(TString("a").Compare("B", false)  < 0);
	TEST(TString("C").Compare("b", false)  > 0);
	TEST(TString("a").Compare("AA", false) < 0);
}
