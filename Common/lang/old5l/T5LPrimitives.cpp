// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for Register5LPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "T5LPrimitives.h"

// Need to implement the primitives.
#include <string>
#include <memory>
#include "TCommonPrimitives.h"
#include "TVariable.h"
#include "TException.h"
#include "TStream.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Register5LPrimitives
//=========================================================================
//  Install our portable primitive functions.  These are only required by
//  our old 5L interpreter, not by more reasonable languages.

void FIVEL_NS Register5LPrimitives()
{
	// Declarations.
	REGISTER_5L_PRIMITIVE(DefPalette);

	// Integer operations.
	REGISTER_5L_PRIMITIVE(Add);
	REGISTER_5L_PRIMITIVE(Sub);
    REGISTER_5L_PRIMITIVE(Div);
	REGISTER_5L_PRIMITIVE_WITH_NAME("+", Plus);
	REGISTER_5L_PRIMITIVE_WITH_NAME("-", Minus);
	REGISTER_5L_PRIMITIVE_WITH_NAME("*", Times);
	REGISTER_5L_PRIMITIVE_WITH_NAME("/", Divide);
	REGISTER_5L_PRIMITIVE_WITH_NAME("%", Modulo);

	// Floating-point operations.
	REGISTER_5L_PRIMITIVE(Truncate);
	REGISTER_5L_PRIMITIVE_WITH_NAME("float+", FloatPlus);
	REGISTER_5L_PRIMITIVE_WITH_NAME("float-", FloatMinus);
	REGISTER_5L_PRIMITIVE_WITH_NAME("float*", FloatTimes);
	REGISTER_5L_PRIMITIVE_WITH_NAME("float/", FloatDivide);

	// String operations.
	REGISTER_5L_PRIMITIVE(strlen);
	REGISTER_5L_PRIMITIVE(substr);
	REGISTER_5L_PRIMITIVE(FindSubStr);
	REGISTER_5L_PRIMITIVE(contains);

	// List operations.
	REGISTER_5L_PRIMITIVE(Length);
	REGISTER_5L_PRIMITIVE(nth);

	// Associative list operations.
	REGISTER_5L_PRIMITIVE(HasKey);
	REGISTER_5L_PRIMITIVE(GetVal);

	// Logical operators.
	REGISTER_5L_PRIMITIVE(And);
	REGISTER_5L_PRIMITIVE(Or);
	REGISTER_5L_PRIMITIVE(Not);

	// Comparison operators.
	REGISTER_5L_PRIMITIVE_WITH_NAME("=", Equals);
	REGISTER_5L_PRIMITIVE_WITH_NAME("<>", NotEquals);
	REGISTER_5L_PRIMITIVE_WITH_NAME("<", LessThan);
	REGISTER_5L_PRIMITIVE_WITH_NAME(">", GreaterThan);
	REGISTER_5L_PRIMITIVE_WITH_NAME("<=", LessThanOrEqualTo);
	REGISTER_5L_PRIMITIVE_WITH_NAME(">=", GreaterThanOrEqualTo);
}


//=========================================================================
//  Declarations
//=========================================================================
//  Top-level defining forms.  You probably want to register these as
//  as top-level form processors, too (see the interpreters for details).

//-------------------------------------------------------------------------
// (DefPalette [INDEX COLOR]...)
//-------------------------------------------------------------------------
// Map 8-bit palette indices to RGB colors, for use with the drawing
// commands.  This will allow us to modify all remaining primitives to
// accept RGB colors, and to dispose of the palette and CLUT code in
// both engines.

DEFINE_5L_PRIMITIVE(DefPalette)
{
	// Clear the entire palette to black.
	for (int i = 0; i < TSTREAM_PALETTE_SIZE; i++)
		gPalette[i] = GraphicsTools::Color(0, 0, 0);

	// Read in the INDEX/COLOR pairs and enter them into the palette.
	while (inArgs.HasMoreArguments())
	{
		// Get the index.
		int32 index;
		inArgs >> index;
		if (index < 0 || TSTREAM_PALETTE_SIZE <= index)
			gLog.FatalError("DefPalette: Index %d out of bounds", index);

		// Get the color.
		GraphicsTools::Color color;
		if (!inArgs.HasMoreArguments())
			gLog.FatalError("DefPalette: Uneven number of INDEX/COLOR pairs");
		inArgs >> color;

		// Update the palette.
		gPalette[index] = color;
	}
}


//=========================================================================
//  Integer Operations
//=========================================================================

//-------------------------------------------------------------------------
// (ADD VARIABLE AMOUNT)
//-------------------------------------------------------------------------
// Adds the given amount to the given variable.

DEFINE_5L_PRIMITIVE(Add)
{
    TString vname;
	int32 amount;

    inArgs >> vname >> amount;

	int32 sum = gVariableManager.GetLong(vname);
	sum += amount;
	gVariableManager.SetLong(vname, sum);
	::SetPrimitiveResult(sum);
}

//-------------------------------------------------------------------------
// (SUB VARIABLE AMOUNT)
//-------------------------------------------------------------------------
// Subtracts the given amount to the given variable.

DEFINE_5L_PRIMITIVE(Sub)
{
    TString vname;
	int32 amount;

    inArgs >> vname >> amount;

	int32 sum = gVariableManager.GetLong(vname);
	sum -= amount;
	gVariableManager.SetLong(vname, sum);
	::SetPrimitiveResult(sum);
}

//-------------------------------------------------------------------------
// (DIV INT INT)
//-------------------------------------------------------------------------
// Performs integer division on two numbers.

DEFINE_5L_PRIMITIVE(Div)
{
	TString vname;
	int32 Divisor = 0;
	int32 Dividend;

	inArgs >> vname >> Divisor;
	Dividend = gVariableManager.GetLong(vname);

	if (Divisor == 0)
		gLog.Caution("Error: Division by zero: %s <%d> / <%d>.",
					 (const char *) vname, Divisor, Dividend);

	Dividend = Dividend / Divisor;
	gVariableManager.SetLong(vname, Dividend);
	::SetPrimitiveResult(Dividend);
}

//-------------------------------------------------------------------------
// (+ INT...)
//-------------------------------------------------------------------------
// Add a series of numbers.

DEFINE_5L_PRIMITIVE(Plus)
{
	int32 result = 0;
	while (inArgs.HasMoreArguments())
	{
		int32 arg;
		inArgs >> arg;
		result += arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (- INT [INT])
//-------------------------------------------------------------------------
// Negate a single number, or subtract one number from another.

DEFINE_5L_PRIMITIVE(Minus)
{
	int32 arg1, arg2;
	inArgs >> arg1;
	if (inArgs.HasMoreArguments())
	{
		inArgs >> arg2;
		::SetPrimitiveResult(arg1 - arg2);
	}
	else
	{
		::SetPrimitiveResult(-arg1);
	}
}


//-------------------------------------------------------------------------
// (* INT...)
//-------------------------------------------------------------------------
// Multiply a series of numbers together.

DEFINE_5L_PRIMITIVE(Times)
{
	int32 result = 1;
	while (inArgs.HasMoreArguments())
	{
		int32 arg;
		inArgs >> arg;
		result *= arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (/ INT INT)
//-------------------------------------------------------------------------
// Divide one number by another.

DEFINE_5L_PRIMITIVE(Divide)
{
	int32 arg1, arg2;
	inArgs >> arg1 >> arg2;
	::SetPrimitiveResult(arg1 / arg2);
}


//-------------------------------------------------------------------------
// (% INT INT)
//-------------------------------------------------------------------------
// Get the value of one number modulo another.

DEFINE_5L_PRIMITIVE(Modulo)
{
	int32 arg1, arg2;
	inArgs >> arg1 >> arg2;
	::SetPrimitiveResult(arg1 % arg2);
}


//=========================================================================
//  Floating-Point Operations
//=========================================================================

//-------------------------------------------------------------------------
// (truncate FLOAT)
//-------------------------------------------------------------------------
// Convert a float to an int.

DEFINE_5L_PRIMITIVE(Truncate)
{
	double arg1;
	inArgs >> arg1;
	::SetPrimitiveResult((int32) arg1);
}


//-------------------------------------------------------------------------
// (float+ FLOAT...)
//-------------------------------------------------------------------------
// Add a series of numbers.

DEFINE_5L_PRIMITIVE(FloatPlus)
{
	double result = 0;
	while (inArgs.HasMoreArguments())
	{
		double arg;
		inArgs >> arg;
		result += arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (float- FLOAT [FLOAT])
//-------------------------------------------------------------------------
// Negate a single number, or subtract one number from another.

DEFINE_5L_PRIMITIVE(FloatMinus)
{
	double arg1, arg2;
	inArgs >> arg1;
	if (inArgs.HasMoreArguments())
	{
		inArgs >> arg2;
		::SetPrimitiveResult(arg1 - arg2);
	}
	else
	{
		::SetPrimitiveResult(-arg1);
	}
}


//-------------------------------------------------------------------------
// (float* FLOAT...)
//-------------------------------------------------------------------------
// Multiply a series of numbers together.

DEFINE_5L_PRIMITIVE(FloatTimes)
{
	double result = 1;
	while (inArgs.HasMoreArguments())
	{
		double arg;
		inArgs >> arg;
		result *= arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (float/ FLOAT FLOAT)
//-------------------------------------------------------------------------
// Divide one number by another.

DEFINE_5L_PRIMITIVE(FloatDivide)
{
	double arg1, arg2;
	inArgs >> arg1 >> arg2;
	::SetPrimitiveResult(arg1 / arg2);
}


//=========================================================================
// String Operations
//=========================================================================

//-------------------------------------------------------------------------
// (strlen STRING)
//-------------------------------------------------------------------------
// Return the number of characters in a string.

DEFINE_5L_PRIMITIVE(strlen)
{
	std::string arg;
	inArgs >> arg;
	::SetPrimitiveResult((int32) arg.length());
}


//-------------------------------------------------------------------------
// (substr STRING INT INT)
//-------------------------------------------------------------------------
// Return the portion of the string starting with the first INT, and
// containing the number of characters specified by the second INT.

DEFINE_5L_PRIMITIVE(substr)
{
	std::string str;
	int32 begin, count;

	// Parse our arguments.
	inArgs >> str >> begin >> count;

	// Get the substring and return it.
	::SetPrimitiveResult(str.substr(begin, count).c_str());
}


//-------------------------------------------------------------------------
// (FindSubStr STRING STRING)
//-------------------------------------------------------------------------
// Return the position of the second STRING in the first, or -1 if the
// second STRING does not appear in the first.

DEFINE_5L_PRIMITIVE(FindSubStr)
{
	std::string str, substr;
	inArgs >> str >> substr;

	std::string::size_type pos = str.find(substr);
	if (pos == std::string::npos)
		::SetPrimitiveResult((int32) -1);
	else
		::SetPrimitiveResult((int32) pos);
}


//-------------------------------------------------------------------------
// (contains STRING STRING)
//-------------------------------------------------------------------------
// Returns true if and only if the first string contains the second.

DEFINE_5L_PRIMITIVE(contains)
{
	std::string str, substr;
	inArgs >> str >> substr;

	std::string::size_type pos = str.find(substr);
	::SetPrimitiveResult((int32) (pos == std::string::npos ? 0 : 1));
}


//=========================================================================
// List Operations
//=========================================================================

//-------------------------------------------------------------------------
// (Length LIST)
//-------------------------------------------------------------------------
// Return the number of items in LIST

DEFINE_5L_PRIMITIVE(Length)
{
	TArgumentList *list_temp;

	inArgs >> list_temp;
	std::auto_ptr<TArgumentList> list(list_temp);

	int32 count = 0;
	while (list->HasMoreArguments())
	{
		TString junk;
		*list >> junk;
		++count;
	}

	::SetPrimitiveResult(count);
}


//-------------------------------------------------------------------------
// (nth INT LIST)
//-------------------------------------------------------------------------
// Return the nth value in the list, counting from zero.

DEFINE_5L_PRIMITIVE(nth)
{
	int32 n;
	TArgumentList *list_temp;

	inArgs >> n >> list_temp;
	std::auto_ptr<TArgumentList> list(list_temp);

	// Find the appropriate element.
	std::string result;
	for (int i = 0; i <= n; i++)
	{
		if (!list->HasMoreArguments())
			throw TException(__FILE__, __LINE__,
							 "Element: List is not that long");
		*list >> result;
	}
	::SetPrimitiveResult(result.c_str());
}


//=========================================================================
// Associative List Operations
//=========================================================================
// An associative list stores a sequence of key-value pairs:
//
//   (foo 1 bar 2 baz 3)
//
// The first item in each pair is the "key", the second is the "value".
// If a key appears more than once, the first value is used.

//-------------------------------------------------------------------------
// (HasKey LIST STRING)
//-------------------------------------------------------------------------
// Return 1 if STRING appears as a key anywhere in LIST.

DEFINE_5L_PRIMITIVE(HasKey)
{
	TArgumentList *list_temp;
	std::string key, k, v;

	inArgs >> list_temp >> key;
	std::auto_ptr<TArgumentList> list(list_temp);

	while (list->HasMoreArguments())
	{
		*list >> k;
		if (!list->HasMoreArguments())
			throw TException(__FILE__, __LINE__,
							 "HasKey: Odd number of items in list");
		*list >> v;

		if (key == k)
		{
			::SetPrimitiveResult((int32) 1);
			return;
		}
	}

	::SetPrimitiveResult((int32) 0);
}


//-------------------------------------------------------------------------
// (GetVal LIST STRING)
//-------------------------------------------------------------------------
// Return the value in LIST corresponding to the key STRING.

DEFINE_5L_PRIMITIVE(GetVal)
{
	TArgumentList *list_temp;
	std::string key, k, v;

	inArgs >> list_temp >> key;
	std::auto_ptr<TArgumentList> list(list_temp);

	while (list->HasMoreArguments())
	{
		*list >> k;
		if (!list->HasMoreArguments())
			throw TException(__FILE__, __LINE__,
							 "HasKey: Odd number of items in list");
		*list >> v;

		if (key == k)
		{
			::SetPrimitiveResult(v.c_str());
			return;
		}
	}

	throw TException(__FILE__, __LINE__, "HasKey: No such key");
}


//=========================================================================
// Logical Operators
//=========================================================================

//-------------------------------------------------------------------------
// (And BOOL...)
//-------------------------------------------------------------------------
// Returns the logical AND of the arguments.

DEFINE_5L_PRIMITIVE(And)
{
	bool result = true;
	while (inArgs.HasMoreArguments())
	{
		bool arg;
		inArgs >> arg;
		result = result && arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (Or BOOL...)
//-------------------------------------------------------------------------
// Returns the logical OR of the arguments.

DEFINE_5L_PRIMITIVE(Or)
{
	bool result = false;
	while (inArgs.HasMoreArguments())
	{
		bool arg;
		inArgs >> arg;
		result = result || arg;
	}
	::SetPrimitiveResult(result);
}


//-------------------------------------------------------------------------
// (Not BOOL...)
//-------------------------------------------------------------------------
// Returns the logical negation of the argument.

DEFINE_5L_PRIMITIVE(Not)
{
	bool arg;
	inArgs >> arg;
	::SetPrimitiveResult(arg ? false : true);
}


//=========================================================================
// Comparison Operators
//=========================================================================

//-------------------------------------------------------------------------
// (== STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(Equals)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result == 0 ? true : false);
}


//-------------------------------------------------------------------------
// (!= STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(NotEquals)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result != 0 ? true : false);
}


//-------------------------------------------------------------------------
// (< STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(LessThan)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result < 0 ? true : false);
}


//-------------------------------------------------------------------------
// (> STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(GreaterThan)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result > 0 ? true : false);
}


//-------------------------------------------------------------------------
// (<= STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(LessThanOrEqualTo)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result <= 0 ? true : false);
}


//-------------------------------------------------------------------------
// (>= STRING STRING)
//-------------------------------------------------------------------------
// Compares two values (handles both strings and integers).

DEFINE_5L_PRIMITIVE(GreaterThanOrEqualTo)
{
	TString str1, str2;
	inArgs >> str1 >> str2;
	int result = str1.TypeCompare(str2);
	::SetPrimitiveResult(result >= 0 ? true : false);
}
