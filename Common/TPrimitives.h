// -*- Mode: C++; tab-width: 4; -*-

#ifndef TPrimitives_H
#define TPrimitives_H

#include <string>

#include "TCommon.h"
#include "TString.h"
#include "TPoint.h"
#include "TRect.h"
#include "GraphicsTools.h"
#include "TInterpreter.h"

BEGIN_NAMESPACE_FIVEL

// These are the primitive types which can currently be passed as
// arguments to a 5L command.
//
// String
//   TString
//   std::string
//
// Integer
//   int16
//   int32
//   uint32
//   double
// 
// Structures
//   Point
//   Rectangle
//   Color
//
// Manipulators
//   open - scan forward for an open parentheses
//   close - scan forward for a close parentheses, skipping nested pairs
//   discard - discard the next token
//   ValueOrPercent - Read in a value, or a percentage of another value
//
// Implicit
//   thunk - a zero-argument callback

//////////
// TArgumentList provides an abstract interface to the argument lists
// passed to a 5L primitive function.  To allow a new TInterpreter
// class to call 5L primitives, you'll need to implement all this
// class's virtual methods.
//
// You have a lot of flexibility in how you implement the Get* methods.
// For example, these methods might automatically coerce arguments to
// the correct data type on the fly.
//
class TArgumentList
{
protected:
	//////////
	// Return the next argument as a string.
	//
	virtual std::string GetStringArg() = 0;

	//////////
	// Return the next argument as a singed, 32-bit integer.
	//
	virtual int32 GetInt32Arg() = 0;

	//////////
	// Return the next argument as an unsinged, 32-bit integer.
	//
	virtual uint32 GetUInt32Arg() = 0;

	//////////
	// Return the next argument as a double.
	//
	virtual double GetDoubleArg() = 0;

	//////////
	// Return the next argument as a point.
	//
	virtual TPoint GetPointArg() = 0;

	//////////
	// Return the next argument as a rectangle.
	//
	virtual TRect GetRectArg() = 0;

	//////////
	// Return the next argument as a color.
	//
	virtual GraphicsTools::Color GetColorArg() = 0;

	//////////
	// Return the next argument as a percent.
	//
	//virtual int32 GetPercentArg() = 0;

	//////////
	// Return the next argument as a callback.  This object
	// is allocated on the heap, and must be destroyed by the
	// caller (typically the primitive function) using delete.
	//
	virtual TCallback *GetCallbackArg() = 0;

public:
	//////////
	// Are there any more arguments left?
	//
	virtual bool HasMoreArguments() = 0;

	// These functions provide handy wrapper functions
	// for the protected Get* functions above.
	friend TArgumentList &operator>>(TArgumentList &args, TString &out);
	friend TArgumentList &operator>>(TArgumentList &args, std::string &out);
	friend TArgumentList &operator>>(TArgumentList &args, int16 &out);
	friend TArgumentList &operator>>(TArgumentList &args, int32 &out);
	friend TArgumentList &operator>>(TArgumentList &args, uint32 &out);
	friend TArgumentList &operator>>(TArgumentList &args, double &out);
	friend TArgumentList &operator>>(TArgumentList &args, TRect &out);
	friend TArgumentList &operator>>(TArgumentList &args, TPoint &out);
	friend TArgumentList &operator>>(TArgumentList &args,
									 GraphicsTools::Color &out);
	friend TArgumentList &operator>>(TArgumentList &args, TCallback* &out);

	// TODO - Handle the ValueOrPercent manipulator here.
};

END_NAMESPACE_FIVEL

#endif // TPrimitives_H
