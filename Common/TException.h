// -*- Mode: C++; tab-width: 4; -*-

#ifndef TException_H
#define TException_H

#include <stdexcept>
#include <string>

#include "TCommon.h"

BEGIN_NAMESPACE_FIVEL

//////////
// A handy exception class for use in 5L code.  Don't bother
// catching this directly; catch something like std::exception
// instead.
// 
class TException : public std::runtime_error
{
	std::string mErrorFile;
	int mErrorLine;
	std::string mErrorMessage;
	int mErrorCode;
	std::string mWhatCache;
	
protected:
	//////////
	// Constructor for use by subclasses.  You can call this, then set up
	// the member variables with SetErrorCode and SetErrorMessage.
	//
	TException(const char *inErrorFile, int inErrorLine)
		: std::runtime_error(""),
		  mErrorFile(inErrorFile),
		  mErrorLine(inErrorLine),
		  mErrorCode(kNoErrorCode) {}

	//////////
	// Set the error code associated with this exception.
	//
	void SetErrorCode(int inErrorCode) { mErrorCode = inErrorCode; }

	//////////
	// Set the error message associated with this exception.
	//
	void SetErrorMessage(const std::string &inErrorMessage)
	    { mErrorMessage = inErrorMessage; }

public:
	enum {
		//////////
		// Not all errors have error codes.  Use this if you don't
		// have anything better.
		//
		kNoErrorCode = 0
	};

	//////////
	// Create a new TException object.
	//
	// [in] inErrorMessage - The error message to display.
	// [in] inErrorCode - The code associated with this error, if any.
	//
	TException(const char *inErrorFile, int inErrorLine,
			   const std::string &inErrorMessage,
			   int inErrorCode = kNoErrorCode)
		: std::runtime_error(""),
		  mErrorFile(inErrorFile),
		  mErrorLine(inErrorLine),
		  mErrorMessage(inErrorMessage),
		  mErrorCode(inErrorCode) {}

	//////////
	// Destroy a TException object.
	//
	virtual ~TException() throw () {}

	//////////
	// Return the name of this exception class.  Subclasses must
	// override this to provide their own name.
	//
    virtual const char *GetClassName() const { return "TException"; }

	//////////
	// Return the error code associated with this exception, or
	// kNoErrorCode if there is none.
	//
	int GetErrorCode() const { return mErrorCode; }
	
	//////////
	// Return the error message associated with this exception.  You
	// don't usually wish to call this; call what() instead.
	//
	std::string GetErrorMessage() const { return mErrorMessage; }
	
	//////////
	// Return a formatted error string for this error.  This method is
	// available on all instances of std::exception.
	//
	// XXX - THIS ROUTINE VIOLATES CONST-CORRECTNESS TO UPDATE THE INTERNAL
	// mWhatCache VARIABLE.  This nasty wart is forced upon us by the fact
	// that std::exception declares what() to return a const char*, not a
	// proper string.  We could fix this by caching values in our Set*
	// methods.
	//
	// [out] return - A pointer to an error string.  This string
	//                is good until the TException is destroyed,
	//                or what() is called a second time, whichever
	//                comes sooner.
	//
	virtual const char* what () const throw ();
};

END_NAMESPACE_FIVEL

#endif // TException_H
