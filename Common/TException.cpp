// -*- Mode: C++; tab-width: 4; -*-

#include <strstream>

#include "TLogger.h"
#include "TException.h"

USING_NAMESPACE_FIVEL

static std::string get_string(std::ostrstream &stream)
{
	// Go through the foolish new rigamarole for extracting a string.
	// We must unfreeze the stream before we exit this function, or
	// we'll leak memory.
	//
	// TODO - Major candidate for refactoring, but where should it live?
	// There's already a duplicate copy in TypographyTests.cpp.
	stream.freeze(1);
	try
	{
		std::string str(stream.str(), stream.pcount());
		stream.freeze(0);
		return str;
	}
	catch (...)
	{
		stream.freeze(0);
		throw;
	}
	
	ASSERT(false);
	return std::string("");
}

const char* TException::what () const throw ()
{
	std::ostrstream s;
	s << GetClassName() << ": " << GetErrorMessage() << " (";
	if (GetErrorCode() != kNoErrorCode)
	{
		s << "error " << GetErrorCode() << " ";
	}
	s << "at " << mErrorFile << ":" << mErrorLine << ")";
	const_cast<TException*>(this)->mWhatCache = get_string(s);
	return mWhatCache.c_str();
}

void TException::ReportException(std::exception &e)
{
	gLog.Error("Error: %s", e.what());
}

void TException::ReportException()
{
	gLog.FatalError("An unexpected internal error occurred, quitting now.");
}
