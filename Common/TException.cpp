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
#include <strstream>


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
#ifdef DEBUG
	s << "at " << mErrorFile << ":" << mErrorLine << ")";
#endif // DEBUG
	const_cast<TException*>(this)->mWhatCache = get_string(s);
	return mWhatCache.c_str();
}

void TException::ReportException(std::exception &e) {
	gLog.Error("Error: %s", e.what());
}

void TException::ReportException() {
	ReportFatalException();
}

void TException::ReportFatalException(std::exception &e) {
	gLog.FatalError("Error: %s", e.what());
}

void TException::ReportFatalException() {
	gLog.FatalError("An unexpected internal error occurred, quitting now.");
}
