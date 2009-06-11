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
#include <sstream>


using namespace Halyard;

const char* TException::what () const throw ()
{
	std::ostringstream s;
	s << GetClassName() << ": " << GetErrorMessage() << " (";
	if (GetErrorCode() != kNoErrorCode)
	{
		s << "error " << GetErrorCode() << " ";
	}
#ifdef DEBUG
	s << "at " << mErrorFile << ":" << mErrorLine << ")";
#endif // DEBUG
	const_cast<TException*>(this)->mWhatCache = s.str();
	return mWhatCache.c_str();
}

void TException::ReportException(std::exception &e) {
	gLog.Error("halyard", "Error: %s", e.what());
}

void TException::ReportException() {
	ReportFatalException();
}

void TException::ReportFatalException(std::exception &e) {
	gLog.Fatal("halyard", "Error: %s", e.what());
}

void TException::ReportFatalException() {
	gLog.Fatal("halyard", "An unexpected internal error occurred, quitting now.");
}
