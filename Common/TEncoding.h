// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

//////////////////////////////////////////////////////////////////////////////
/// \file TEncoding.h
///
/// Conversion from specially-formatted 7-bit strings to Unicode
/// strings.
///

#ifndef TEncoding_h
#define TEncoding_h

BEGIN_NAMESPACE_FIVEL

//////////
/// An opaque struct type for storing entity to character mappings.
///
struct EntityMapping;

//////////
/// Turn HTML-escaped characters into Unicode characters, and handle a few
/// other special escape sequences (--, smart quotes).  This rather ad hoc
/// encoding was chosen at the request of content authors--they don't want
/// to use HTML entities for certain very common characters.
///
class TEncoding
{
public:
	//////////
	/// A callback function that logs errors in strings passed to various
	/// TEncoding methods.  A logging function may ignore errors, write them
	/// to a log file, or throw an exception.  However, a logging function
	/// should be thread-safe and re-entrant.
	///
	/// \param inBadString  the string with an error
	/// \param inBadPos  the 0-based character position of the error
	/// \param inErrMsg  a message explaining what is wrong
	typedef void (*ErrorLoggingFunc) (const std::wstring &inBadString,
									  size_t inBadPos,
									  const char *inErrMsg);

private:
	//////////
	/// Our error logging callback.
	///
	ErrorLoggingFunc mErrorLoggingFunc;
	
	//////////
	/// The entity mappings for this encoding.
	///
	const EntityMapping *mEntityMapping;

public:
	//////////
	/// Constructor.
	///
	TEncoding (ErrorLoggingFunc inErrorLoggingFunc);

	//////////
	/// Transform double hyphens into m-dash entities (a dash the
	/// width of the capital letter 'M'), and '...' sequences into
	/// horizontal ellipsis entities.
	///
	/// \param inString  The string to transform.
	/// \return  The transformed string.
	///
	std::wstring FixSpecials (const std::wstring& inString) const;

	//////////
	/// Transform \' and \" characters into appropriate left and right
	/// quote entities.
	///
	/// \param inString  The string to transform.
	/// \return  The transformed string.
	///
	std::wstring FixQuotes (const std::wstring& inString) const;

	//////////
	/// Transform ISO entities (&quot;, &mdash;, etc.) into appropriate
	/// characters in the current encoding.  Not all entities are
	/// supported.
	///
	/// For now, 8-bit data is passed unchanged for backwards compatibility.
   	///
	/// \param inString  The string to transform.
	/// \return  The transformed string.
	///
	std::wstring EncodeEntities (const std::wstring& inString) const;

	//////////
	/// Transform string into a native 8-bit string.  This applies all
	/// the transformations supported by this object in an appropriate
	/// sequence.
	///
	/// For now, 8-bit data is passed unchanged for backwards compatibility.
   	///
	/// \param inString  A specially formatted 7-bit string.
	/// \return  An 8-bit string.
	///
	std::wstring TransformString (const std::wstring& inString) const;
};

END_NAMESPACE_FIVEL

#endif // TEncoding_h
