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
// \file TEncoding.h
//
// Conversion from specially-formatted 7-bit strings to native 8-bit
// strings.
//

#ifndef TEncoding_h
#define TEncoding_h

BEGIN_NAMESPACE_FIVEL

//////////
// An opaque struct type for storing entity to character mappings.
//
template <class CharT>
struct EntityMapping;

//////////
/// Turn HTML-escaped characters into native 8- or 16-bit characters,
/// and handle a few other special escape sequences (--, smart
/// quotes).  This rather ad hoc encoding was chosen at the
/// request of content authors--they don't want to use HTML
/// entities for certain very common characters.
///
/// This class is, unfortunately, a mess.  It has to support
/// both 7- to 8-bit conversion and 7- to 16-bit conversion.  This
/// allows it to support both new-style "(textaa ...)" calls and
/// old-style "(text ...)" calls.
///
/// It's also tied up in the generally broken mess of 5L escape
/// sequences.  Once I'm allowed to deprecate the old-style text
/// drawing routines (Header.* on Win32, and CText.* on the Mac),
/// this code will get much better.
///
template <class CharT>
class TEncoding
{
public:
	typedef CharT char_type;
	typedef std::basic_string<CharT> string_type;

	//////////
	// A callback function that logs errors in strings passed to various
	// TEncoding methods.  A logging function may ignore errors, write them
	// to a log file, or throw an exception.  However, a logging function
	// should be thread-safe and re-entrant.
	//
	// [in] inBadString - the string with an error
	// [in] inBadPos - the 0-based character position of the error
	// [in] inErrMsg - a message explaining what is wrong
	typedef void (*ErrorLoggingFunc) (const string_type &inBadString,
									  size_t inBadPos,
									  const char *inErrMsg);

private:
	//////////
	// The name of the encoding we're using.
	// 
	std::string mEncodingName;

	//////////
	// Our error logging callback.
	//
	ErrorLoggingFunc mErrorLoggingFunc;
	
	//////////
	// The entity mappings for this encoding.
	//
	const EntityMapping<CharT> *mEntityMapping;

public:
	//////////
	// Constructor.  Valid encoding names are currently:
	//
	//    ISO-8859-1: ISO Latin 1 (Unix & web sites)
	//    windows-1252: Windows Latin 1 (U.S. versions of Windows)
	//      http://www.microsoft.com/globaldev/reference/sbcs/1252.htm
	//    macintosh: Standard Apple character set
	//  
	// These encoding names are selected from the IANA MIME character
	// set names at <http://www.iana.org/assignments/character-sets>.
	//
	// [in] inEncodingName - An encoding name.
	//
	TEncoding (const std::string &inEncodingName,
			   ErrorLoggingFunc inErrorLoggingFunc);

	//////////
	// Fetch the name of the encoding supported by this class.
	//
	const std::string GetEncodingName () const
		{ return mEncodingName; }

	//////////
	// Transform double hyphens into m-dash entities (a dash the
	// width of the capital letter 'M'), and '...' sequences into
	// horizontal ellipsis entities.
	//
	// [in] inString - The string to transform.
	// [out] return - The transformed string.
	//
	string_type FixSpecials (const string_type& inString) const;

	//////////
	// Transform \' and \" characters into appropriate left and right
	// quote entities.
	//
	// [in] inString - The string to transform.
	// [out] return - The transformed string.
	//
	string_type FixQuotes (const string_type& inString) const;

	//////////
	// Transform ISO entities (&quot;, &mdash;, etc.) into appropriate
	// characters in the current encoding.  Not all entities are
	// supported.
	//
	// For now, 8-bit data is passed unchanged for backwards compatibility.
   	//
	// [in] inString - The string to transform.
	// [out] return - The transformed string.
	//
	string_type EncodeEntities (const string_type& inString) const;

	//////////
	// Transform string into a native 8-bit string.  This applies all
	// the transformations supported by this object in an appropriate
	// sequence.
	//
	// For now, 8-bit data is passed unchanged for backwards compatibility.
   	//
	// [in] inString - A specially formatted 7-bit string.
	// [out] return - An 8-bit string.
	//
	string_type TransformString (const string_type& inString) const;
};

END_NAMESPACE_FIVEL

#endif // TEncoding_h
