//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 2002, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TEncoding.h : Conversion from specially-formatted 7-bit strings to
//               native 8-bit strings.
//

#ifndef TEncoding_h
#define TEncoding_h

#include "TObject.h"
#include "TString.h"

BEGIN_NAMESPACE_FIVEL

//////////
// An opaque struct type for storing entity to character mappings.
//
struct EntityMapping;


/*-----------------------------------------------------------------

CLASS
    TEncoding

    Turn HTML-escaped characters into native 8-bit characters,
	and handle a few other special escape sequences (--, smart
	quotes).  This rather ad hoc encoding was chosen at the
	request of content authors--they don't want to use HTML
	entities for certain very common characters.

AUTHOR
    Eric Kidd

------------------------------------------------------------------*/
class TEncoding : public TObject
{
public:
	//////////
	// A callback function that logs errors in strings passed to various
	// TEncoding methods.  A logging function may ignore errors, write them
	// to a log file, or throw an exception.  However, a logging function
	// should be thread-safe and re-entrant.
	//
	// [in] inBadString - the string with an error
	// [in] inBadPos - the 0-based character position of the error
	// [in] inErrMsg - a message explaining what is wrong
	typedef void (*ErrorLoggingFunc) (const char *inBadString,
									  size_t inBadPos,
									  const char *inErrMsg);

private:
	//////////
	// The name of the encoding we're using.
	// 
	TString mEncodingName;

	//////////
	// Our error logging callback.
	//
	ErrorLoggingFunc mErrorLoggingFunc;
	
	//////////
	// The entity mappings for this encoding.
	//
	EntityMapping *mEntityMapping;

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
	TEncoding (const TString& inEncodingName,
			   ErrorLoggingFunc inErrorLoggingFunc);

	//////////
	// Fetch the name of the encoding supported by this class.
	//
	const TString GetEncodingName () const
		{ return mEncodingName; }

	//////////
	// Transform double hyphens into m-dash entities (a dash the
	// width of the capital letter 'M'), and '...' sequences into
	// horizontal ellipsis entities.
	//
	// [in] inString - The string to transform.
	// [out] return - The transformed string.
	//
	TString FixSpecials (const TString& inString) const;

	//////////
	// Transform \' and \" characters into appropriate left and right
	// quote entities.
	//
	// [in] inString - The string to transform.
	// [out] return - The transformed string.
	//
	TString FixQuotes (const TString& inString) const;

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
	TString EncodeEntities (const TString& inString) const;

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
	TString TransformString (const TString& inString) const;
};

END_NAMESPACE_FIVEL

#endif // TEncoding_h
