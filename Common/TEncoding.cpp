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

#include "CommonHeaders.h"

#include <ctype.h>
#include <string.h>

#include "TEncoding.h"
#include "TUtilities.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Entity Mapping Tables
//=========================================================================
//  These tables can be used to convert ISO entities into Unicode.
//
//  This is not a particularly efficient data structure, but we'll keep
//  it simple until we *know* it's too slow.
//
//  Back when we supported character sets other than Unicode, the
//  following references were quite useful:
//
//    http://developer.apple.com/techpubs/mac/Text/Text-516.html
//    http://www.microsoft.com/globaldev/reference/sbcs/1252.htm
//    http://www.htmlhelp.com/reference/charset/latin1.gif
//    http://www.iana.org/assignments/character-sets

/// A template mapping HTML-style entity names to groups of characters.
struct FIVEL_NS EntityMapping {
    wchar_t *name;
    wchar_t encoded;
};

// Just in case some compilers require a cast...
#define WCHAR(c) (c)

static EntityMapping UnicodeEntityMapping[] = {
    // Standard ASCII Entities
    {L"quot",   L'\"'},
    {L"amp",    L'&'},
    {L"apos",   L'\''},
    {L"hyphen", L'-'},
    {L"period", L'.'},

    // Internally-Referenced Entities
    {L"mdash",  WCHAR(0x2014)},
    {L"lsquo",  WCHAR(0x2018)},
    {L"rsquo",  WCHAR(0x2019)},
    {L"ldquo",  WCHAR(0x201C)},
    {L"rdquo",  WCHAR(0x201D)},
    {L"hellip", WCHAR(0x2026)},

    // Common Script Entities
    {L"copy",   WCHAR(0x00A9)},
    {L"reg",    WCHAR(0x00AE)},
    {L"trade",  WCHAR(0x2122)},
    {L"bull",   WCHAR(0x2022)},
    {L"sect",   WCHAR(0x00A7)},
    {L"ndash",  WCHAR(0x2013)},
    {L"dagger", WCHAR(0x2020)},
    {L"micro",  WCHAR(0x00B5)},
    {L"para",   WCHAR(0x00B6)},
    {L"eacute", WCHAR(0x00E9)},
    {L"Ccedil", WCHAR(0x00C7)},
    {L"ccedil", WCHAR(0x00E7)},

	// Unicode-only entities.  Here's a nice table of HTML entity names:
    //   http://www.cs.tut.fi/~jkorpela/html/guide/entities.html
	{L"egrave", WCHAR(0x00E8)}, // E with grave accent.
	{L"nbsp",   WCHAR(0x00A0)}, // Non-breaking space.
	{L"shy",    WCHAR(0x00AD)}, // Soft hyphen.
    {L"deg",    WCHAR(0x00B0)}, // Degree sign.
	{L"radic",  WCHAR(0x221A)}, // Square root.
	{L"check",  WCHAR(0x2713)}, // Check mark (may not have font support).
	{L"cross",  WCHAR(0x2717)}, // Ballot X (may not have font support).
	{L"Delta",  WCHAR(0x2206)}, // Unicode INCREMENT character.
    {L"alpha",  WCHAR(0x03B1)}, // Lowercase greek alpha.
    {L"beta",   WCHAR(0x03B2)}, // Lowercase greek beta.
    {L"gamma",  WCHAR(0x03B3)}, // Lowercase greek gamma.
	{L"delta",  WCHAR(0x03B4)}, // Lowercase greek delta.
    {L"lambda", WCHAR(0x03BB)}, // Lowercase greek lambda.
    {L"pi",     WCHAR(0x03C0)}, // Lowercase greek pi.
    {L"infin",  WCHAR(0x221E)}, // Infinity.
    {L"ne",     WCHAR(0x2260)}, // Not equal sign.
    {L"AElig",  WCHAR(0x00C6)}, // AE ligature.
    {L"aelig",  WCHAR(0x00E6)}, // ae ligature.
    {L"OElig",  WCHAR(0x0152)}, // OE ligature.
    {L"oelig",  WCHAR(0x0153)}, // oe ligature.
    {L"sup1",   WCHAR(0x00B9)}, // Superscript 1.
    {L"sup2",   WCHAR(0x00B2)}, // Superscript 2.
    {L"sup3",   WCHAR(0x00B3)}, // Superscript 3.
    {L"frac14", WCHAR(0x00BC)}, // 1/4.
    {L"frac12", WCHAR(0x00BD)}, // 1/2.
    {L"frac34", WCHAR(0x00BE)}, // 3/4.
    {L"plusmn", WCHAR(0x00B1)}, // +-
    {L"there4", WCHAR(0x2234)}, // "Therefore" (three-dot triangle).
    {L"times",  WCHAR(0x00D7)}, // Multiplication.
    {L"divide", WCHAR(0x00F7)}, // Division.
    {L"euro",   WCHAR(0x8364)}, // Euro (may not have font support).

    {NULL}
};


//=========================================================================
//  Support Functions
//=========================================================================

static bool parseInt(const wchar_t *inStr, int &outInt) {
    // Look up a numeric entity. We need to reimplement 'watoi' here
    // because we need some halfway decent error-handling, and because the
    // library support for Unicode is pretty awful circa 2005.
    //
    // TODO - Detect overflow?
    outInt = 0;
    for (size_t i = 0; inStr[i] != '\0'; ++i) {
        if (inStr[i] > 255)
            return false;
        char c = static_cast<char>(inStr[i]);
        if (!isdigit(c))
            return false;
		char s[2];
		s[0] = c;
		s[1] = '\0';
        outInt = outInt*10 + atoi(s);
    }
    return true;
}

static std::wstring convert_string(const char *inString) {
	return ConstructString<wchar_t,const char*>(inString,
                                                inString+strlen(inString));
}


//=========================================================================
//  Constructor
//=========================================================================

TEncoding::TEncoding (ErrorLoggingFunc inErrorLoggingFunc)
	: mErrorLoggingFunc(inErrorLoggingFunc)
{
	mEntityMapping = &UnicodeEntityMapping[0];
}


//=========================================================================
//  Magic Character -> Entity Conversions
//=========================================================================
//  Convert various "magic" character sequences that commonly appear in
//  ASCII text into ISO entities.

std::wstring TEncoding::FixSpecials(const std::wstring &inString) const {
    std::wstring result;

    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++) {
		wchar_t current = inString[i];
		if (current == '-' && i+1 < input_length && inString[i+1] == '-') {
			result += convert_string("&mdash;");
			i++;
		} else if (current == '.' && i+2 < input_length &&
				 inString[i+1] == '.' && inString[i+2] == '.') {
			result += convert_string("&hellip;");
			i += 2;
		} else {
			result += current;
		}
    }
    return result;
}

std::wstring TEncoding::FixQuotes(const std::wstring& inString) const {
    std::wstring result;
	
    // 'want_left' indicates that the next character, if a quote,
    // should be a left quote.
    // Quotes at the beginning of the string should be a left quote.
    bool want_left = true;
	
    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++) {
		wchar_t current = inString[i];
		if (current == '\'') {
			if (want_left)
				result += convert_string("&lsquo;");
			else
				result += convert_string("&rsquo;");
			// Leave want_left unchanged.
		} else if (current == '\"') {
			if (want_left)
				result += convert_string("&ldquo;");
			else
				result += convert_string("&rdquo;");
			// Leave want_left unchanged.	    
		} else if (isspace(current)) {
			result += current;
			want_left = true;
		} else {
			result += current;
			want_left = false;
		}
    }
    return result;
}

std::wstring TEncoding::EncodeEntities(const std::wstring& inString) const {
    std::wstring result;

    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++) {
		wchar_t current = inString[i];
		if (current == '&') {
			// Scan forward until we find the end of the entity name.
			i++;
			int start = i;
			while (i < input_length && inString[i] != ';')
				i++;
			
			// Extract the entity name.
			int name_len = i - start;
			std::wstring name = inString.substr(start, name_len);
			
			// Handle unterminated entities.
			if (inString[i] != ';') {
				(*mErrorLoggingFunc)(inString, start - 1,
									 "Entity needs a trailing semicolon");
				result += '&';
				result += name;
				return result;
			}
			
			// Look up the entity name.
			bool found_mapping = false;
            if (name[0] == '#' && name_len > 1) {
                // Look up a numeric entity. We need to reimplement 'atoi'
                // here because we don't know what character size we're dealing
                // with and the library support for Unicode is pretty awful
                // circa 2005.
                int number;
                if (parseInt(&(name.c_str()[1]), number)) {
                    found_mapping = true;
                    result += static_cast<wchar_t>(number);
                }
            } else {
                // Look up a named entity.
                for (const EntityMapping *mapping = mEntityMapping;
                     mapping->name != NULL; mapping++)
                {
                    if (mapping->name == name) {
                        result += mapping->encoded;
                        found_mapping = true;
                        break;
                    }
                }
            }

			if (!found_mapping) {
				(*mErrorLoggingFunc)(inString, start - 1,
									 "Unknown entity name");
				result += (convert_string("&") + name +
						   convert_string(";"));
			}
		} else {
			result += current;
		}
    }
    return result;
}

std::wstring TEncoding::TransformString (const std::wstring& inString) const {
	return EncodeEntities(FixQuotes(FixSpecials(inString)));
}
