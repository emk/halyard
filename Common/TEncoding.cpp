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
//  These tables can be used to convert ISO entities into various native
//  character sets.
//
//  This is not a particularly efficient data structure, but we'll keep
//  it simple until we *know* it's too slow.
//
//  Good references include:
//    http://developer.apple.com/techpubs/mac/Text/Text-516.html
//    http://www.microsoft.com/globaldev/reference/sbcs/1252.htm
//    http://www.htmlhelp.com/reference/charset/latin1.gif
//    http://www.iana.org/assignments/character-sets

static const int kMaxEncodedCharLength = 10;

/// A template mapping HTML-style entity names to groups of characters.
template <class CharT>
struct FIVEL_NS EntityMapping {
    CharT *name;
    CharT encoded[kMaxEncodedCharLength];
};

// These macros wrap a single-byte string constant in all the grik required
// by each of the compilers we support.  The silly '(char)' is there to
// turn off some MSVC++ warnings.
#define CHAR_1(byte) {(char) (byte), (char) 0x00}
#define WCHAR_1(byte) {(wchar_t) (byte), (wchar_t) 0x00}

// Entities which appear in each of our 7-bit mappings.
// See the definition immediately below!
#define STANDARD_ASCII_CHAR_ENTITIES \
    {"quot",    CHAR_1('\"')}, \
    {"amp",     CHAR_1('&')}, \
    {"apos",    CHAR_1('\'')}, \
    {"hyphen",  CHAR_1('-')}, \
    {"period",  CHAR_1('.')}

// The same as the above, but for Unicode.
#define STANDARD_ASCII_WCHAR_ENTITIES \
    {L"quot",   WCHAR_1('\"')}, \
    {L"amp",    WCHAR_1('&')}, \
    {L"apos",   WCHAR_1('\'')}, \
    {L"hyphen", WCHAR_1('-')}, \
    {L"period", WCHAR_1('.')}

static EntityMapping<char> IsoLatin1EntityMapping[] = {
    STANDARD_ASCII_CHAR_ENTITIES,
    
    // Internally-Referenced Entities
    {"mdash",  "--"},
    {"lsquo",  "`"},
    {"rsquo",  "'"},
    {"ldquo",  "\""},
    {"rdquo",  "\""},
    {"hellip", "..."},

    // Common Script Entities
    {"copy",   CHAR_1(0xA9)},
    {"reg",    CHAR_1(0xAE)},
    {"trade",  "<TM>"},
    {"bull",   "*"},
    {"sect",   CHAR_1(0xA7)},
    {"ndash",  "-"},
    {"dagger", "<t>"},
    {"micro",  CHAR_1(0xB5)},
    {"para",   CHAR_1(0xB6)},
    {"eacute", CHAR_1(0xE9)},
    {"Ccedil", CHAR_1(0xC7)},
    {"ccedil", CHAR_1(0xE7)},
    
    {NULL}
};

static EntityMapping<char> Windows1252EntityMapping[] = {
    STANDARD_ASCII_CHAR_ENTITIES,

    // Internally-Referenced Entities
    {"mdash",  CHAR_1(0x97)},
    {"lsquo",  CHAR_1(0x91)},
    {"rsquo",  CHAR_1(0x92)},
    {"ldquo",  CHAR_1(0x93)},
    {"rdquo",  CHAR_1(0x94)},
    {"hellip", CHAR_1(0x85)},

    // Common Script Entities
    {"copy",   CHAR_1(0xA9)},
    {"reg",    CHAR_1(0xAE)},
    {"trade",  CHAR_1(0x99)},
    {"bull",   CHAR_1(0x95)},
    {"sect",   CHAR_1(0xA7)},
    {"ndash",  CHAR_1(0x96)},
    {"dagger", CHAR_1(0x86)},
    {"micro",  CHAR_1(0xB5)},
    {"para",   CHAR_1(0xB6)},
    {"eacute", CHAR_1(0xE9)},
    {"Ccedil", CHAR_1(0xC7)},
    {"ccedil", CHAR_1(0xE7)},

    {NULL}
};

static EntityMapping<char> MacintoshEntityMapping[] = {
    STANDARD_ASCII_CHAR_ENTITIES,

    // Internally-Referenced Entities
    {"mdash",  CHAR_1(0xD1)},
    {"lsquo",  CHAR_1(0xD4)},
    {"rsquo",  CHAR_1(0xD5)},
    {"ldquo",  CHAR_1(0xD2)},
    {"rdquo",  CHAR_1(0xD3)},
    {"hellip", CHAR_1(0xC9)},

    // Common Script Entities
    {"copy",   CHAR_1(0xA9)},
    {"reg",    CHAR_1(0xA8)},
    {"trade",  CHAR_1(0xAA)},
    {"bull",   CHAR_1(0xA5)},
    {"sect",   CHAR_1(0xA4)},
    {"ndash",  CHAR_1(0xD0)},
    {"dagger", CHAR_1(0xA0)},
    {"micro",  CHAR_1(0xB5)},
    {"para",   CHAR_1(0xA6)},
    {"eacute", CHAR_1(0x8E)},
    {"Ccedil", CHAR_1(0x82)},
    {"ccedil", CHAR_1(0x8D)},

    {NULL}
};

static EntityMapping<wchar_t> UnicodeEntityMapping[] = {
    STANDARD_ASCII_WCHAR_ENTITIES,

    // Internally-Referenced Entities
    {L"mdash",  WCHAR_1(0x2014)},
    {L"lsquo",  WCHAR_1(0x2018)},
    {L"rsquo",  WCHAR_1(0x2019)},
    {L"ldquo",  WCHAR_1(0x201C)},
    {L"rdquo",  WCHAR_1(0x201D)},
    {L"hellip", WCHAR_1(0x2026)},

    // Common Script Entities
    {L"copy",   WCHAR_1(0x00A9)},
    {L"reg",    WCHAR_1(0x00AE)},
    {L"trade",  WCHAR_1(0x2122)},
    {L"bull",   WCHAR_1(0x2022)},
    {L"sect",   WCHAR_1(0x00A7)},
    {L"ndash",  WCHAR_1(0x2013)},
    {L"dagger", WCHAR_1(0x2020)},
    {L"micro",  WCHAR_1(0x2021)},
    {L"para",   WCHAR_1(0x00B6)},
    {L"eacute", WCHAR_1(0x00E9)},
    {L"Ccedil", WCHAR_1(0x00C7)},
    {L"ccedil", WCHAR_1(0x00E7)},

	// Unicode-only entities.
	{L"egrave", WCHAR_1(0x00E8)}, // E with grave accent.
	{L"nbsp",   WCHAR_1(0x00A0)}, // Non-breaking space.
	{L"shy",    WCHAR_1(0x00AD)}, // Soft hyphen.
	{L"radic",  WCHAR_1(0x221A)}, // Square root.
	{L"check",  WCHAR_1(0x2713)}, // Check mark.
	{L"cross",  WCHAR_1(0x2717)}, // Ballot X.
	{L"Delta",  WCHAR_1(0x2206)}, // Unicode INCREMENT character.
	{L"delta",  WCHAR_1(0x03B4)}, // Lowercase greek delta.

    {NULL}
};


//=========================================================================
//  Template Support Functions
//=========================================================================
//  These are basically cruft--they're only here to allow us to support
//  both 'char' and 'wchar_t' during the 'text' -> 'textaa' migration.
//  Once the 'text' command goes away, we can simplify this class
//  ruthlessly.

// Create an empty template function, and specialize it for char
// and wchar_t.  The 'tag' argument is unused, but is required for
// some C++ compilers to select the right version of the template.
// Pass it in as '(char) 0' or '(wchar_t) 0'.
template <class CharT>
static const EntityMapping<CharT> *
find_mapping(const std::string& inEncoding, CharT tag);

template <>
static const EntityMapping<char> *
find_mapping(const std::string& inEncoding, char tag)
{
	if (inEncoding == "ISO-8859-1")
		return &IsoLatin1EntityMapping[0];
	else if (inEncoding == "windows-1252")
		return &Windows1252EntityMapping[0];
    else if (inEncoding == "macintosh")
		return &MacintoshEntityMapping[0];
	else
		return NULL;
}

template <>
static const EntityMapping<wchar_t> *
find_mapping(const std::string& inEncoding, wchar_t tag)
{
	if (inEncoding == "UTF-16")
		return &UnicodeEntityMapping[0];
	else
		return NULL;
}

template <class CharT>
static std::basic_string<CharT> convert_string(const char *inString)
{
	return ConstructString<CharT,const char*>(inString,
											  inString+strlen(inString));
}


//=========================================================================
//  Constructor
//=========================================================================

template<class CharT>
TEncoding<CharT>::TEncoding (const std::string& inEncodingName,
							 ErrorLoggingFunc inErrorLoggingFunc)
	: mEncodingName(inEncodingName), mErrorLoggingFunc(inErrorLoggingFunc)
{
	mEntityMapping = find_mapping<CharT>(inEncodingName, (CharT) 0);
	if (mEntityMapping == NULL)
		gLog.FatalError("Unknown character set %s", inEncodingName.c_str());
}


//=========================================================================
//  Magic Character -> Entity Conversions
//=========================================================================
//  Convert various "magic" character sequences that commonly appear in
//  ASCII text into ISO entities.

template<class CharT>
typename TEncoding<CharT>::string_type
TEncoding<CharT>::FixSpecials (const string_type& inString) const
{
    string_type result;

    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++)
    {
		char current = inString[i];
		if (current == '-' && i+1 < input_length && inString[i+1] == '-')
		{
			result += convert_string<CharT>("&mdash;");
			i++;
		}
		else if (current == '.' && i+2 < input_length &&
				 inString[i+1] == '.' && inString[i+2] == '.')
		{
			result += convert_string<CharT>("&hellip;");
			i += 2;
		}
		else
		{
			result += current;
		}
    }
    return result;
}

template<class CharT>
typename TEncoding<CharT>::string_type
TEncoding<CharT>::FixQuotes (const string_type& inString) const
{
    string_type result;
	
    // 'want_left' indicates that the next character, if a quote,
    // should be a left quote.
    // Quotes at the beginning of the string should be a left quote.
    bool want_left = true;
	
    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++)
    {
		char current = inString[i];
		if (current == '\'')
		{
			if (want_left)
				result += convert_string<CharT>("&lsquo;");
			else
				result += convert_string<CharT>("&rsquo;");
			// Leave want_left unchanged.
		}
		else if (current == '\"')
		{
			if (want_left)
				result += convert_string<CharT>("&ldquo;");
			else
				result += convert_string<CharT>("&rdquo;");
			// Leave want_left unchanged.	    
		}
		else if (isspace(current))
		{
			result += current;
			want_left = true;
		}
		else
		{
			result += current;
			want_left = false;
		}
    }
    return result;
}

template<class CharT>
typename TEncoding<CharT>::string_type
TEncoding<CharT>::EncodeEntities (const string_type& inString) const
{
    string_type result;

    // Iterate through the string.
    uint32 input_length = inString.length();
    for (uint32 i = 0; i < input_length; i++)
    {
		char current = inString[i];
		if (current == '&')
		{
			// Scan forward until we find the end of the entity name.
			i++;
			int start = i;
			while (i < input_length && inString[i] != ';')
				i++;
			
			// Extract the entity name.
			int name_len = i - start;
			string_type name = inString.substr(start, name_len);
			
			// Handle unterminated entities.
			if (inString[i] != ';')
			{
				(*mErrorLoggingFunc)(inString, start - 1,
									 "Entity needs a trailing semicolon");
				result += '&';
				result += name;
				return result;
			}
			
			// Look up the entity name.
			bool found_mapping = false;
			for (const EntityMapping<CharT> *entity_mapping = mEntityMapping;
				 entity_mapping->name != NULL; entity_mapping++)
			{
				if (entity_mapping->name == name)
				{
					result += entity_mapping->encoded;
					found_mapping = true;
					break;
				}
			}
			if (!found_mapping)
			{
				(*mErrorLoggingFunc)(inString, start - 1,
									 "Unknown entity name");
				result += (convert_string<CharT>("&") + name +
						   convert_string<CharT>(";"));
			}
		}
		else
		{
			result += current;
		}
    }
    return result;
}

template<class CharT>
typename TEncoding<CharT>::string_type
TEncoding<CharT>::TransformString (const string_type& inString) const
{
	return EncodeEntities(FixQuotes(FixSpecials(inString)));
}


//=========================================================================
//  Template Instantiations
//=========================================================================
//  We need to instantiate our templates manually.

template class TEncoding<char>;
template class TEncoding<wchar_t>;

