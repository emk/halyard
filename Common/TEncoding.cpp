//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TString.cpp : 
//

#include <ctype.h>
#include <string.h>

#include "THeader.h"
#include "TCommon.h"
#include "TEncoding.h"

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

struct FIVEL_NS EntityMapping {
    char *name;
    char encoded[kMaxEncodedCharLength];
};

#define STANDARD_ASCII_ENTITIES \
    {"quot", "\""}, \
    {"amp", "&"}, \
    {"apos", "'"}, \
    {"hyphen", "-"}, \
    {"period", "."}

// This macro wraps a single-byte string constant in all the grik required
// by each of the compilers we support.  The silly '(char)' is there to
// turn off some MSVC++ warnings.
#define BYTE_1(byte) {(char) (byte), (char) 0x00}

static EntityMapping IsoLatin1EntityMapping[] = {
    STANDARD_ASCII_ENTITIES,
    
    // Internally-Referenced Entities
    {"mdash",  "--"},
    {"lsquo",  "`"},
    {"rsquo",  "'"},
    {"ldquo",  "\""},
    {"rdquo",  "\""},
    {"hellip", "..."},

    // Common Script Entities
    {"copy",   BYTE_1(0xA9)},
    {"reg",    BYTE_1(0xAE)},
    {"trade",  "<TM>"},
    {"bull",   "*"},
    {"sect",   BYTE_1(0xA7)},
    {"ndash",  "-"},
    {"dagger", "<t>"},
    {"micro",  BYTE_1(0xB5)},
    {"para",   BYTE_1(0xB6)},
    {"eacute", BYTE_1(0xE9)},
    {"Ccedil", BYTE_1(0xC7)},
    {"ccedil", BYTE_1(0xE7)},
    
    {NULL}
};

static EntityMapping Windows1252EntityMapping[] = {
    STANDARD_ASCII_ENTITIES,

    // Internally-Referenced Entities
    {"mdash",  BYTE_1(0x97)},
    {"lsquo",  BYTE_1(0x91)},
    {"rsquo",  BYTE_1(0x92)},
    {"ldquo",  BYTE_1(0x93)},
    {"rdquo",  BYTE_1(0x94)},
    {"hellip", BYTE_1(0x85)},

    // Common Script Entities
    {"copy",   BYTE_1(0xA9)},
    {"reg",    BYTE_1(0xAE)},
    {"trade",  BYTE_1(0x99)},
    {"bull",   BYTE_1(0x95)},
    {"sect",   BYTE_1(0xA7)},
    {"ndash",  BYTE_1(0x96)},
    {"dagger", BYTE_1(0x86)},
    {"micro",  BYTE_1(0xB5)},
    {"para",   BYTE_1(0xB6)},
    {"eacute", BYTE_1(0xE9)},
    {"Ccedil", BYTE_1(0xC7)},
    {"ccedil", BYTE_1(0xE7)},

    {NULL}
};

static EntityMapping MacintoshEntityMapping[] = {
    STANDARD_ASCII_ENTITIES,

    // Internally-Referenced Entities
    {"mdash",  BYTE_1(0xD1)},
    {"lsquo",  BYTE_1(0xD4)},
    {"rsquo",  BYTE_1(0xD5)},
    {"ldquo",  BYTE_1(0xD2)},
    {"rdquo",  BYTE_1(0xD3)},
    {"hellip", BYTE_1(0xC9)},

    // Common Script Entities
    {"copy",   BYTE_1(0xA9)},
    {"reg",    BYTE_1(0xA8)},
    {"trade",  BYTE_1(0xAA)},
    {"bull",   BYTE_1(0xA5)},
    {"sect",   BYTE_1(0xA4)},
    {"ndash",  BYTE_1(0xD0)},
    {"dagger", BYTE_1(0xA0)},
    {"micro",  BYTE_1(0xB5)},
    {"para",   BYTE_1(0xA6)},
    {"eacute", BYTE_1(0x8E)},
    {"Ccedil", BYTE_1(0x82)},
    {"ccedil", BYTE_1(0x8D)},

    {NULL}
};


//=========================================================================
//  Constructor
//=========================================================================

TEncoding::TEncoding (const TString& inEncodingName,
					  ErrorLoggingFunc inErrorLoggingFunc)
	: mEncodingName(inEncodingName), mErrorLoggingFunc(inErrorLoggingFunc)
{
	if (mEncodingName == "ISO-8859-1")
		mEntityMapping = &IsoLatin1EntityMapping[0];
	else if (mEncodingName == "windows-1252")
		mEntityMapping = &Windows1252EntityMapping[0];
    else if (mEncodingName == "macintosh")
		mEntityMapping = &MacintoshEntityMapping[0];
	else
	{
		// XXX - We should never get here, so just go ahead and trigger
		// an assertion failure while debugging.  We should really look
		// into issuing some kind of fatal runtime error using the
		// logging subsystem.
		mEntityMapping = NULL;
		ASSERT(false);
	}
}


//=========================================================================
//  Magic Character -> Entity Conversions
//=========================================================================
//  Convert various "magic" character sequences that commonly appear in
//  ASCII text into ISO entities.

TString TEncoding::FixSpecials (const TString& inString) const
{
    TString result;

    // Iterate through the string.
    uint32 input_length = inString.Length();
    for (uint32 i = 0; i < input_length; i++)
    {
		char current = inString[i];
		if (current == '-' && i+1 < input_length && inString[i+1] == '-')
		{
			result += "&mdash;";
			i++;
		}
		else if (current == '.' && i+2 < input_length &&
				 inString[i+1] == '.' && inString[i+2] == '.')
		{
			result += "&hellip;";
			i += 2;
		}
		else
		{
			result += current;
		}
    }
    return result;
}

TString TEncoding::FixQuotes (const TString& inString) const
{
    TString result;
	
    // 'want_left' indicates that the next character, if a quote,
    // should be a left quote.
    // Quotes at the beginning of the string should be a left quote.
    bool want_left = true;
	
    // Iterate through the string.
    uint32 input_length = inString.Length();
    for (uint32 i = 0; i < input_length; i++)
    {
		char current = inString[i];
		if (current == '\'')
		{
			if (want_left)
				result += "&lsquo;";
			else
				result += "&rsquo;";
			// Leave want_left unchanged.
		}
		else if (current == '\"')
		{
			if (want_left)
				result += "&ldquo;";
			else
				result += "&rdquo;";
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

TString TEncoding::EncodeEntities (const TString& inString) const
{
    TString result;

    // Iterate through the string.
    uint32 input_length = inString.Length();
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
			TString name(&((const char*) inString)[start], name_len);
			
			// Handle unterminated entities.
			if (inString[i] != ';')
			{
				(*mErrorLoggingFunc)(inString, start - 1,
									 "Entity needs a trailing semicolon");
				result += "&";
				result += name;
				return result;
			}
			
			// Look up the entity name.
			bool found_mapping = false;
			for (EntityMapping *entity_mapping = mEntityMapping;
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
				result += TString("&") + name + TString(";");
			}
		}
		else
		{
			result += current;
		}
    }
    return result;
}

TString TEncoding::TransformString (const TString& inString) const
{
	return EncodeEntities(FixQuotes(FixSpecials(inString)));
}
