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
#include "TEncoding.h"

USING_NAMESPACE_FIVEL

TString TEncoding::FixMDashes (const TString& inString) const
{
    TString result;

    // Iterate through the string.
    int input_length = inString.Length();
    for (int i = 0; i < input_length; i++)
    {
	char current = inString[i];
	if (current == '-' && i+1 < input_length && inString[i+1] == '-')
	{
	    result += "&mdash;";
	    i++;
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
    int input_length = inString.Length();
    for (int i = 0; i < input_length; i++)
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

typedef struct {
    char *name;
    char *encoded;
} EntityMapping;

static EntityMapping entity_mappings[] = {
    {"quot", "\""},
    {"amp", "&"},
    {"#39", "'"},
    {"#45", "-"},
    {NULL, NULL}
};

TString TEncoding::EncodeEntities (const TString& inString) const
{
    TString result;

    // Iterate through the string.
    int input_length = inString.Length();
    for (int i = 0; i < input_length; i++)
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
	    for (EntityMapping *entity_mapping = entity_mappings;
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
		result += "&" + name + ";";
	    }
	}
	else
	{
	    result += current;
	}
    }
    return result;
}
