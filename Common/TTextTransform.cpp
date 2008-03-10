// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include "TTextTransform.h"

using namespace Halyard;


//=========================================================================
//  Magic Character -> Entity Conversions
//=========================================================================
//  Convert various "magic" character sequences that commonly appear in
//  ASCII text into ISO entities.

utf8_string TTextTransform::FixSpecials(const utf8_string &inString) {
    utf8_string result;

    // Iterate through the string.
    size_t input_length = inString.length();
    for (size_t i = 0; i < input_length; i++) {
		char current = inString[i];
		if (current == '-' && i+1 < input_length && inString[i+1] == '-') {
			result += "&mdash;";
			i++;
		} else if (current == '.' && i+2 < input_length &&
				 inString[i+1] == '.' && inString[i+2] == '.') {
			result += "&hellip;";
			i += 2;
        } else if (current == '<') {
            // Skip to matching '>'.
            result += inString[i++];
            while (i < input_length && inString[i] != '>')
                result += inString[i++];
            if (i < input_length)
                result += inString[i];
		} else {
			result += current;
		}
    }
    return result;
}

utf8_string TTextTransform::FixQuotes(const utf8_string& inString) {
    utf8_string result;
	
    // 'want_left' indicates that the next character, if a quote,
    // should be a left quote.
    // Quotes at the beginning of the string should be a left quote.
    bool want_left = true;
	
    // Iterate through the string.
    size_t input_length = inString.length();
    for (size_t i = 0; i < input_length; i++) {
		char current = inString[i];
		if (current == '\'') {
			if (want_left)
				result += "&lsquo;";
			else
				result += "&rsquo;";
			// Leave want_left unchanged.
		} else if (current == '\"') {
			if (want_left)
				result += "&ldquo;";
			else
				result += "&rdquo;";
			// Leave want_left unchanged.	    
		} else if (isspace(current)) {
			result += current;
			want_left = true;
		} else if (current == '<') {
            // Skip to matching '>'.
            result += inString[i++];
            while (i < input_length && inString[i] != '>')
                result += inString[i++];
            if (i < input_length)
                result += inString[i];
		} else {
			result += current;
			want_left = false;
		}
    }
    return result;
}

utf8_string TTextTransform::TransformString (const utf8_string& inString) {
	return FixQuotes(FixSpecials(inString));
}
