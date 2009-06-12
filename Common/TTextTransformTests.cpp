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

#include <string.h>

#include "ImlUnit.h"
#include "TTextTransform.h"

using namespace Halyard;
using namespace TTextTransform;


//=========================================================================
//  Test Cases
//=========================================================================

extern void test_TTextTransform (void);

void test_TTextTransform (void) {
    // A null transformation.
    TEST(TransformString("Hello!") == "Hello!");
    
    // Skip anything in angle brackets.
    TEST(TransformString("<a href=\"...--'\">Foo</a>") ==
         "<a href=\"...--'\">Foo</a>");

    // Transform double hyphens into m-dash escape sequences.
    TEST(FixSpecials("") == "");
    TEST(FixSpecials("a-b") == "a-b");
    TEST(FixSpecials("a--b") == "a&mdash;b");
    TEST(FixSpecials("a---b") == "a&mdash;-b");
    TEST(FixSpecials("a----b") == "a&mdash;&mdash;b");
    TEST(FixSpecials("a -- b") == "a &mdash; b");
    TEST(FixSpecials("--") == "&mdash;");
    
    // Transform '...' sequences into ellipses.
    TEST(FixSpecials("...") == "&hellip;");
    TEST(FixSpecials("....") == "&hellip;.");
    TEST(FixSpecials(".....") == "&hellip;..");
    TEST(FixSpecials("......") == "&hellip;&hellip;");
    TEST(FixSpecials("Bye...") == "Bye&hellip;");
    
    // Education of quotes.
    // Double quotes within strings must be written as \042 (the octal
    // escape sequence) to avoid bugs in the expansion of the TEST macro
    // under Metrowerks C++ and Visual C++.
    TEST(FixQuotes("") == "");
    TEST(FixQuotes("\042Hello!\042") == "&ldquo;Hello!&rdquo;");
    TEST(FixQuotes("Sam's") == "Sam&rsquo;s");
    TEST(FixQuotes(" 'foo' ") == " &lsquo;foo&rsquo; ");
    TEST(FixQuotes(" 'foo,' ") == " &lsquo;foo,&rsquo; ");
    TEST(FixQuotes(" 'foo', ") == " &lsquo;foo&rsquo;, ");
    TEST(FixQuotes(" \042foo\042 ") == " &ldquo;foo&rdquo; ");
    TEST(FixQuotes(" \042foo,\042 ") == " &ldquo;foo,&rdquo; ");
    TEST(FixQuotes(" \042foo\042, ") == " &ldquo;foo&rdquo;, ");
    TEST(FixQuotes(" \042\'foo,\' he said!\042 ") ==
         " &ldquo;&lsquo;foo,&rsquo; he said!&rdquo; ");
    TEST(FixQuotes("\n\042Hi!\042") == "\n&ldquo;Hi!&rdquo;");
    TEST(FixQuotes("\r\042Hi!\042") == "\r&ldquo;Hi!&rdquo;");
    
    // I know authors won't like these behaviors, but they match
    // most smart quoting algorithms, and they keep the code simple.
    TEST(FixQuotes("class of '90") == "class of &lsquo;90");
    TEST(FixQuotes("'till") == "&lsquo;till");
}
