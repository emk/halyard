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

//////////////////////////////////////////////////////////////////////////////
/// \file TEncoding.h
///
/// Conversion from specially-formatted 7-bit strings to Unicode
/// strings.
///

#ifndef TTextTransform_h
#define TTextTransform_h

BEGIN_NAMESPACE_HALYARD

//////////
/// Turn HTML-escaped characters into Unicode characters, and handle a few
/// other special escape sequences (--, smart quotes).  This rather ad hoc
/// encoding was chosen at the request of content authors--they don't want
/// to use HTML entities for certain very common characters.
///
/// Note that we do not tranform anything inside angle brackets ("<>"), so
/// this routine can be more-or-less safely applied to XML text, and won't
/// do anything particularly useful to non-XML text.
///
namespace TTextTransform
{
    //////////
    /// Transform double hyphens into m-dash entities (a dash the
    /// width of the capital letter 'M'), and '...' sequences into
    /// horizontal ellipsis entities.
    ///
    /// \param inString  The string to transform.
    /// \return  The transformed string.
    ///
    utf8_string FixSpecials(const utf8_string& inString);

    //////////
    /// Transform \' and \" characters into appropriate left and right
    /// quote entities.
    ///
    /// \param inString  The string to transform.
    /// \return  The transformed string.
    ///
    utf8_string FixQuotes(const utf8_string& inString);

    //////////
    /// Apply all transformations to a string.
    ///
    utf8_string TransformString(const utf8_string& inString);
};

END_NAMESPACE_HALYARD

#endif // TTextTransform_h
