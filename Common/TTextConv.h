// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#ifndef TTextConv_H
#define TTextConv_H

BEGIN_NAMESPACE_FIVEL

/// We use this typedef when a string is known to contain UTF-8 encoded
/// data.  Note that we require all characters to be encoded with the
/// smallest possible number of bytes.
typedef std::string utf8_string;

/// We use this typedef when a string is known to contain UTF-16 encoded
/// data.  Note that we cannot portably store UTF-32 data in any standard
/// C++ character type.
///
/// Note that right now, this type is only _approximately_ UTF-16.
/// Depending on how we choose to handle non-BMP characters, we may want to
/// rename this typedef to something as accurate as possible.
typedef std::wstring utf16_string;

utf16_string utf16_from_utf8(const utf8_string &utf8);
utf8_string utf8_from_utf16(const utf16_string &utf16);

std::string multibyte_from_utf16(const utf16_string &utf16);
utf16_string utf16_from_multibyte(const std::string &str);

std::string multibyte_from_utf8(const utf8_string &utf8);
utf8_string utf8_from_multibyte(const std::string &str);

END_NAMESPACE_FIVEL

#endif // TTextTransform_h
