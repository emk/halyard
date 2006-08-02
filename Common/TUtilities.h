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

#ifndef TUtilities_H
#define TUtilities_H

BEGIN_NAMESPACE_FIVEL

//////////
/// Copy characters from a source (specified using input
/// iterators) into a new string.
///
/// This function exists only to work around the lack of an
///   template <class InputIter>
///   basic_string(InputIter begin, InputIter end)
/// constructor in MSVC++.
///
template <class OutCharT, class InputIter>
inline std::basic_string<OutCharT>
ConstructString(InputIter begin, InputIter end)
{
	std::basic_string<OutCharT> result(end - begin, ' ');
	std::copy(begin, end, result.begin());
	return result;
}

END_NAMESPACE_FIVEL

#endif // TUtilities_H
