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

#ifndef TTemplateUtils_H
#define TTemplateUtils_H

BEGIN_NAMESPACE_HALYARD

/// Perform a case-insensitive comparison of two strings.
extern bool StringIComp(const std::string &inStr1, const std::string &inStr2);

/// Trim spaces in string.
extern void StringLTrim(std::string &ioStr);

/// Return true if inStr1 starts with inStr2. Do a case sensitive 
/// comparison by default.
extern bool StringStartsWith(const std::string &inStr1,
                             const std::string &inStr2);

/// Convert an STL string to lowercase.
///
/// \param inString  the string to convert
/// \return  the input string, as lowercase
extern std::string MakeStringLowercase(std::string inString);

/// Convert nested quotes within an STL string into 
/// escape-character quotes.
///
/// \param inString  the string to convert
/// \return  the input string, with escaped quotes
extern std::string MakeQuotedString(const std::string &inString);

/// This functor can be used to delete all the pointers in a container
/// as follows:
///
///   for_each(container.begin, container.end(), DeletePointer())
///
/// This code is adapted from Meyer's "Effective STL", and modified
/// to use the appropriate boost deletion operators.
struct DeletePointer {
    template <typename T>
    void operator() (T* ptr) const
    {
        boost::checked_delete<T>(ptr);
    }
};

/// The same as DeletePointer, but for use with containers of pointers
/// TO ARRAYS.
struct DeleteArray {
    template <typename T>
    void operator() (T* ptr) const
    {
        boost::checked_array_delete<T>(ptr);
    }
};

/// You can use this class to save and restore a value in an exception-safe
/// fashion.  To use:
///
///   int i = 0;
///   StValueRestorer<int> restore_i(i);
///
/// When restore_i goes out of scope, i will be reset to its original value.
template<class Type>
class StValueRestorer : boost::noncopyable {
    Type &mLocation;
    Type mSaved;

public:
    explicit StValueRestorer(Type &inVariable)
        : mLocation(inVariable), mSaved(inVariable) { }
    ~StValueRestorer() { mLocation = mSaved; }
};

END_NAMESPACE_HALYARD

#endif // TTemplateUtils_H
