// -*- Mode: C++; tab-width: 4; -*-

#ifndef TUtilities_H
#define TUtilities_H

BEGIN_NAMESPACE_FIVEL

//////////
// Copy characters from a source (specified using input
// iterators) into a new string.
//
// This function exists only to work around the lack of an
//   template <class InputIter>
//   basic_string(InputIter begin, InputIter end)
// constructor in MSVC++.
//
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
