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

#ifndef TSchemeCallback_H
#define TSchemeCallback_H

BEGIN_NAMESPACE_HALYARD

//////////
/// A C++ wrapper for a zero-argument Scheme callback function (a "thunk").
///
class TSchemeCallback : public TCallback
{
	//////////
	/// Our callback object.  Note that we need to use a TSchemePtr to
	/// prevent mCallback from being garbage-collected, because we're
	/// a heap-based object.
	///
	TSchemePtr<Scheme_Object> mCallback;

public:
	TSchemeCallback(Scheme_Object *inCallback) : mCallback(inCallback) {}

	// For documentation of these virtual methods, see TInterpreter.h.
	virtual TValue Run(const TValueList &inArguments = TValueList());
	virtual std::string PrintableRepresentation() { return "#<thunk>"; }
};

END_NAMESPACE_HALYARD

#endif // TSchemeCallback_H
