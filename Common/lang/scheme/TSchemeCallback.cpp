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
#include "TSchemeInterpreter.h"
#include "TSchemeConv.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//	TSchemeCallback Methods
//=========================================================================

TValue TSchemeCallback::Run(const TValueList &inArguments)
{
	// Make sure we have a Scheme interpreter and that it isn't stopped.
	ASSERT(TSchemeInterpreter::HaveInstance());
	ASSERT(!TSchemeInterpreter::GetInstance()->IsStopped());
	
	// TODO - I wish we could do this without consing.
	Scheme_Object *args[2];
	args[0] = mCallback;
	args[1] = TValueToScheme(inArguments);
    Scheme_Object *result =
        TSchemeInterpreter::CallScheme("%kernel-run-callback", 2, args);
    return SchemeToTValue(result);
}
