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
#include "TSchemeInterpreter.h"
#include "TSchemeConv.h"

using namespace Halyard;


//=========================================================================
//  TSchemeCallback Methods
//=========================================================================

TValue TSchemeCallback::Run(const TValueList &inArguments)
{

    // Make sure we have a Scheme interpreter and that it isn't stopped.
    ASSERT(TSchemeInterpreter::HaveInstance());
    ASSERT(!TSchemeInterpreter::GetInstance()->IsStopped());

    Scheme_Object *result = NULL;
    TSchemeArgs<2> args;

    TSchemeReg<1,1> reg;
    reg.local(result);
    reg.args(args);
    reg.done();

    // TODO - I wish we could do this without consing, at least in the case
    // where we have simple arguments.
    args[0] = mCallback;
    args[1] = TValueToScheme(inArguments);
    result = TSchemeInterpreter::CallSchemeStatic("%kernel-run-callback",
                                                  args.size(), args.get());
    return SchemeToTValue(result);
}
