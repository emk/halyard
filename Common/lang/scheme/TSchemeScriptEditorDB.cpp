// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
#include "TSchemeScriptEditorDB.h"
#include "TSchemeInterpreter.h"

using namespace Halyard;


//=========================================================================
//  TSchemeScriptEditorDB Methods
//=========================================================================

void TSchemeScriptEditorDB::UpdateDatabase() {
    if (TInterpreter::HaveInstance()) {
        StTransaction transaction(this);
        
        ScriptEditorDB::UpdateDatabase();
        ProcessTree("scripts", ".ss");
        ProcessTree("collects", ".ss");
        ProcessTree("runtime", ".ss");
        
        transaction.Commit();
    }
}

void TSchemeScriptEditorDB::ProcessFileInternal(const std::string &path) {
    TSchemeArgs<1> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();

	args[0] = scheme_make_path(path.c_str());
	TSchemeInterpreter::CallSchemeStatic("%kernel-extract-definitions",
                                         args.size(), args.get());
}
