// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

USING_NAMESPACE_FIVEL


//=========================================================================
//  RegisterSchemeScriptEditorDBPrimitives
//=========================================================================
//  Install our portable primitive functions.

void FIVEL_NS RegisterSchemeScriptEditorDBPrimitives()
{
	REGISTER_5L_PRIMITIVE(ScriptEditorDBInsertDef);
}


//=========================================================================
//  TSchemeInterpreterDB Methods
//=========================================================================

DEFINE_5L_PRIMITIVE(ScriptEditorDBInsertDef) {

    std::string name;
    std::string type_name;
    int32 lineno;

    inArgs >> SymbolName(name) >> SymbolName(type_name) >> lineno;

    ScriptEditorDB *db =
        TInterpreterManager::GetInstance()->GetScriptEditorDB();
    TScriptIdentifier::Type type =
        TSchemeInterpreter::IdentifierType(type_name);
    db->InsertDefinition(name, type, lineno);
}


//=========================================================================
//  TSchemeInterpreterDB Methods
//=========================================================================

void TSchemeScriptEditorDB::ProcessFileInternal(const std::string &relpath) {
	Scheme_Object *args[1];
	args[0] = scheme_make_string(relpath.c_str());
	TSchemeInterpreter::CallScheme("%kernel-extract-definitions", 1, args);
}
