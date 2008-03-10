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
#include "TSchemeScriptEditorDB.h"
#include "TSchemeInterpreter.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  RegisterSchemeScriptEditorDBPrimitives
//=========================================================================
//  Install our portable primitive functions.

void FIVEL_NS RegisterSchemeScriptEditorDBPrimitives()
{
	REGISTER_PRIMITIVE(ScriptEditorDBInsertDef);
    REGISTER_PRIMITIVE(ScriptEditorDBInsertHelp);
}


//=========================================================================
//  TSchemeInterpreterDB Methods
//=========================================================================

DEFINE_PRIMITIVE(ScriptEditorDBInsertDef) {
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

DEFINE_PRIMITIVE(ScriptEditorDBInsertHelp) {
    std::string name, help_string;
    inArgs >> SymbolName(name) >> help_string;

    ScriptEditorDB *db =
        TInterpreterManager::GetInstance()->GetScriptEditorDB();
    db->InsertHelp(name, help_string);
}


//=========================================================================
//  TSchemeInterpreterDB Methods
//=========================================================================

void TSchemeScriptEditorDB::UpdateDatabase() {
    if (TInterpreter::HaveInstance()) {
        StTransaction transaction(this);
        
        ScriptEditorDB::UpdateDatabase();
        ProcessTree("Runtime", ".ss");
        ProcessTree(FileSystem::GetScriptsDirectoryName(), ".ss");
        
        transaction.Commit();
    }
}

void TSchemeScriptEditorDB::ProcessFileInternal(const std::string &relpath) {
	Scheme_Object *args[1];
	args[0] = scheme_make_path(relpath.c_str());
	TSchemeInterpreter::CallScheme("%kernel-extract-definitions", 1, args);
}
