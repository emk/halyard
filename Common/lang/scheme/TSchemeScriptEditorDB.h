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

#ifndef TSchemeScriptEditorDB_H
#define TSchemeScriptEditorDB_H

#include "ScriptEditorDB.h"

BEGIN_NAMESPACE_FIVEL

void RegisterSchemeScriptEditorDBPrimitives();

class TSchemeScriptEditorDB : public ScriptEditorDB {
protected:
    virtual void ProcessFileInternal(const std::string &relpath);    

public:
    TSchemeScriptEditorDB(const std::string &db_path)
        : ScriptEditorDB(db_path) {}
};

END_NAMESPACE_FIVEL

#endif // TSchemeScriptEditorDB_H
