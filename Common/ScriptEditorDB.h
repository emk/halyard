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

#ifndef ScriptEditorDB_H
#define ScriptEditorDB_H

#include "TInterpreter.h" // for TScriptIdentifier::Type

namespace sqlite3 {
    class connection;
};

BEGIN_NAMESPACE_FIVEL

/// A database of information used by the script editor.
class ScriptEditorDB {
public:
    /// Information about a definition of an indentifier in a script.
    /// \todo Merge this with TScriptIdentifier.
    struct Definition {
        std::string file_path;
        std::string name;
        TScriptIdentifier::Type type;
        int line_number;

        Definition(const std::string &inFilePath,
                   const std::string &inName,
                   TScriptIdentifier::Type inType,
                   int inLineNumber)
            : file_path(inFilePath), name(inName),
              type(inType), line_number(inLineNumber) {}
    };

    /// A list of definitions.
    typedef std::vector<Definition> Definitions;

    /// A list of strings.
    typedef std::vector<std::string> strings;

private:
    shared_ptr<sqlite3::connection> mDB;
    bool mIsProcessingFile;
    __int64 mProcessingFileId;

    void EnsureCorrectSchema();

protected:
    virtual void ProcessFileInternal(const std::string &relpath);

public:
    ScriptEditorDB(const std::string &relpath);
    ~ScriptEditorDB();

    void PurgeDataForDeletedFiles();
    strings ScanTree(const std::string &relpath,
                     const std::string &extension);
    void DeleteAnyFileData(const std::string &relpath);
    bool NeedsProcessing(const std::string &relpath);
    void BeginProcessingFile(const std::string &relpath);
    void EndProcessingFile();

    void ProcessTree(const std::string &relpath,
                     const std::string &extension);

    void InsertDefinition(const std::string &name,
                          TScriptIdentifier::Type type,
                          int lineno);

    Definitions FindDefinitions(const std::string &name);
};

END_NAMESPACE_FIVEL

#endif // ScriptEditorDB_H
