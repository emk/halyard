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

#ifndef ScriptEditorDB_H
#define ScriptEditorDB_H

#include "TInterpreter.h" // for TScriptIdentifier::Type

namespace sqlite3 {
    class connection;
    class reader;
};

BEGIN_NAMESPACE_HALYARD

/// A database of information used by the script editor.
class ScriptEditorDB {
public:
    const static char *BuiltInIdentifierRelPath;

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

        std::string GetNativePath();
    };

    /// A list of definitions.
    typedef std::vector<Definition> Definitions;

    /// A list of strings.
    typedef std::vector<std::string> strings;

    // Listens for changes made to the ScriptEditorDB.
    class IListener {
    public:
        virtual ~IListener() {}
        virtual void FileChanged(const std::string &relpath) = 0;
        virtual void FileDeleted(const std::string &relpath) = 0;
    };
    
private:
    typedef std::map<std::string,time_t> ModTimeMap;

    shared_ptr<sqlite3::connection> mDB;
    bool mIsProcessingFile;
    __int64 mProcessingFileId;
    std::string mProcessingFileRelPath;
    bool mIsInTransaction;
    std::vector<IListener*> mListeners;

    void EnsureCorrectSchema();

    strings GetAllFilePaths();
    void FixBrokenDatabaseEntries();
    void DeleteAnyFileDataAndNotifyListeners(const std::string &relpath);    

    void FetchModTimesFromDatabase(ModTimeMap &outMap);
    void ScanTreeInternal(const std::string &relpath,
                          const std::string &extension,
                          const ModTimeMap &modtimes,
                          strings &outFilesToProcess);
    bool NeedsProcessingInternal(const std::string &relpath,
                                 const ModTimeMap &modtimes);
    void ProcessBuiltInIdentifiers();

protected:
    /// Transaction monitor class for ScriptEditorDB database.  You can
    /// nest instances of StScriptEditorDBTransaction, but only the outermost
    /// object will actually do anything.
    class StTransaction {
        ScriptEditorDB *mDB;
        bool mIsRunning;

    public:
        StTransaction(ScriptEditorDB *db);
        ~StTransaction();
        
        void Commit();
    };

    virtual void ProcessFileInternal(const std::string &relpath);

    Definitions ReadDefinitions(sqlite3::reader &r);

public:
    ScriptEditorDB(const std::string &db_name);
    virtual ~ScriptEditorDB();

    std::string NativeToRelPath(const std::string &relpath);
    std::string RelPathToNative(const std::string &native);

    virtual void UpdateDatabase();

    void PurgeDataForDeletedFiles();
    strings ScanTree(const std::string &relpath,
                     const std::string &extension);
    void DeleteAnyFileData(const std::string &relpath);
    bool NeedsProcessing(const std::string &relpath);
    void BeginProcessingFile(const std::string &relpath);
    void EndProcessingFile();

    virtual bool ShouldSkipDirectory(const std::string &relpath);

    void ProcessTree(const std::string &relpath,
                     const std::string &extension);

    void InsertDefinition(const std::string &name,
                          TScriptIdentifier::Type type,
                          int lineno);
    void InsertHelp(const std::string &name, const std::string &help);
    void InsertIndentation(const std::string &name, int indentation);

    Definitions FindDefinitions(const std::string &name);
    Definitions FindDefinitionsInFile(const std::string &relpath);
    strings FindHelp(const std::string &name);
    IdentifierList GetAllIdentifiers();

    /// Add a new listner to the ScriptEditorDB (if it isn't already there).
    void AddListener(IListener *inListener);

    /// Remove a listener from the ScriptEditorDB if it exists.
    void RemoveListener(IListener *inListener);
};

extern void RegisterScriptEditorDBPrimitives();

END_NAMESPACE_HALYARD

#endif // ScriptEditorDB_H
