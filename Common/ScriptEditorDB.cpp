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

/// \todo The boost::filesystem headers interact destructively with
/// MSVC++ header precompilation, so we need to go through a little
/// song-and-dance here.
#define BOOST_ALL_NO_LIB 1
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>
#undef BOOST_ALL_NO_LIB

#include "CommonHeaders.h"
//#include <boost/iterator/iterator_facade.hpp>
#include "TInterpreter.h"
#include "ScriptEditorDB.h"
#include <sqlite3_plus.h>

namespace fs = boost::filesystem;

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(ScriptEditorDB);

//=========================================================================
//  Local Helper Functions
//=========================================================================
//  We can't declare these in the header because they need access to
//  boost::filesystem headers, which break precompiled headers horribly.

namespace {

    /// Return the directory in which we should store ScriptEditorDB files.
    fs::path RootPath() {
        // We can't decide where to put these files until we have a working
        // directory, which gets set up when the interpreter is loaded.
        //
        // \todo This assertion fails when called from the test suites,
        // because there's no Scheme interpreter. This will be a problem
        // very shortly for other reasons.
        //ASSERT(TInterpreterManager::GetInstance()->InterpreterHasBegun());
        
        // Convert from our portable path library to boost's.
        std::string p(FileSystem::GetBaseDirectory().ToNativePathString());
        return fs::path(p, fs::native);
    }
};


//=========================================================================
//  ScriptEditorDB Methods
//=========================================================================

/// Create a new ScriptEditorDB.
ScriptEditorDB::ScriptEditorDB(const std::string &relpath)
    : mDB(new sqlite3::connection((RootPath()/relpath).native_file_string().c_str())),
      mIsProcessingFile(false)
{
    EnsureCorrectSchema();
}

ScriptEditorDB::~ScriptEditorDB() {
    try {
        mDB->close();
    } catch (std::exception &e) {
        gLog.Error("%s", std::string("Error closing database: ")+e.what());
    }
}

/// Make sure the database contains the table schema we want. If not,
/// delete the database and create it from scratch.
void ScriptEditorDB::EnsureCorrectSchema() {
    // Schema for the tables we want to create.
    struct schema_info {
        const char *type;
        const char *name;
        const char *sql;
    };
    schema_info schema[] = {
        {"table", "file",
         "CREATE TABLE file ("
         "  path TEXT NOT NULL,"
         "  modtime INT NOT NULL)"},
        {"table", "def",
         "CREATE TABLE def ("
         "  file_id INT NOT NULL,"
         "  name TEXT NOT NULL,"
         "  type TEXT NOT NULL,"
         "  line INT NOT NULL,"
         "  prefix_len INT,"
         "  prefix TEXT)"},
        {"table", "alias",
         "CREATE TABLE alias ("
         "  file_id INT NOT NULL,"
         "  name TEXT NOT NULL,"
		 "  points_to TEXT NOT NULL)"},
        {"table", "help",
         "CREATE TABLE help ("
         "  file_id INT NOT NULL,"
         "  name TEXT NOT NULL,"
         "  helpstr TEXT NOT NULL)"},
        {NULL, NULL, NULL}
    };

    // Check the schemas for each of our tables.
    bool schema_ok = true;
    for (schema_info *sptr = schema; sptr->name != NULL; sptr++) {
        try {
            std::string existing_sql =
                mDB->executescalar("SELECT sql FROM sqlite_master"
                                   "  WHERE tbl_name = '%q' AND type = '%q'",
                                   sptr->name, sptr->type);
            if (existing_sql != sptr->sql)
                schema_ok = false;
        } catch (std::exception &) {
            schema_ok = false;
        }
        if (schema_ok == false)
            break;
    }

    // If the schemas are OK, return immediately.
    if (schema_ok)
        return;

    // Drop everything we've got.
    for (schema_info *sptr = schema; sptr->name != NULL; sptr++) {
        try {
            mDB->executenonquery("DROP %s %s", sptr->type, sptr->name);
        } catch (std::exception &) {
            // Ignore failure to delete--the table may not be there.
        }
    }

    // Reclaim disk space.
    mDB->executenonquery("VACUUM");

    // Insert our new tables.
    for (schema_info *sptr = schema; sptr->name != NULL; sptr++) {
        mDB->executenonquery(sptr->sql);
    }
}

/// Remove all database entries derived from files which have been
/// deleted.
void ScriptEditorDB::PurgeDataForDeletedFiles() {
    // Get the relpaths for all files in the database.
    sqlite3::reader r = mDB->executereader("SELECT path FROM file");
    strings relpaths;
    while (r.read())
        relpaths.push_back(r.getstring(0));
    r.close();

    // If a given path no longer points to a file, purge any related data.
    strings::iterator i = relpaths.begin();
    for (; i != relpaths.end(); ++i) {
        fs::path path(RootPath() / *i);
        if (!fs::exists(path)) 
            DeleteAnyFileData(*i);
    }
}

/// Scan the specified directory tree, and find all files that end with the
/// specified extension and that need to be processed. Also, clean up any
/// database entries corresponding to files which have been deleted.
///
/// \return A list of files which need to be processed.
ScriptEditorDB::strings ScriptEditorDB::ScanTree(const std::string &relpath,
                                                 const std::string &extension)
{
    strings result;

    fs::path path(RootPath()/relpath);
    fs::directory_iterator i(path);
    ASSERT(fs::exists(path) && fs::is_directory(path));
	for (; i != fs::directory_iterator(); ++i) {
        // Don't process non-existant files, and run screaming from
        // symlinks.  (The documentation suggests that we check "exists".
        // I'm not sure why, but I'm definitely scared.)
        if (!fs::exists(*i) || fs::symbolic_link_exists(*i))
            continue;
        
        std::string child_relpath(relpath + "/" + i->leaf());
        if (fs::is_directory(*i)) {
            // Scan the sub-tree and copy its result into our result.
            // Yes, we know that this is inefficient, but we're too lazy
            // to write a helper function--and since we're doing file I/O,
            // CPU time is completely irrelevant compared to disk seek
            // time.
            strings sub_tree = ScanTree(child_relpath, extension);
            result.insert(result.end(), sub_tree.begin(), sub_tree.end());
        } else if (fs::extension(*i) == extension) {
            // OK, this is probably a regular file, and it has the right
            // extension, so we'll pretend it's not (say) a badly named
            // device node under /dev.
            if (NeedsProcessing(child_relpath))
                result.push_back(child_relpath);
        }
    }

    return result;
}

/// Delete all the old data associated with the file, if any.
void ScriptEditorDB::DeleteAnyFileData(const std::string &relpath) {
    sqlite3::reader r =
        mDB->executereader("SELECT rowid FROM file WHERE path = '%q'",
                           relpath.c_str());

    // Read all the row IDs. We can't start any new queries while this
    // loop is running; we'll do that below.
    std::vector<__int64> rowids;
    while (r.read()) {
        // sqlite represents 64-bit integers as __int64. 
        rowids.push_back(r.getint64(0));
    }
    r.close();

    // Delete all data associated with each of this file's row_ids.
    std::vector<__int64>::iterator i = rowids.begin();
    for (; i != rowids.end(); ++i) {
        // sqlite uses the format string '%lld' to pass __int64 values back
        // to the database.
        mDB->executenonquery("DELETE FROM def WHERE file_id = %lld", *i);
        mDB->executenonquery("DELETE FROM file WHERE rowid = %lld", *i);
    }

    // We shouldn't have multiple file entries in the database.
    ASSERT(rowids.size() <= 1);
}

bool ScriptEditorDB::NeedsProcessing(const std::string &relpath) {

	fs::path path(RootPath()/relpath);

    if (!fs::exists(path))
        throw fs::filesystem_error("ScriptEditorDB::NeedsProcessing",
                                   relpath, fs::not_found_error);

    sqlite3::reader r = 
        mDB->executereader("SELECT modtime FROM file"
                           "  WHERE path = '%q'", relpath.c_str());

    if (r.read()) {
        time_t modtime = static_cast<time_t>(r.getint64(0));
        return (modtime < fs::last_write_time(path));
    } else {
        return true;
    }
}

void ScriptEditorDB::BeginProcessingFile(const std::string &relpath) {
    ASSERT(!mIsProcessingFile);
    DeleteAnyFileData(relpath);

    // Insert a new entry in the 'file' table.
    fs::path path(RootPath()/relpath);
    __int64 modtime = static_cast<__int64>(fs::last_write_time(path));
    mDB->executenonquery("INSERT INTO file VALUES ('%q', %lld)",
                        relpath.c_str(), modtime);

    // Remember the ID for this file.
    mIsProcessingFile = true;
    mProcessingFileId = mDB->insertid();
}

void ScriptEditorDB::EndProcessingFile() {
    ASSERT(mIsProcessingFile);
    mIsProcessingFile = false;
}

/// For each unprocessed file in the specified directory tree (with the
/// specified extension), call ProcessFileInternal.  You'll definitely want
/// to override the default ProcessFileInternal.
///
/// \see ProcessFileInternal
void ScriptEditorDB::ProcessTree(const std::string &relpath,
                                 const std::string &extension)
{
    mDB->executenonquery("BEGIN TRANSACTION");
    try {

        strings files = ScanTree(relpath, extension);
        for (strings::iterator i = files.begin(); i != files.end(); ++i) {
            BeginProcessingFile(*i);
            ProcessFileInternal(*i);
            EndProcessingFile();
        }

    } catch (...) {
        mDB->executenonquery("ROLLBACK TRANSACTION");
        throw;
    }
    mDB->executenonquery("COMMIT TRANSACTION");
}

/// This function is called by ProcessTree.  You'll probably want to
/// override the default implementation, which does nothing.
///
/// \see ProcessTree
void ScriptEditorDB::ProcessFileInternal(const std::string &relpath) {
}

void ScriptEditorDB::InsertDefinition(const std::string &name,
                                      TScriptIdentifier::Type type,
                                      int lineno)
{
    ASSERT(mIsProcessingFile);
    mDB->executenonquery("INSERT INTO def VALUES(%lld,'%q',%d,%d,NULL,NULL)",
                         mProcessingFileId, name.c_str(),
                         static_cast<int>(type), lineno);
}

ScriptEditorDB::Definitions
ScriptEditorDB::FindDefinitions(const std::string &name) {
    Definitions result;
    
    // Look up the definition in the database.
    sqlite3::reader r =
        mDB->executereader("SELECT path,name,type,line FROM file,def"
                           "  WHERE file.rowid = file_id AND name = '%q'",
                           name.c_str());

    // Add each row returned by the query to our result.
    while (r.read()) {
        Definition d(r.getstring(0), r.getstring(1),
                     static_cast<TScriptIdentifier::Type>(r.getint32(2)),
                     r.getint32(3));
        result.push_back(d);
    }

    return result;
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

#define CHECK_DEF(DEF,PATH,NAME,TYPE,LINE) \
    CHECK_EQ((DEF).file_path, (PATH)); \
    CHECK_EQ((DEF).name, (NAME)); \
    CHECK_EQ((DEF).type, (TYPE)); \
    CHECK_EQ((DEF).line_number, (LINE));

BEGIN_TEST_CASE(TestScriptEditorDB, TestCase) {
    // Get a database.
    ScriptEditorDB db("test.sqlite3");

    // Check to make sure the file exists.
	fs::path db_path(RootPath()/"test.sqlite3");
	CHECK_EQ(fs::exists(db_path), true);
    CHECK_EQ(fs::is_directory(db_path), false);

    // Process a file.
    db.BeginProcessingFile("Scripts/indexed.ss");
    db.InsertDefinition("foo", TScriptIdentifier::FUNCTION, 19);
    db.InsertDefinition("bar", TScriptIdentifier::VARIABLE, 20);
    db.InsertDefinition("bar", TScriptIdentifier::FUNCTION, 21);
    db.EndProcessingFile();

    // Process another file.
    db.BeginProcessingFile("Scripts/indexed2.ss");
    db.InsertDefinition("baz", TScriptIdentifier::FUNCTION, 43);
    db.EndProcessingFile();
    
    // Make sure that the library doesn't purge data for *undeleted* files.
    // We should really test that this function works correctly for *deleted*
    // files, but that involves more C++ I/O than I want to get involved
    // with this morning.
    db.PurgeDataForDeletedFiles();

    // Look up the data we entered.
    ScriptEditorDB::Definitions foo_defs = db.FindDefinitions("foo");
    CHECK_EQ(foo_defs.size(), 1);
    CHECK_DEF(foo_defs[0], "Scripts/indexed.ss", "foo",
              TScriptIdentifier::FUNCTION, 19);

    ScriptEditorDB::Definitions bar_defs = db.FindDefinitions("bar");
    CHECK_EQ(bar_defs.size(), 2);
    CHECK_DEF(bar_defs[0], "Scripts/indexed.ss", "bar",
              TScriptIdentifier::VARIABLE, 20);
    CHECK_DEF(bar_defs[1], "Scripts/indexed.ss", "bar",
              TScriptIdentifier::FUNCTION, 21);

    ScriptEditorDB::Definitions baz_defs = db.FindDefinitions("baz");
    CHECK_EQ(baz_defs.size(), 1);
    CHECK_DEF(baz_defs[0], "Scripts/indexed2.ss", "baz",
              TScriptIdentifier::FUNCTION, 43);
    
    // This file has just been processed.
    CHECK_EQ(db.NeedsProcessing("Scripts/indexed.ss"), false);

    // Files which don't appear in the database need to be processed...
    db.DeleteAnyFileData("Scripts/indexed3.ss");
    CHECK_THROWN(std::exception, db.NeedsProcessing("Scripts/nosuch.ss"));
    CHECK_EQ(db.NeedsProcessing("Scripts/indexed3.ss"), true);

    // ...but after we process them, they're good.
    db.BeginProcessingFile("Scripts/indexed3.ss");
    db.EndProcessingFile();
    CHECK_EQ(db.NeedsProcessing("Scripts/indexed3.ss"), false);

    // If the file is modified, it should once again need processing.
    fs::path index3_path(RootPath()/"Scripts/indexed3.ss");
    fs::last_write_time(index3_path, std::time_t());
    CHECK_EQ(db.NeedsProcessing("Scripts/indexed3.ss"), true);

    // Scan a tree and look for unprocessed files.
    std::vector<std::string> unprocessed =
        db.ScanTree("Scripts/scantest", ".ss");
    std::sort(unprocessed.begin(), unprocessed.end());
    CHECK_EQ(unprocessed.size(), 3);
    CHECK_EQ(unprocessed[0], "Scripts/scantest/a.ss");
    CHECK_EQ(unprocessed[1], "Scripts/scantest/b.ss");
    CHECK_EQ(unprocessed[2], "Scripts/scantest/subdir/c.ss");

    // Make sure that ScanTree doesn't find processed files.
    unprocessed = db.ScanTree("Scripts", ".ss");
    CHECK_EQ(std::find(unprocessed.begin(), unprocessed.end(),
                       "Scripts/indexed.ss") == unprocessed.end(),
             true);
    
} END_TEST_CASE(TestScriptEditorDB);

#endif // BUILD_TEST_CASES

