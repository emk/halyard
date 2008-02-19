// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
//  ScriptEditorDB::Definition Methods
//=========================================================================

std::string ScriptEditorDB::Definition::GetNativePath() {
    return (RootPath()/file_path).native_file_string();
}

//=========================================================================
//  StTransaction Methods
//=========================================================================

ScriptEditorDB::StTransaction::StTransaction(
    ScriptEditorDB *db)
    : mDB(db), mIsRunning(!mDB->mIsInTransaction)
{
    if (mIsRunning) {
        mDB->mDB->executenonquery("BEGIN TRANSACTION");    
        mDB->mIsInTransaction = true;
    }
}

ScriptEditorDB::StTransaction::~StTransaction() {
    if (mIsRunning) {
        mDB->mDB->executenonquery("ROLLBACK TRANSACTION");
        mDB->mIsInTransaction = false;
    }
}
        
void ScriptEditorDB::StTransaction::Commit() {
    if (mIsRunning) {
        mDB->mDB->executenonquery("COMMIT TRANSACTION");
        mDB->mIsInTransaction = false;
        mIsRunning = false;
    }
}


//=========================================================================
//  ScriptEditorDB Methods
//=========================================================================

/// Create a new ScriptEditorDB.
ScriptEditorDB::ScriptEditorDB(const std::string &relpath)
    : mDB(new sqlite3::connection((RootPath()/relpath).native_file_string().c_str())),
      mIsProcessingFile(false), mIsInTransaction(false)
{
    EnsureCorrectSchema();
}

ScriptEditorDB::~ScriptEditorDB() {
    ASSERT(!mIsInTransaction);
    try {
        mDB->close();
    } catch (std::exception &e) {
        gLog.Error("%s", std::string("Error closing database: ")+e.what());
    }
}

/// Make sure the database contains the table schema we want. If not,
/// delete the database and create it from scratch.
void ScriptEditorDB::EnsureCorrectSchema() {
    StTransaction transaction(this);

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
        {"index", "file_path",
         "CREATE INDEX file_path ON file(path)"},
        {"table", "def",
         "CREATE TABLE def ("
         "  file_id INT NOT NULL,"
         "  name TEXT NOT NULL,"
         "  type TEXT NOT NULL,"
         "  line INT NOT NULL,"
         "  prefix_len INT,"
         "  prefix TEXT)"},
        {"index", "def_file_id",
         "CREATE INDEX def_file_id ON def(file_id)"},
        {"index", "def_name",
         "CREATE INDEX def_name ON def(name)"},
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
        {"index", "help_file_id",
         "CREATE INDEX help_file_id ON help(file_id)"},
        {"index", "help_name",
         "CREATE INDEX help_name ON help(name)"},
        {NULL, NULL, NULL}
    };

    // Check the schemas for each of our tables.
    bool schema_ok = true;
    for (schema_info *sptr = schema; sptr->name != NULL; sptr++) {
        try {
            std::string existing_sql =
                mDB->executescalar("SELECT sql FROM sqlite_master"
                                   "  WHERE name = '%q' AND type = '%q'",
                                   sptr->name, sptr->type);
            if (existing_sql != sptr->sql)
                schema_ok = false;
        } catch (std::exception &) {
            schema_ok = false;
        }
        if (schema_ok == false)
            break;
    }

    transaction.Commit();

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

/// Convert a relative path to a native OS path.
std::string ScriptEditorDB::RelPathToNative(const std::string &relpath) {
    return (RootPath()/relpath).native_file_string().c_str();
}

/// Convert a native OS path to a relative path.  Returns the empty string
/// if the conversion fails.
std::string ScriptEditorDB::NativeToRelPath(const std::string &native) {
    fs::path root_path(RootPath());
    fs::path native_path(native, fs::native);
    std::string relpath;
    
    // Strip successive leaves off of native_path until it either
    // matches root_path or has nothing left to strip.
    while (root_path.string() != native_path.string() && !native_path.empty())
    {
        std::string leaf = native_path.leaf();
        native_path = native_path.branch_path();
        if (relpath.empty())
            relpath = leaf;
        else
            relpath = leaf + "/" + relpath;
    }

    // If we're in the right directory, tear off a 
    if (root_path.string() == native_path.string())
        return relpath;
    else
        return "";
}

/// The primary interface for updating the database.  This will generally
/// be overriden by subclasses.
void ScriptEditorDB::UpdateDatabase() {
    FixBrokenDatabaseEntries();
    PurgeDataForDeletedFiles();
}

/// Return the relpath to each file in our database.
ScriptEditorDB::strings ScriptEditorDB::GetAllFilePaths() {
    // Get the relpaths for all files in the database.
    sqlite3::reader r = mDB->executereader("SELECT path FROM file");
    strings relpaths;
    while (r.read())
        relpaths.push_back(r.getstring(0));
    r.close();
    return relpaths;
}

// Slow, ugly comparison functions for use by FixBrokenDatabaseEntries.  We
// really shouldn't call MakeStringLowercase each time.
namespace {
    bool icase_less(const std::string &a, const std::string &b) {
        return (MakeStringLowercase(a) < MakeStringLowercase(b));
    }

    bool icase_equal_to(const std::string &a, const std::string &b) {
        return (MakeStringLowercase(a) == MakeStringLowercase(b));
    }
};

/// This function is used to clean up after the mess created when someone
/// renames 'R1.ss' to 'r1.ss' on a case-insensitive file system.  This
/// function is a nasty kludge, and we should really be able to detect
/// these better elsewhere.  At least we don't do any file I/O to detect
/// duplicates (even a very slow memory-only function will outperform even
/// the most optimized function that touches a disk).
///
/// This function doesn't actually catch duplicates until the first update
/// *after* the duplicates are created, so it's definitely a half-baked
/// measure.
///
/// PORTABILITY - Do not run on case-sensitive file systems (such as Unix).
void ScriptEditorDB::FixBrokenDatabaseEntries() {
    // Get all the paths in our database, and sort them case-insensitively.
    strings relpaths = GetAllFilePaths();
    std::sort(relpaths.begin(), relpaths.end(), icase_less);

    // Look for duplicate entries, and delete them.
    strings::iterator r_begin = relpaths.begin();
    while (r_begin != relpaths.end()) {
        // Extend r_end until [r_begin,r_end) contains a complete set
        // of duplicates.
        strings::iterator r_end = r_begin + 1;
        while (r_end != relpaths.end() && icase_equal_to(*r_begin, *r_end))
            ++r_end;

        // If [r_begin,r_end) contains more than one item, delete them all.
        if ((r_end - r_begin) > 1)
            for (strings::iterator i = r_begin; i != r_end; ++i)
                DeleteAnyFileDataAndNotifyListeners(*i);

        // Skip to the end of our set of duplicates.
        r_begin = r_end;
    }
}

/// Remove all database entries derived from files which have been
/// deleted.
void ScriptEditorDB::PurgeDataForDeletedFiles() {
    strings relpaths = GetAllFilePaths();

    // If a given path no longer points to a file, purge any related data.
    strings::iterator i = relpaths.begin();
    for (; i != relpaths.end(); ++i) {
        fs::path path(RootPath() / *i);
        if (!fs::exists(path))
            DeleteAnyFileDataAndNotifyListeners(*i);
    }
}

/// Get all the modification times stored in the database, and copy them
/// into outMap.  
void ScriptEditorDB::FetchModTimesFromDatabase(ModTimeMap &outMap) {
    sqlite3::reader r = mDB->executereader("SELECT path,modtime FROM file");
    while (r.read()) {
        time_t modtime = static_cast<time_t>(r.getint64(1));
        ASSERT(outMap.find(r.getstring(0)) == outMap.end());
        outMap.insert(ModTimeMap::value_type(r.getstring(0), modtime));
    }
}

/// The recursive guts of ScanTree.
void ScriptEditorDB::ScanTreeInternal(const std::string &relpath,
                                      const std::string &extension,
                                      const ModTimeMap &modtimes,
                                      strings &outFilesToProcess)
{
    // We need to use fs::no_check here because this path contains
    // nasty platform-specific characters.
    //
    // PORTABILITY - Make sure that this actually works.
    fs::path path(RootPath()/fs::path(relpath, fs::no_check));
    if (fs::symbolic_link_exists(path)) {
        // Don't follow symlinks.
    } else if (fs::is_directory(path)) {
        if (!ShouldSkipDirectory(relpath)) {
            // Recursively scan everything in this directory.
            fs::directory_iterator i(path);
            for (; i != fs::directory_iterator(); ++i) {
                std::string child_relpath(relpath + "/" + i->leaf());
                ScanTreeInternal(child_relpath, extension, modtimes,
                                 outFilesToProcess);
            }
        }
    } else if (fs::extension(path) == extension) {
        // OK, this is probably a regular file, and it has the right
        // extension, so we'll pretend it's not (say) a badly named
        // device node under /dev.
        if (NeedsProcessingInternal(relpath, modtimes))
            outFilesToProcess.push_back(relpath);
    }
}

/// Scan the specified directory tree, and find all files that end with the
/// specified extension and that need to be processed.
///
/// \return A list of files which need to be processed.
ScriptEditorDB::strings ScriptEditorDB::ScanTree(const std::string &relpath,
                                                 const std::string &extension)
{
    strings result;
    ModTimeMap modtimes;

    FetchModTimesFromDatabase(modtimes);    
    StTransaction transaction(this);
    ScanTreeInternal(relpath, extension, modtimes, result);    
    transaction.Commit();

    return result;
}

/// Delete all the old data associated with the file, if any, and notify
/// ScriptEditorDB listeners that the file is gone.
void
ScriptEditorDB::DeleteAnyFileDataAndNotifyListeners(const std::string &relpath)
{
    DeleteAnyFileData(relpath);
    
    // Notify any listeners that this file has been deleted.
    std::vector<IListener*>::iterator i = mListeners.begin();
    for (; i != mListeners.end(); ++i)
        (*i)->FileDeleted(relpath); 
}

/// Delete all the old data associated with the file, if any.
void ScriptEditorDB::DeleteAnyFileData(const std::string &relpath) {
    StTransaction transaction(this);

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
        mDB->executenonquery("DELETE FROM help WHERE file_id = %lld", *i);
        mDB->executenonquery("DELETE FROM file WHERE rowid = %lld", *i);
    }

    // We shouldn't have multiple file entries in the database.
    ASSERT(rowids.size() <= 1);
    transaction.Commit();
}

bool ScriptEditorDB::NeedsProcessingInternal(const std::string &relpath,
                                             const ModTimeMap &modtimes)
{
	fs::path path(RootPath()/relpath);
	time_t new_modtime = fs::last_write_time(path);

    ModTimeMap::const_iterator found = modtimes.find(relpath);
    if (found != modtimes.end()) {
        time_t old_modtime = found->second;
        return (old_modtime != new_modtime);
    } else {
        return true;
    }
}

bool ScriptEditorDB::NeedsProcessing(const std::string &relpath) {
    ModTimeMap modtimes;
    FetchModTimesFromDatabase(modtimes);
    return NeedsProcessingInternal(relpath, modtimes);
}

void ScriptEditorDB::BeginProcessingFile(const std::string &relpath) {
    ASSERT(!mIsProcessingFile);
    DeleteAnyFileData(relpath);

    // Insert a new entry in the 'file' table.
    fs::path path(RootPath()/relpath);
    __int64 modtime = static_cast<__int64>(fs::last_write_time(path));
    mDB->executenonquery("INSERT INTO file VALUES ('%q', %lld)",
                        relpath.c_str(), modtime);

    // Remember the ID & relpath for this file.
    mIsProcessingFile = true;
    mProcessingFileId = mDB->insertid();
    mProcessingFileRelPath = relpath;
}

void ScriptEditorDB::EndProcessingFile() {
    ASSERT(mIsProcessingFile);
    mIsProcessingFile = false;

    // Notify any listeners that this file has been processed.
    std::vector<IListener*>::iterator i = mListeners.begin();
    for (; i != mListeners.end(); ++i)
        (*i)->FileChanged(mProcessingFileRelPath); 
}

bool ScriptEditorDB::ShouldSkipDirectory(const std::string &relpath) {
    std::string dirname(fs::path(relpath, fs::no_check).leaf());
    // TODO - Really, "compiled" is the responsibility of our Scheme-based
    // subclass to deal with, but that's a lot of work right now.
    if (dirname == ".svn" || dirname == "CVS" || dirname == "compiled")
        return true;
    return false;
}

/// For each unprocessed file in the specified directory tree (with the
/// specified extension), call ProcessFileInternal.  You'll definitely want
/// to override the default ProcessFileInternal.
///
/// \see ProcessFileInternal
void ScriptEditorDB::ProcessTree(const std::string &relpath,
                                 const std::string &extension)
{
    StTransaction transaction(this);

    strings files = ScanTree(relpath, extension);
    for (strings::iterator i = files.begin(); i != files.end(); ++i) {
        BeginProcessingFile(*i);
        ProcessFileInternal(*i);
        EndProcessingFile();
    }
    
    transaction.Commit();
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

void ScriptEditorDB::InsertHelp(const std::string &name, 
                                const std::string &help)
{
    ASSERT(mIsProcessingFile);
    mDB->executenonquery("INSERT INTO help VALUES(%lld, '%q', '%q')",
                         mProcessingFileId, name.c_str(), help.c_str());
}

ScriptEditorDB::Definitions
ScriptEditorDB::ReadDefinitions(sqlite3::reader &r) {
    // Add each row returned by the query to our result.
    Definitions result;
    while (r.read()) {
        Definition d(r.getstring(0), r.getstring(1),
                     static_cast<TScriptIdentifier::Type>(r.getint32(2)),
                     r.getint32(3));
        result.push_back(d);
    }
    return result;
}

ScriptEditorDB::Definitions
ScriptEditorDB::FindDefinitions(const std::string &name) {
    // Look up the definition in the database.
    sqlite3::reader r =
        mDB->executereader("SELECT path,name,type,line FROM file,def"
                           "  WHERE file.rowid = file_id AND name = '%q'",
                           name.c_str());
    return ReadDefinitions(r);
}

ScriptEditorDB::Definitions
ScriptEditorDB::FindDefinitionsInFile(const std::string &relpath) {
    // Look up the definition in the database.
    sqlite3::reader r =
        mDB->executereader("SELECT path,name,type,line FROM file,def"
                           "  WHERE file.rowid = file_id AND path = '%q'"
                           "  ORDER BY line",
                           relpath.c_str());
    return ReadDefinitions(r);
}

ScriptEditorDB::strings ScriptEditorDB::FindHelp(const std::string &name) {
    strings result;
    
    // Look up the help strings in the database.
    sqlite3::reader r =
        mDB->executereader("SELECT helpstr FROM help WHERE name = '%q'",
                           name.c_str());
    
    // Add each row returned by the query to our result.
    while (r.read()) 
        result.push_back(r.getstring(0));
    
    return result;
}

void ScriptEditorDB::AddListener(IListener *inListener) {
    // Adapted from Model::RegisterView, because the template grik was
    // too ugly to refactor.
    if (std::count(mListeners.begin(), mListeners.end(), inListener) == 0)
		mListeners.push_back(inListener);

    StTransaction transaction(this);

    // Extract a list of all the files in the database.
    sqlite3::reader r = mDB->executereader("SELECT path FROM file");
    std::vector<std::string> files;
    while (r.read())
        files.push_back(r.getstring(0));
    r.close();

    // Send FileChanged messages for each file in the database.
	for (std::vector<std::string>::iterator i = files.begin();
		 i != files.end(); ++i)
        inListener->FileChanged(*i);
}

void ScriptEditorDB::RemoveListener(IListener *inListener) {
    // Adapted from Model::RegisterView, because the template grik was
    // too ugly to refactor.
	std::vector<IListener*>::iterator found =
		std::find(mListeners.begin(), mListeners.end(), inListener);
	if (found != mListeners.end())
        mListeners.erase(found);
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
    db.BeginProcessingFile("TestScripts/indexed.ss");
    db.InsertDefinition("foo", TScriptIdentifier::FUNCTION, 19);
    db.InsertHelp("foo", "(foo x)");
    db.InsertHelp("foo", "(foo y)");
    db.InsertDefinition("bar", TScriptIdentifier::VARIABLE, 20);
    db.InsertDefinition("bar", TScriptIdentifier::FUNCTION, 21);
    db.EndProcessingFile();

    // Process another file.
    db.BeginProcessingFile("TestScripts/indexed2.ss");
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
    CHECK_DEF(foo_defs[0], "TestScripts/indexed.ss", "foo",
              TScriptIdentifier::FUNCTION, 19);

    std::vector<std::string> foo_help(db.FindHelp("foo"));
    CHECK_EQ(foo_help.size(), 2);
    CHECK_EQ(foo_help[0], "(foo x)");
    CHECK_EQ(foo_help[1], "(foo y)");

    ScriptEditorDB::Definitions bar_defs = db.FindDefinitions("bar");
    CHECK_EQ(bar_defs.size(), 2);
    CHECK_DEF(bar_defs[0], "TestScripts/indexed.ss", "bar",
              TScriptIdentifier::VARIABLE, 20);
    CHECK_DEF(bar_defs[1], "TestScripts/indexed.ss", "bar",
              TScriptIdentifier::FUNCTION, 21);

    ScriptEditorDB::Definitions baz_defs = db.FindDefinitions("baz");
    CHECK_EQ(baz_defs.size(), 1);
    CHECK_DEF(baz_defs[0], "TestScripts/indexed2.ss", "baz",
              TScriptIdentifier::FUNCTION, 43);
    
    // This file has just been processed.
    CHECK_EQ(db.NeedsProcessing("TestScripts/indexed.ss"), false);

    // Files which don't appear in the database need to be processed...
    db.DeleteAnyFileData("TestScripts/indexed3.ss");
    CHECK_THROWN(std::exception, db.NeedsProcessing("TestScripts/nosuch.ss"));
    CHECK_EQ(db.NeedsProcessing("TestScripts/indexed3.ss"), true);

    // ...but after we process them, they're good.
    db.BeginProcessingFile("TestScripts/indexed3.ss");
    db.EndProcessingFile();
    CHECK_EQ(db.NeedsProcessing("TestScripts/indexed3.ss"), false);

    // If the file is modified, it should once again need processing.
    fs::path index3_path(RootPath()/"TestScripts/indexed3.ss");
    fs::last_write_time(index3_path, std::time_t());
    CHECK_EQ(db.NeedsProcessing("TestScripts/indexed3.ss"), true);

    // Scan a tree and look for unprocessed files.
    std::vector<std::string> unprocessed =
        db.ScanTree("TestScripts/scantest", ".ss");
    std::sort(unprocessed.begin(), unprocessed.end());
    CHECK_EQ(unprocessed.size(), 3);
    CHECK_EQ(unprocessed[0], "TestScripts/scantest/a.ss");
    CHECK_EQ(unprocessed[1], "TestScripts/scantest/b.ss");
    CHECK_EQ(unprocessed[2], "TestScripts/scantest/subdir/c.ss");

    // Make sure that ScanTree doesn't find processed files.
    unprocessed = db.ScanTree("TestScripts", ".ss");
    CHECK_EQ(std::find(unprocessed.begin(), unprocessed.end(),
                       "TestScripts/indexed.ss") == unprocessed.end(),
             true);
    
} END_TEST_CASE(TestScriptEditorDB);

#endif // BUILD_TEST_CASES

