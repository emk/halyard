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

#define BOOST_ALL_NO_LIB 1
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#undef BOOST_ALL_NO_LIB
#include "CommonHeaders.h"

//#include "CommonHeaders.h"
//#include <boost/filesystem/config.hpp>
//#include <boost/filesystem/path.hpp>
#include <boost/iterator/iterator_facade.hpp>
//#include <boost/filesystem/operations.hpp>
//#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <sqlite3_plus.h>

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(ScriptEditorDB);

/// A database of information used by the script editor.
class ScriptEditorDB {
    sqlite3::connection mDB;

    void EnsureCorrectSchema();

public:
    ScriptEditorDB(const std::string &relpath);
    ~ScriptEditorDB();

    static boost::filesystem::path RootPath();
};

/// Create a new ScriptEditorDB.
ScriptEditorDB::ScriptEditorDB(const std::string &relpath)
    : mDB((RootPath()/relpath).native_file_string().c_str())
{
    EnsureCorrectSchema();
}

ScriptEditorDB::~ScriptEditorDB() {
    try {
        mDB.close();
    } catch (std::exception &e) {
        gLog.Error("%s", std::string("Error closing database: ")+e.what());
    }
}

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
         "  modtime TEXT NOT NULL)"},
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
                mDB.executescalar("SELECT sql FROM sqlite_master"
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
            mDB.executenonquery("DROP %s %s", sptr->type, sptr->name);
        } catch (std::exception &) {
            // Ignore failure to delete--the table may not be there.
        }
    }

    // Reclaim disk space.
    mDB.executenonquery("VACUUM");

    // Insert our new tables.
    for (schema_info *sptr = schema; sptr->name != NULL; sptr++) {
        mDB.executenonquery(sptr->sql);
    }
}

boost::filesystem::path ScriptEditorDB::RootPath() {
    std::string p(FileSystem::GetBaseDirectory().ToNativePathString());
    return boost::filesystem::path(p, boost::filesystem::native);
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

BEGIN_TEST_CASE(TestScriptEditorDB, TestCase) {
    // Get a database.
    ScriptEditorDB db("test.sqlite3");

    // Check to make sure the file exists.
	boost::filesystem::path db_path(ScriptEditorDB::RootPath()/"test.sqlite3");
	CHECK_EQ(boost::filesystem::exists(db_path), true);
    CHECK_EQ(boost::filesystem::is_directory(db_path), false);

    // Process a file.
    //db.BeginProcessingFile("Scripts/indexed.ss");
    //db.EndProcessingFile();
} END_TEST_CASE(TestScriptEditorDB);

#endif // BUILD_TEST_CASES

