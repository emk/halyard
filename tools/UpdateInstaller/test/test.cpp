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

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>

#include <string>
#include "FileSet.h"
#include "SpecFile.h"
#include "CommandLine.h"
#include "UpdateInstaller.h"
#include "Interface.h"

using boost::filesystem::path;

void UpdateProgressRange(size_t step_count) {
    // Do nothing.
}

void UpdateProgress(size_t steps_completed) {
    // Do nothing.
}

#define CHECK_ENTRY(DIGEST,SIZE,PATH,ENTRIES) \
    do { \
        FileSet::EntrySet _e(ENTRIES); \
        BOOST_CHECK(_e.find(FileSet::Entry((DIGEST), (SIZE), (PATH))) \
                    != _e.end()); \
    } while(0)

const char *foo_digest = "855426068ee8939df6bce2c2c4b1e7346532a133";
const char *null_digest = "da39a3ee5e6b4b0d3255bfef95601890afd80709";

BOOST_AUTO_TEST_CASE(test_parse_diff) {
    FileSet diff(FileSet::FromManifestFile(path("Updates/temp/MANIFEST-DIFF")));

    BOOST_CHECK(3 == diff.Entries().size());
    CHECK_ENTRY(foo_digest, 5, "sub/foo.txt", diff.Entries());
    CHECK_ENTRY(null_digest, 0, "sub/quux.txt", diff.Entries());
    CHECK_ENTRY(foo_digest, 5, "foo.txt", diff.Entries());
}

BOOST_AUTO_TEST_CASE(test_parse_manifest) {
    path base_path("Updates/manifests/update/MANIFEST.base");
    FileSet base_manifest(FileSet::FromManifestFile(base_path));
    BOOST_CHECK(2 == base_manifest.Entries().size());
    CHECK_ENTRY(null_digest, 0, "bar.txt", base_manifest.Entries());
    CHECK_ENTRY(foo_digest, 5, "foo.txt", base_manifest.Entries());

    FileSet::Entry foo(foo_digest, 5, "foo.txt");
    BOOST_CHECK(base_manifest.HasMatchingEntry(foo));

    path sub_path("Updates/manifests/update/MANIFEST.sub");
    FileSet sub_manifest(FileSet::FromManifestFile(sub_path));
    BOOST_CHECK(3 == sub_manifest.Entries().size());
    CHECK_ENTRY(null_digest, 0, "sub/baz.txt", sub_manifest.Entries());
    CHECK_ENTRY(foo_digest, 5, "sub/foo.txt", sub_manifest.Entries());
    CHECK_ENTRY(null_digest, 0, "sub/quux.txt", sub_manifest.Entries());

    FileSet::Entry quux(null_digest, 0, "sub/quux.txt");
    BOOST_CHECK(sub_manifest.HasMatchingEntry(quux));
    
    FileSet::Entry bad_entry("not-a-digest", 0, "sub/quux.txt");
    BOOST_CHECK(!sub_manifest.HasMatchingEntry(bad_entry));
}

BOOST_AUTO_TEST_CASE(test_all_manifests_in_dir) {
    path update_manifest_dir("Updates/manifests/update/");
    FileSet full_manifest(FileSet::FromManifestsInDir(update_manifest_dir));

    BOOST_CHECK(5 == full_manifest.Entries().size());
    CHECK_ENTRY(null_digest, 0, "bar.txt", full_manifest.Entries());
    CHECK_ENTRY(foo_digest, 5, "foo.txt", full_manifest.Entries());
    CHECK_ENTRY(null_digest, 0, "sub/baz.txt", full_manifest.Entries());
    CHECK_ENTRY(foo_digest, 5, "sub/foo.txt", full_manifest.Entries());
    CHECK_ENTRY(null_digest, 0, "sub/quux.txt", full_manifest.Entries());

    FileSet::Entry sub_foo(foo_digest, 5, "sub/foo.txt");
    FileSet::Entry foo(foo_digest, 5, "foo.txt");
    BOOST_CHECK(full_manifest.HasMatchingEntry(sub_foo));
    BOOST_CHECK(full_manifest.HasMatchingEntry(foo));
}

BOOST_AUTO_TEST_CASE(test_diff_manifests) {
    FileSet base(FileSet::FromManifestsInDir("."));
    FileSet update(FileSet::FromManifestsInDir("Updates/manifests/update"));
    FileSet diff(update.MinusExactMatches(base));

    BOOST_CHECK(3 == diff.Entries().size());
    CHECK_ENTRY(foo_digest, 5, "foo.txt", diff.Entries());
    CHECK_ENTRY(foo_digest, 5, "sub/foo.txt", diff.Entries());
    CHECK_ENTRY(null_digest, 0, "sub/quux.txt", diff.Entries());
}

BOOST_AUTO_TEST_CASE(test_parse_spec) {
    SpecFile spec(path("Updates/release.spec"));
    BOOST_CHECK("http://www.example.com/updates/" == spec.url());
    BOOST_CHECK("update" == spec.build());
    BOOST_CHECK(2 == spec.manifest().Entries().size());
}

BOOST_AUTO_TEST_CASE(test_windows_command_line_quoting) {
    char *test[5] = { "C:\\Program Files\\foo.exe",
                      "Something with spaces",
                      "Something\" with\" quotes",
                      "Something with \\\" backslash quotes",
                      "Big\\\" old\" mix \\of \\\\\" stuff" };
                      
    CommandLine cl(5, test); 
    BOOST_CHECK_EQUAL(std::string("\"C:\\Program Files\\foo.exe\" ") 
                      + "\"Something with spaces\" " 
                      + "\"Something\\\" with\\\" quotes\" " 
                      + "\"Something with \\\\\\\" backslash quotes\" " 
                      + "\"Big\\\\\\\" old\\\" mix \\of \\\\\\\\\\\" stuff\"",
                      cl.WindowsQuotedString());
}

BOOST_AUTO_TEST_CASE(test_is_update_possible) {
    UpdateInstaller installer = UpdateInstaller(path("."), path("."));
    
    installer.PrepareForUpdate();
    BOOST_REQUIRE(installer.IsUpdatePossible());    

    rename(path("Updates/pool/da39a3ee5e6b4b0d3255bfef95601890afd80709"), 
           path("Updates/pool/temp"));
    BOOST_CHECK(!installer.IsUpdatePossible());
    
    rename(path("Updates/pool/temp"),
           path("Updates/pool/da39a3ee5e6b4b0d3255bfef95601890afd80709"));
    BOOST_CHECK(installer.IsUpdatePossible());
}
