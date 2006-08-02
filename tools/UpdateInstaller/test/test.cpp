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

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/auto_unit_test.hpp>

#define BOOST_FILESYSTEM_SOURCE
#include "boost/filesystem/path.hpp"

#include <vector>
#include <string>
#include "Manifest.h"
#include "CommandLine.h"

using boost::filesystem::path;

#define CHECK_ENTRY(DIGEST,SIZE,PATH,ENTRY) \
    do { \
        Manifest::Entry _e(ENTRY); \
        BOOST_CHECK((DIGEST) == _e.digest()); \
        BOOST_CHECK((SIZE) == _e.size()); \
        BOOST_CHECK((PATH) == _e.path()); \
    } while(0)

BOOST_AUTO_UNIT_TEST(test_parse_diff) {
    Manifest diff(path("Updates/temp/MANIFEST-DIFF"));
    BOOST_CHECK(3 == diff.entries().size());
    CHECK_ENTRY("855426068ee8939df6bce2c2c4b1e7346532a133", 5, "sub/foo.txt",
                diff.entries()[0]);
    CHECK_ENTRY("da39a3ee5e6b4b0d3255bfef95601890afd80709", 0, "sub/quux.txt",
                diff.entries()[1]);
    CHECK_ENTRY("855426068ee8939df6bce2c2c4b1e7346532a133", 5, "foo.txt",
                diff.entries()[2]);
}

BOOST_AUTO_UNIT_TEST(test_parse_spec) {
	SpecFile spec(path("Updates/release.spec"));
	BOOST_CHECK("http://www.example.com/updates/" == spec.url());
	BOOST_CHECK("update" == spec.build());
	BOOST_CHECK(2 == spec.manifest().entries().size());
}

BOOST_AUTO_UNIT_TEST(test_windows_command_line_quoting) {
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
