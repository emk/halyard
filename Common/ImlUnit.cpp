// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#include <iostream>

#include <stdlib.h>

#include "ImlUnit.h"

int total_tests = 0;
int total_failures = 0;

// This is a good place to set a breakpoint.
void test_failure (const char *file, int line,
		   const char *label, const char *statement) 
{
	total_failures++;
	std::cout << std::endl << file << ":" << line << ": test failure: "
		      << label << " " << statement << std::endl;
 
	// We exit automatically, because these tests are supposed to run
	// at 100% all the time.  No test failures should ever be checked
	// into the CVS repository.
	exit(1);
}

int tests_finished (void) 
{
	// Summarize our test run.
	std::cout << std::endl << "Ran " << total_tests << " tests, "
		      << total_failures << " failed, "
		      << 100.0 - (100.0 * total_failures) / total_tests
		      << "% passed" << std::endl;

	// Print the final result.
	if (total_failures == 0) {
		std::cout << "OK" << std::endl;
		return 0;
	}

	std::cout << "FAILED" << std::endl;
	return 1;
}
