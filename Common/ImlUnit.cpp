#include <stdlib.h>

#include "ImlUnit.h"

int total_tests = 0;
int total_failures = 0;

// This is a good place to set a breakpoint.
void test_failure (char *file, int line, char *label, char *statement) 
{
	total_failures++;
	std::cout << std::endl << file << ":" << line << ": test failure: "
		      << label << " (" << statement << ")" << std::endl;
 
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
