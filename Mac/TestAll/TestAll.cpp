#include "ImlUnit.h"
#include <iostream.h>

// XXX - For now, we'll declare the per-file test entry points here, to
// avoid creating extra header files just for one function.  These will
// most likely be replaced with static constructor tricks as I continue to
// C++-ify the testing API.
extern void test_TStream (void);

int main (int argc, char **argv) 
{
	test_TStream();

	return tests_finished();
}
