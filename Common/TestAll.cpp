// -*- Mode: C++; tab-width: 4; -*-

#include <iostream>
#include <stdexcept>

#include "ImlUnit.h"

// XXX - For now, we'll declare the per-file test entry points here, to
// avoid creating extra header files just for one function.  These will
// most likely be replaced with static constructor tricks as I continue to
// C++-ify the testing API.
extern void test_TString (void);
extern void test_TEncoding (void);
extern void test_FileSystem (void);
extern void test_Typography (void);

int main (int argc, char **argv)
{
	try
	{
		test_TString();
		test_TEncoding();
		test_FileSystem();
		test_Typography();
	}
	catch (std::exception &error)
	{
		std::cerr << std::endl << error.what() << std::endl;
		return 1;
	}
	catch (...)
	{
		std::cerr << std::endl << "An unknown exception occurred!" << std::endl;
		return 1;
	}

	return tests_finished();
}
