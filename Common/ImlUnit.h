//=========================================================================
//  Test Harness
//=========================================================================
//  This is a super light-weight test harness. It's vaguely inspired by
//  Kent Beck's book on eXtreme Programming (XP)--the output is succinct,
//  new tests can be coded quickly, and the whole thing runs in a few
//  second's time.
//
//  To check for memory leaks, install RedHat's 'memprof' utility, and
//  type 'memprof testapp'.
//
//  If you write any tests using this framework, please deallocate any data
//  structures you use in the appropriate fashion. This allows us to test
//  various destructor code for memory leaks.

#ifndef IMLUNIT_H
#define IMLUNIT_H

#include <iostream.h>

// Private API (needed by preprocessor macros)

extern int total_tests;
extern int total_failures;

extern void test_failure (char *file, int line, char *label, char *statement);

// Public API

extern int tests_finished (void);

// This macro involves quite a bit of preprocessor arcana.  Yes, it all
// needs to be here.  You are not expected to understand it.  But you are
// expected *not* to change it unless you do understand it.
#define TEST(statement) \
	do { \
		total_tests++; \
		if ((statement)) { \
			cout << "."; \
		} else { \
			test_failure(__FILE__, __LINE__, "expected", #statement); \
		} \
	} while (0)

#endif // IMLUNIT_H
