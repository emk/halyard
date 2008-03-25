// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include "CommonHeaders.h"

#include "ImlUnit.h"   // Old and simple.
#include "TestCase.h"  // New and spiffy.

// We need to call some module initialization functions.
#include "TStartup.h"
#include "CrashReporter.h"

// We declare some testing-related primitives for the interpreter.
#include "TPrimitives.h"

// XXX - Hack to make REGISTER_5L_PRIMITIVE work correctly.  It needs to be
// called from a function in the FiveL:: namespace, which is silly.
BEGIN_NAMESPACE_FIVEL
extern void RegisterTestPrimitives();
END_NAMESPACE_FIVEL

USING_NAMESPACE_FIVEL


//=========================================================================
//  Support for Tests Using Legacy ImlUnit Test Framework
//=========================================================================

// XXX - For now, we'll declare the per-file test entry points here, to
// avoid creating extra header files just for one function.  These will
// most likely be replaced with static constructor tricks as I continue to
// C++-ify the testing API.
//extern void test_TString (void);
extern void test_TEncoding (void);
extern void test_FileSystem (void);
extern void test_Model(void);
extern void test_Typography (void);
extern void test_TStyleSheet (void);
extern void test_TSchemeInterpreter (void);
extern void test_TVectorDiff (void);
extern void test_TPolygon (void);

REFERENCE_TEST_CASE_FILE(TestCase);
REFERENCE_TEST_CASE_FILE(TTemplateUtils);
REFERENCE_TEST_CASE_FILE(TValue);
REFERENCE_TEST_CASE_FILE(TSchemeConv);
REFERENCE_TEST_CASE_FILE(TVariableManager);
REFERENCE_TEST_CASE_FILE(TStateDB);
REFERENCE_TEST_CASE_FILE(ScriptEditorDB);

DEFINE_5L_PRIMITIVE(test) {
	std::string info;
	bool result;
	inArgs >> info >> result;
	TEST_WITH_LABEL(info.c_str(), result);
}

void FIVEL_NS RegisterTestPrimitives() {
	REGISTER_5L_PRIMITIVE(test);
}

static void run_imlunit_tests() {
	RegisterTestPrimitives();
	test_TEncoding();
	test_FileSystem();
	test_Model();
	test_Typography();
	test_TStyleSheet();
	test_TSchemeInterpreter();
	test_TVectorDiff();
	test_TPolygon();
	tests_finished();	
}


//=========================================================================
//  Support for Tests Using the New-Style TestCase Framework
//=========================================================================

/// Output for a command-line-based unit test run.
class ConsoleMeter : public ITestProgressMeter {
public:
	void UpdateTestProgress(int inTestIndex, int inTestCount,
							const TestCaseReport &inReport);
};

void ConsoleMeter::UpdateTestProgress(int inTestIndex,int inTestCount,
									  const TestCaseReport &inReport)
{
	switch (inReport.GetTestResult()) {
		case TEST_PASSED:
			std::cout << ".";
			break;
		case TEST_FAILED:
			std::cout << "F";
			break;
		case TEST_SKIPPED:
			std::cout << "S";
			break;
		default:
			std::cout << "?";
			break;
	}
}

static void run_testcase_tests() {
	TestRegistry *registry = TestRegistry::GetGlobalRegistry();
	ConsoleMeter meter;
	TestRunReport::ptr report = registry->RunAllTests(&meter);
	std::cout << std::endl << report->GetSummary() << std::endl;
	TestRunReport::iterator i = report->begin();
	for (; i != report->end(); ++i)
		std::cout << (*i)->GetSummaryIfInteresting();
}


//=========================================================================
//  Main Entry Point
//=========================================================================

/// This is needed on Windows NT, 2000, etc., when running under Visual
/// Studio.
void prompt_done(bool should_wait) {
	if (should_wait) {
		std::cerr << "Press enter to continue.";
		char c;
		std::cin >> std::noskipws >> c;
	}
}

int main(int argc, char **argv) {
	FIVEL_SET_STACK_BASE();

	bool should_wait = false;
	if (argc == 2 && std::string(argv[1]) == "--wait")
		should_wait = true;

	FileSystem::SetScriptDataDirectoryName("Test");

	try {
		FIVEL_NS InitializeCommonCode(new CrashReporter());
		std::cout << "Old-Style ImlUnit Tests" << std::endl;
		run_imlunit_tests();
		std::cout << std::endl << "New-Style TestCase Tests" << std::endl;
		run_testcase_tests();
	} catch (std::exception &error) {
		std::cerr << std::endl << "Exception: " << error.what() << std::endl;
        prompt_done(should_wait);
		return 1;
	} catch (...) {
		std::cerr << std::endl << "An unknown exception occurred!"
				  << std::endl;
        prompt_done(should_wait);
		return 1;
	}

    prompt_done(should_wait);
	return 0;
}
