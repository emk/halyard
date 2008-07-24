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
#include "doc/Document.h"

// The TSchemeInterpreterTests file needs to be handled specially.
#include "lang/scheme/TSchemeInterpreterTests.h"

// We declare some testing-related primitives for the interpreter.
#include "TPrimitives.h"


using namespace Halyard;


//=========================================================================
//  Support for Tests Using Legacy ImlUnit Test Framework
//=========================================================================

static bool gShouldWait = false;

/// should_wait is needed on Windows NT, 2000, etc., when running under
/// Visual Studio.
static __attribute__((noreturn)) void finished(int status) {
	if (gShouldWait) {
		std::cerr << "Press enter to continue.";
		char c;
		std::cin >> std::noskipws >> c;
	}
    exit(status);
}

#define BEGIN_TEST_EXCEPTION_TRAPPER \
    try {

#define END_TEST_EXCEPTION_TRAPPER \
	} catch (std::exception &error) { \
		std::cerr << std::endl << "Exception: " << error.what() << std::endl; \
        finished(1); \
	} catch (...) { \
		std::cerr << std::endl << "An unknown exception occurred!" \
				  << std::endl; \
        finished(1); \
	}


//=========================================================================
//  Support for Tests Using Legacy ImlUnit Test Framework
//=========================================================================

// XXX - For now, we'll declare the per-file test entry points here, to
// avoid creating extra header files just for one function.  These will
// most likely be replaced with static constructor tricks as I continue to
// C++-ify the testing API.
extern void test_TTextTransform (void);
extern void test_FileSystem (void);
extern void test_Model(void);
extern void test_Typography (void);
extern void test_TStyleSheet (void);
extern void test_TVectorDiff (void);
extern void test_TPolygon (void);

REFERENCE_TEST_CASE_FILE(TestCase);
REFERENCE_TEST_CASE_FILE(CaptionList);
REFERENCE_TEST_CASE_FILE(TTemplateUtils);
REFERENCE_TEST_CASE_FILE(TValue);
REFERENCE_TEST_CASE_FILE(TSchemeConv);
REFERENCE_TEST_CASE_FILE(TVariableManager);
REFERENCE_TEST_CASE_FILE(TStateDB);
REFERENCE_TEST_CASE_FILE(ScriptEditorDB);
REFERENCE_TEST_CASE_FILE(TTextConv);

static void run_imlunit_tests() {
	test_TTextTransform();
	test_FileSystem();
	test_Model();
	test_Typography();
	test_TStyleSheet();
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
//  Testing Primitives
//=========================================================================

DEFINE_PRIMITIVE(Test) {
	std::string info;
	bool result;
	inArgs >> info >> result;
	TEST_WITH_LABEL(info.c_str(), result);
}

DEFINE_PRIMITIVE(RunAllCppTests) {
    BEGIN_TEST_EXCEPTION_TRAPPER

    CheckTSchemeInterpreterTestResults();
    std::cout << std::endl << std::endl
              << "Old-Style ImlUnit Tests" << std::endl;
    run_imlunit_tests();
    std::cout << std::endl << "New-Style TestCase Tests" << std::endl;
    run_testcase_tests();

    END_TEST_EXCEPTION_TRAPPER
}

static void RegisterTestPrimitives() {
	REGISTER_PRIMITIVE(Test);
    REGISTER_PRIMITIVE(RunAllCppTests);
    RegisterTSchemeInterpreterTestPrimitives();
}


//=========================================================================
//  Main Entry Point
//=========================================================================

int main(int argc, char **argv) {
	if (argc == 2 && std::string(argv[1]) == "--wait")
		gShouldWait = true;

    RegisterTestPrimitives();
    
    // We need to find the root directory of our Halyard tree and set up
    // our runtime directory location correctly.
    FileSystem::Path current_dir = FileSystem::Path();
    FileSystem::Path root_dir(current_dir.ParentDirectory().ParentDirectory());
    FileSystem::SetRuntimeDirectory(root_dir.AddComponent("runtime"));

	FileSystem::SetScriptDataDirectoryName("Halyard Common Test");

    BEGIN_TEST_EXCEPTION_TRAPPER

	Halyard::InitializeCommonCode(new CrashReporter());

    std::cout << "Scheme Interpreter Tests" << std::endl;
    
    // Initialize our Scheme interpreter normally.
    scoped_ptr<TInterpreterManager> manager(
        GetSchemeInterpreterManager(&TSchemeInterpreterTestIdleProc));
    Document doc(FileSystem::Path().ToNativePathString(), Document::OPEN);
    
    // Run our Scheme interpreter.
    manager->Run();
    if (manager->ExitedWithError()) {
            std::cerr << std::endl << "Interpreter exited with error"
                      << std::endl;
            finished(1);
    }

    END_TEST_EXCEPTION_TRAPPER

    finished(0);
}
