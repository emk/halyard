// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

//========================================================================
//  Testing Framework
//=========================================================================
//  This testing framework is based on the xUnit family of testing
//  frameworks, as originally designed by the eXtreme Programming
//  community.  For a nice example of xUnit, see _Test-Driven_Development_
//  by Kent Beck, which shows the framework in action, and how to create
//  it in a new language.  I've followed his approach loosely here.

#ifndef TestCase_H
#define TestCase_H

// Forward declarations of a few classes.
class TestRegistry;
class TestRunReport;

//////////
// Define BUILD_TEST_CASES if it wasn't set in the project options or
// on the command line.  You should wrap all TestCase subclasses in
// BUILD_TEST_CASES.
//
#ifndef BUILD_TEST_CASES
#	define BUILD_TEST_CASES 1
#endif // ndef BUILD_TEST_CASES

//////////
// A test case can have one of three results: TEST_PASSED, TEST_FAILED
// or TEST_SKIPPED.  Note that we don't distinguish between different
// types of failure.  Several other xUnit-style frameworks *do* make
// this distinction, but combining all the failure types into a single
// code seems to make the implementation simpler without the loss
// of any useful output.
//
enum TestResult {
	TEST_PASSED,
	TEST_FAILED,
	TEST_SKIPPED,
	TEST_RESULT_MAX
};

//////////
// If a test case fails explicity (by calling FAIL, or violating a CHECK_*
// assertion), we throw a TestFailed exception.
//
class TestFailed : public TException
{
	std::string mMessage;

public:
	TestFailed(const char *inErrorFile, int inErrorLine,
			   const std::string &inMessage)
	    : TException(inErrorFile, inErrorLine, inMessage.c_str()) {}
};

//////////
// TestCase is the heart of the framework.  It has three overridable methods:
// SetUp, Test and TearDown.  If a family of test cases share similar
// initialization and cleanup, then put the initialization into SetUp, and
// the cleanup code into TearDown.  Then, for each test relying on those
// resources, create a subclass of TestCase and override Test.  It's best
// to use the BEGIN_TEST_CASE and END_TEST_CASE macros; they also take
// care of properly registering your test case.
//
// This is the closest C++ adaptation I can achieve of the standard
// xUnit TestCase class.
//
class TestCase
{
public:
	void Run();
	virtual void SetUp() {}
	virtual void Test() = 0;
	virtual void TearDown() {}
};

//////////
// TestCaseFactory knows how to manufacture instances of a given TestCase
// at runtime.  It's used to help automatically register TestCases
// with the global TestRegistry.  The process is managed by
// REGISTER_TEST_CASE.
//
class TestCaseFactory {
	std::string mName;

public:
	TestCaseFactory(const char *inName, TestRegistry *inRegistry);
	virtual ~TestCaseFactory() {}

	std::string GetName() { return mName; }
	virtual shared_ptr<TestCase> Create() = 0;
};

//////////
// TestCaseFactoryImpl provides an implementation of TestCaseFactory
// for a specific subclass of TestCase.  Again, this is used
// primarily by REGISTER_TEST_CASE.
//
template <class TestCaseType>
class TestCaseFactoryImpl : public TestCaseFactory
{
public:
	TestCaseFactoryImpl(const char *inName, TestRegistry *inRegistry)
		: TestCaseFactory(inName, inRegistry) { }
	shared_ptr<TestCase> Create() { return new TestCaseType(); }
};

//////////
// This object holds a report on the results of running a single test
// case.
//
class TestCaseReport {
public:
	typedef shared_ptr<TestCaseReport> ptr;

private:
	std::string mName;
	TestResult mTestResult;

	std::string mErrorMessage;
	std::string mErrorFile;
	int mErrorLine;

public:
	// This object must be created with 'new', and it immediately becomes
	// the property of inReport.
	// XXX - Wrap constructor in factory function.
	TestCaseReport(TestRunReport *inReport,
				   std::string inName, TestResult inTestResult,
				   const std::string &inErrorMessage = "",
				   const std::string &inErrorFile = "unknown",
				   int inErrorLine = 0);

	std::string GetName() const { return mName; }
	TestResult GetTestResult() const { return mTestResult; }

	std::string GetErrorMessage() const
		{ ASSERT(mErrorMessage != ""); return mErrorMessage; }
	std::string GetErrorFile() const { return mErrorFile; }
	int GetErrorLine() const { return mErrorLine; }

	std::string GetSummaryIfInteresting() const;
};

//////////
// Information about a complete run of zero or more TestCase objects.
// This is basically a collection of TestCaseReport objects with
// some summary statistics.
//
class TestRunReport
{
	typedef std::vector<TestCaseReport::ptr> TestCaseReportVector;

	TestCaseReportVector mTestCaseReports;
	std::vector<int> mResultCount;

public:
	typedef TestCaseReportVector::iterator iterator;
	typedef shared_ptr<TestRunReport> TestRunReport::ptr;

	TestRunReport() : mResultCount(TEST_RESULT_MAX, 0) {}

	void AddTestCaseReport(TestCaseReport::ptr inTestCaseReport);

	iterator begin() { return mTestCaseReports.begin(); }
	iterator end() { return mTestCaseReports.end(); }

	int GetTestResultCount(TestResult inType)
		{ return mResultCount[inType]; }

	int GetNumTestsPassed() { return GetTestResultCount(TEST_PASSED); }
	int GetNumTestsFailed() { return GetTestResultCount(TEST_FAILED); }
	int GetNumTestsSkipped() { return GetTestResultCount(TEST_SKIPPED); }

	bool AnyTestFailed() { return GetNumTestsFailed() != 0; }
	
	std::string GetSummary();
};

//////////
// An abstract interface for code which reports on the status of a test
// run.  We don't implement this here; it's the responsibility of
// various test drivers.
//
class ITestProgressMeter {
public:
	//////////
	// Override this method to report status.
	//
	// [in] inTestIndex - The zero-based index of the test just run.
	// [in] inTestCount - The total number of tests to run.
	// [in] inReport    - The results of running the test.
	//
	virtual void UpdateTestProgress(int inTestIndex, int inTestCount,
									const TestCaseReport &inReport) = 0;
};

//////////
// TestRegistry knows about a set of test cases, and can run them
// on behalf of various test drivers.
//
// The function GetGlobalRegistry returns the registry used by
// REGISTER_TEST_CASE.  This is the only registry which you will
// normally need to use.
//
class TestRegistry
{
	// The REGISTER_TEST_CASE macro uses this global registry for
	// registering test cases.
	static TestRegistry *sGlobalRegistry;

	// We're not responsible for deleting these pointers--they typically
	// point to global variables.
	std::vector<TestCaseFactory*> mTestCaseFactories;

public:
	static TestRegistry *GetGlobalRegistry();

	typedef std::vector<TestCaseFactory*>::iterator iterator;

	void RegisterTestCaseFactory(TestCaseFactory *inFactory);

	iterator begin() { return mTestCaseFactories.begin(); }
	iterator end() { return mTestCaseFactories.end(); }

	TestRunReport::ptr RunAllTests(ITestProgressMeter *inMeter = NULL);
};

//////////
// Immediately fail the current test case.
//
#define FAIL_TEST(MESSAGE) \
    throw TestFailed(__FILE__, __LINE__, MESSAGE)

//////////
// Make sure that CODE throws an exception of TYPE.  If not, fail.
//
#define CHECK_THROWN(TYPE, CODE) \
    do { \
        bool caught_exception = false; \
        try { \
            CODE; \
        } catch (TYPE &) { \
            caught_exception = true; \
        } \
        if (!caught_exception) \
            FAIL_TEST("Expected an exception: " ## #CODE); \
    } while (0)

//////////
// Create a function NAME to help test OP.  OP must be a binary operator,
// and its arguments must support operator<<.  This is macro, because
// MSVC doesn't like overloaded operators as template parameters.  It
// generates a template because most operators are overloaded for
// multiple types, and we need to choose the right type.
//
#define DEFINE_CHECK_OP_HELPER(NAME, OP) \
    template <class Type1, class Type2> \
	void NAME(const char *inErrorFile, int inErrorLine, \
			  const char *inExpr1, const char *inExpr2, \
			  const Type1 &inVal1, const Type2 &inVal2) { \
		if (!(inVal1 OP inVal2)) { \
			std::ostringstream out; \
			out << "expected " << inExpr1 << " " << #OP << " " << inExpr2 \
				<< ", got: " << inVal1 << ", " << inVal2; \
			throw TestFailed(inErrorFile, inErrorLine, out.str()); \
		} \
	}

template <typename Functor, class Type1, class Type2>
void CheckFuncHelper(const char *inErrorFile, int inErrorLine,
					 const char *inCompName,
					 const char *inExpr1, const char *inExpr2,
					 Functor inComp,
					 const Type1 &inVal1, const Type2 &inVal2) {
	if (!inComp(inVal1, inVal2)) {
		std::ostringstream out;
		out << "expected " << inCompName << "(" << inExpr1 << ", " << inExpr2
			<< "), got: " << inVal1 << ", " << inVal2;
		throw TestFailed(inErrorFile, inErrorLine, out.str());
	}
}


//////////
// Fail if !(expr1 == expr2).  Both expressions must return a type
// supporting operator<<.
//
#define CHECK_EQ(expr1, expr2) \
    CheckEqualHelper(__FILE__, __LINE__, #expr1, #expr2, expr1, expr2)
DEFINE_CHECK_OP_HELPER(CheckEqualHelper, ==)

//////////
// Fail if !(expr1 != expr2).  Both expressions must return a type
// supporting operator<<.
//
#define CHECK_NE(expr1, expr2) \
    CheckNotEqualHelper(__FILE__, __LINE__, #expr1, #expr2, expr1, expr2)
DEFINE_CHECK_OP_HELPER(CheckNotEqualHelper, !=)

//////////
// A handy wrapper unregistered test cases; it saves typing.  You normally
// want BEGIN_TEST_CASE and END_TEST_CASE below.
//
// This macro is split into two parts because we want the body of
// TestCase::Test to be outside the macro expansion.  Although it would
// save typing to make TestCase::Test a BODY parameter, this causes
// the MSVC debugger to get terribly confused about line numbers, and
// assign the same line number to all the code in the test suite.
//
#define BEGIN_UNREGISTERED_TEST_CASE(NAME, SUPERCLASS) \
    class NAME : public SUPERCLASS { public: void Test()
#define END_UNREGISTERED_TEST_CASE(NAME) }

//////////
// Register the TestCase subclass NAME with the global TestRegistry.  This
// relies on creating a global object and letting the constructor do all
// the work (this is a standard C++ technique for initializing code before
// main() gets called).
//
#define REGISTER_TEST_CASE(NAME) \
    TestCaseFactoryImpl<NAME> \
        NAME##_factory(#NAME, TestRegistry::GetGlobalRegistry())

//////////
// Handy wrapper for defining a TestCase subclass and registering it with
// the global TestRegistry.  See BEGIN_UNREGISTERED_TEST_CASE for an
// explanation of why this macro is split into two parts.
//
#define BEGIN_TEST_CASE(NAME, SUPERCLASS) \
    BEGIN_UNREGISTERED_TEST_CASE(NAME, SUPERCLASS)
#define END_TEST_CASE(NAME) \
    END_UNREGISTERED_TEST_CASE(NAME); \
	REGISTER_TEST_CASE(NAME)

//////////
//
//
#define REGISTER_TEST_CASE_FILE(NAME) \
	char test_registration_ ## NAME = '\0'

#define REFERENCE_TEST_CASE_FILE(NAME) \
	extern char test_registration_ ## NAME; \
	static char *test_reference_ ## NAME = &test_registration_ ## NAME

#endif // TestCase_H
