// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <iostream>
#include <sstream>

#include "FiveLApp.h"
#include "TestHarness.h"
#include "Stage.h"
#include "FancyStatusBar.h"


//=========================================================================
//  TestCase
//=========================================================================

class TestFailed : public TException
{
	std::string mMessage;

public:
	TestFailed(const char *inErrorFile, int inErrorLine,
			   const std::string &inMessage)
        : TException(inErrorFile, inErrorLine, inMessage.c_str()) {}
};

class TestCase
{
public:
	void Run() { SetUp(); Test(); }
	virtual void SetUp() {}
	virtual void Test() = 0;
};

#define FAIL_TEST(MESSAGE) \
    throw TestFailed(__FILE__, __LINE__, MESSAGE)

#define CHECK_THROWN(TYPE, CODE) \
    do { \
        bool caught_exception = false; \
        try { \
            CODE; \
        } catch (TYPE &WXUNUSED(exception)) { \
            caught_exception = true; \
        } \
        if (!caught_exception) \
            FAIL_TEST("Expected an exception: " ## #CODE); \
    } while (0)

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

DEFINE_CHECK_OP_HELPER(CheckEqualHelper, ==)

#define CHECK_EQ(expr1, expr2) \
    CheckEqualHelper(__FILE__, __LINE__, #expr1, #expr2, expr1, expr2)

DEFINE_CHECK_OP_HELPER(CheckNotEqualHelper, !=)

#define CHECK_NE(expr1, expr2) \
    CheckNotEqualHelper(__FILE__, __LINE__, #expr1, #expr2, expr1, expr2)

#define UNREGISTERED_TEST_CASE(NAME, SUPERCLASS, BODY) \
    class NAME : public SUPERCLASS { public: void Test() { BODY } }


//=========================================================================
//  TestRegistry
//=========================================================================

class TestRegistry;

class TestCaseFactory {
	std::string mName;

public:
	TestCaseFactory(const char *inName, TestRegistry *inRegistry);
	virtual ~TestCaseFactory() {}

	std::string GetName() { return mName; }
	virtual boost::shared_ptr<TestCase> Create() = 0;
};

template <class TestCaseType>
class TestCaseFactoryImpl : public TestCaseFactory
{
public:
	TestCaseFactoryImpl(const char *inName, TestRegistry *inRegistry)
		: TestCaseFactory(inName, inRegistry) { }
	boost::shared_ptr<TestCase> Create() { return new TestCaseType(); }
};

class TestRunReport;

enum TestResult {
	TEST_PASSED,
	TEST_FAILED,
	TEST_SKIPPED,
	TEST_RESULT_TYPE_COUNT
};

class TestCaseReport {
public:
	typedef boost::shared_ptr<TestCaseReport> ptr;

private:
	std::string mName;
	TestResult mTestResult;

	std::string mErrorMessage;
	std::string mErrorFile;
	int mErrorLine;

public:
	// This object must be created with 'new', and it immediately becomes
	// the property of inReport.
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
};

class TestRunReport
{
	typedef std::vector<TestCaseReport::ptr> TestCaseReportVector;

	TestCaseReportVector mTestCaseReports;
	std::vector<int> mResultCount;

public:
	typedef TestCaseReportVector::iterator iterator;
	typedef boost::shared_ptr<TestRunReport> TestRunReport::ptr;

	TestRunReport() : mResultCount(TEST_RESULT_TYPE_COUNT, 0) {}

	iterator begin() { return mTestCaseReports.begin(); }
	iterator end() { return mTestCaseReports.end(); }

	int GetTestResultCount(TestResult inType)
		{ return mResultCount[inType]; }

	int GetNumTestsPassed() { return GetTestResultCount(TEST_PASSED); }
	int GetNumTestsFailed() { return GetTestResultCount(TEST_FAILED); }
	int GetNumTestsSkipped() { return GetTestResultCount(TEST_SKIPPED); }

	bool AnyTestFailed() { return GetNumTestsFailed() != 0; }
	
	void AddTestCaseReport(TestCaseReport::ptr inTestCaseReport)
	{
		mResultCount[inTestCaseReport->GetTestResult()]++;
		mTestCaseReports.push_back(inTestCaseReport);
	}

	std::string GetSummary()
	{
		std::ostringstream out;
		if (AnyTestFailed())
			out << "FAIL: " << GetNumTestsFailed() << " failed, ";
		else
			out << "OK: ";
		out << GetNumTestsPassed() << " passed, "
			<< GetNumTestsSkipped() << " skipped";
		return out.str();
	}
};

TestCaseReport::TestCaseReport(TestRunReport *inReport,
							   std::string inName, TestResult inTestResult,
							   const std::string &inErrorMessage,
							   const std::string &inErrorFile,
							   int inErrorLine)
	: mName(inName), mTestResult(inTestResult),
	  mErrorMessage(inErrorMessage), mErrorFile(inErrorFile),
	  mErrorLine(inErrorLine)
{
	inReport->AddTestCaseReport(TestCaseReport::ptr(this));
}

class ITestProgressMeter {
public:
	virtual void UpdateTestProgress(int inTestIndex, int inTestCount,
									const TestCaseReport &inReport) = 0;
};

class TestRegistry
{
	// The REGISTER_TEST_CASE macro uses this global registry for
	// registering test cases.
	static TestRegistry *sGlobalRegistry;

	// We're not responsible for deleting these pointers--they typically
	// point to global variables.
	std::vector<TestCaseFactory*> mTestCaseFactories;

public:
	static TestRegistry *GetGlobalRegistry()
    {
		if (sGlobalRegistry == NULL)
			sGlobalRegistry = new TestRegistry;
		return sGlobalRegistry;
	}

	typedef std::vector<TestCaseFactory*>::iterator iterator;

	iterator begin() { return mTestCaseFactories.begin(); }
	iterator end() { return mTestCaseFactories.end(); }

	void RegisterTestCaseFactory(TestCaseFactory *inFactory) {
		mTestCaseFactories.push_back(inFactory);
	}

	TestRunReport::ptr RunAllTests(ITestProgressMeter *inMeter = NULL) {
		TestRunReport::ptr report(new TestRunReport);
		int index = 0;
		for (iterator i = begin(); i != end(); ++i) {
			std::string name = (*i)->GetName();
			try {
				(*i)->Create()->Run();
				new TestCaseReport(report.get(), name, TEST_PASSED);
			} catch (TException &e) {
				new TestCaseReport(report.get(), name, TEST_FAILED,
								   e.GetErrorMessage(),
								   e.GetErrorFile(), e.GetErrorLine());
			} catch (std::exception &e) {
				new TestCaseReport(report.get(), name, TEST_FAILED,
								   e.what());
			} catch (...) {
				new TestCaseReport(report.get(), name, TEST_FAILED,
								   "unknown exception (maybe segfault?)");
			}
			if (inMeter)
				inMeter->UpdateTestProgress(index++, end() - begin(),
											**(report->end() - 1));
		}
		return report;
	}
};

TestRegistry *TestRegistry::sGlobalRegistry = NULL;

TestCaseFactory::TestCaseFactory(const char *inName, TestRegistry *inRegistry)
	: mName(inName)
{
	inRegistry->RegisterTestCaseFactory(this);
}

#define REGISTER_TEST_CASE(NAME) \
    TestCaseFactoryImpl<NAME> \
        NAME##_factory(#NAME, TestRegistry::GetGlobalRegistry())

#define TEST_CASE(NAME, SUPERCLASS, BODY) \
    UNREGISTERED_TEST_CASE(NAME, SUPERCLASS, BODY); \
	REGISTER_TEST_CASE(NAME)


//=========================================================================
//  TestCase Tests
//=========================================================================

class BootstrapTestCase : public TestCase {
public:
	bool mTestPassed;
	BootstrapTestCase() { mTestPassed = false; }
};

template <class BTC>
class TestBootstrapTestCase : public TestCase {
	void Test() {
		// We need to use assertions, because we haven't yet proved that
		// the usual failure mechanism is working at this point in the
		// bootstrap process.
		BTC test;
		ASSERT(!test.mTestPassed);
		test.Run();
		ASSERT(test.mTestPassed);
	}
};

#define TEST_BOOTSTRAP_TEST_CASE(NAME) \
    class Test##NAME : public TestBootstrapTestCase<NAME> {}; \
    REGISTER_TEST_CASE(Test##NAME)

template <class FTC, class ExpectedException>
class TestFailingTestCase : public TestCase {
public:
	virtual std::string WantMsg() = 0;
	void Test() {
		// We need to use assertions, because we haven't yet proved that
		// the usual failure mechanism is working at this point in the
		// bootstrap process.
		FTC test;
		bool caught_exception = false;
		try {
			test.Run();
		} catch (ExpectedException &failure) {
			caught_exception = true;
			ASSERT(failure.GetErrorMessage() == WantMsg());
		}
		ASSERT(caught_exception);
	}
};

#define TEST_FAILING_TEST_CASE(NAME, EXCEPTION, WANT_MSG) \
    class Test##NAME \
		: public TestFailingTestCase<NAME, EXCEPTION> { \
			std::string WantMsg() { return WANT_MSG; } \
	}; \
    REGISTER_TEST_CASE(Test##NAME)

// *** Make sure tests actually get run.
UNREGISTERED_TEST_CASE(WasRunTest, BootstrapTestCase, {
	mTestPassed = true;
});
TEST_BOOTSTRAP_TEST_CASE(WasRunTest);

// *** Make sure FAIL_TEST throws an exception.
UNREGISTERED_TEST_CASE(FailThrowsExceptionTest, TestCase, {
	FAIL_TEST("my message");
});
TEST_FAILING_TEST_CASE(FailThrowsExceptionTest, TestFailed, "my message");

// *** Make sure CHECK_THROWN blocks exceptions.
UNREGISTERED_TEST_CASE(PassCheckThrownTest, BootstrapTestCase, {
	CHECK_THROWN(TException, THROW("Random exception"));
	mTestPassed = true;
});
TEST_BOOTSTRAP_TEST_CASE(PassCheckThrownTest);

// *** Make sure CHECK_THROWN fails if no exception occurs.
UNREGISTERED_TEST_CASE(FailCheckThrownTest, TestCase, {
	CHECK_THROWN(TestFailed, 2 + 2);
});
TEST_FAILING_TEST_CASE(FailCheckThrownTest, TestFailed,
					   "Expected an exception: 2 + 2");

// *** Make sure CHECK_THROWN fails if the wrong exception occurs.
UNREGISTERED_TEST_CASE(CheckThrownChecksTypeTest, TestCase, {
	CHECK_THROWN(TestFailed, THROW("unexpected exception type"));
});
TEST_FAILING_TEST_CASE(CheckThrownChecksTypeTest, TException,
					   "unexpected exception type");

UNREGISTERED_TEST_CASE(PassCheckEqualTest, BootstrapTestCase, {
	CHECK_EQ(2, 1 + 1);
	mTestPassed = true;
});
TEST_BOOTSTRAP_TEST_CASE(PassCheckEqualTest);

UNREGISTERED_TEST_CASE(FailCheckEqualTest, BootstrapTestCase, {
	CHECK_EQ(1, 1 + 1);
});
TEST_FAILING_TEST_CASE(FailCheckEqualTest, TestFailed,
					   "expected 1 == 1 + 1, got: 1, 2");

UNREGISTERED_TEST_CASE(FailStringCheckEqualTest, BootstrapTestCase, {
	CHECK_EQ(std::string("foo"), "bar");
});
TEST_FAILING_TEST_CASE(FailStringCheckEqualTest, TestFailed,
					   "expected std::string(\"foo\") == \"bar\", "
					   "got: foo, bar");

UNREGISTERED_TEST_CASE(PassCheckNotEqualTest, BootstrapTestCase, {
	CHECK_NE(1, 1 + 1);
	mTestPassed = true;
});
TEST_BOOTSTRAP_TEST_CASE(PassCheckNotEqualTest);

UNREGISTERED_TEST_CASE(FailCheckNotEqualTest, BootstrapTestCase, {
	CHECK_NE(2, 1 + 1);
});
TEST_FAILING_TEST_CASE(FailCheckNotEqualTest, TestFailed,
					   "expected 2 != 1 + 1, got: 2, 2");

class WasSetUpTest : public BootstrapTestCase {
public:
	void SetUp()   { mTestPassed = true; };
	void Test()    { CHECK_EQ(mTestPassed, true); }
};
TEST_BOOTSTRAP_TEST_CASE(WasSetUpTest);


//=========================================================================
//  TestRegistry Tests
//=========================================================================

UNREGISTERED_TEST_CASE(SimplePassingTestCase, TestCase, {
	CHECK_EQ(2, 2);
});

UNREGISTERED_TEST_CASE(SimpleFailingTestCase, TestCase, {
	CHECK_EQ(1, 1 + 1);
});

TEST_CASE(TestRegistryRegisterTest, TestCase, {
	TestRegistry registry;
	CHECK_EQ(registry.begin(), registry.end());
	TestCaseFactoryImpl<SimplePassingTestCase>
		created_factory("SimplePassingTestCase", &registry);
    CHECK_NE(registry.begin(), registry.end());
	TestCaseFactory *registered_factory = *registry.begin();
	CHECK_EQ(&created_factory, registered_factory);
	CHECK_EQ(registered_factory->GetName(), "SimplePassingTestCase");
	boost::shared_ptr<TestCase> test(registered_factory->Create());
	test->Run();
});

TEST_CASE(TestRegistryRunAllTest, TestCase, {
	TestRegistry registry;
	TestCaseFactoryImpl<SimplePassingTestCase>
		factory1("SimplePassingTestCase", &registry);
	TestCaseFactoryImpl<SimpleFailingTestCase>
		factory2("SimpleFailingTestCase", &registry);
	TestRunReport::ptr report = registry.RunAllTests();
	CHECK_EQ(report->GetNumTestsPassed(), 1);
	CHECK_EQ(report->GetNumTestsFailed(), 1);
	CHECK_EQ(report->GetNumTestsSkipped(), 0);
	CHECK_EQ(report->GetSummary(), "FAIL: 1 failed, 1 passed, 0 skipped"); 
	TestRunReport::iterator i = report->begin();
	CHECK_EQ((*i)->GetTestResult(), TEST_PASSED);
	CHECK_EQ((*i++)->GetName(), "SimplePassingTestCase");
	CHECK_EQ((*i)->GetTestResult(), TEST_FAILED);
	CHECK_EQ((*i)->GetName(), "SimpleFailingTestCase");
	CHECK_EQ((*i++)->GetErrorMessage(), "expected 1 == 1 + 1, got: 1, 2");
	CHECK_EQ(i, report->end());
});

TEST_CASE(TestRegistryRunAllOk, TestCase, {
	TestRegistry registry;
	TestCaseFactoryImpl<SimplePassingTestCase>
		factory1("SimplePassingTestCase", &registry);
	TestRunReport::ptr report = registry.RunAllTests();
	CHECK_EQ(report->GetSummary(), "OK: 1 passed, 0 skipped"); 
});

class DummyTestProgressMeter : public ITestProgressMeter {
	int mNextTestIndex;

public:
	DummyTestProgressMeter() : mNextTestIndex(0) {}

	int GetTestsSeenCount() { return mNextTestIndex; }

	void UpdateTestProgress(int inTestIndex, int inTestCount,
							const TestCaseReport &inReport)
	{
		ASSERT(0 <= inTestIndex && inTestIndex < inTestCount);
		CHECK_EQ(inTestIndex, mNextTestIndex++);
		CHECK_EQ(inTestCount, 3);
		if (inTestIndex == 1)
			CHECK_EQ(inReport.GetTestResult(),
					 TEST_FAILED);
		else
			CHECK_EQ(inReport.GetTestResult(),
					 TEST_PASSED);
	}
};

TEST_CASE(TestRegistryProgressMeterTest, TestCase, {
	TestRegistry registry;
	TestCaseFactoryImpl<SimplePassingTestCase>
		factory1("SimplePassingTestCase", &registry);
	TestCaseFactoryImpl<SimpleFailingTestCase>
		factory2("SimpleFailingTestCase", &registry);
	TestCaseFactoryImpl<SimplePassingTestCase>
		factory3("SimplePassingTestCase", &registry);
	DummyTestProgressMeter meter;
	registry.RunAllTests(&meter);
	CHECK_EQ(meter.GetTestsSeenCount(), 3);
});


//=========================================================================
//  TestReportFrame
//=========================================================================

class TestReportFrame : public wxFrame {
	wxTextCtrl *mOutput;

public:
	TestReportFrame(wxFrame *inParent, TestRunReport::ptr inReport);
};

TestReportFrame::TestReportFrame(wxFrame *inParent,
								 TestRunReport::ptr inReport)
	: wxFrame(inParent, -1, "Test Results")
{
    mOutput = new wxTextCtrl(this, -1, "", wxDefaultPosition,
							 wxDefaultSize,
							 wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH);
	
    wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
    sizer->Add(mOutput, 1 /* stretch */, wxGROW, 0);
    SetSizer(sizer);
    sizer->SetSizeHints(this);

    SetClientSize(640, 240);	

	mOutput->AppendText(inReport->GetSummary().c_str());
	mOutput->AppendText("\n");
	TestRunReport::iterator i = inReport->begin();
	for (; i != inReport->end(); ++i) {
		if ((*i)->GetTestResult() == TEST_FAILED) {
			wxString str;
			str.Printf("%s:%d: %s: %s\n",
					   (*i)->GetErrorFile().c_str(),
					   (*i)->GetErrorLine(),
					   (*i)->GetName().c_str(),
					   (*i)->GetErrorMessage().c_str());
			mOutput->AppendText(str);
		}
	}
}

//=========================================================================
//  TestHarness
//=========================================================================

TestHarness *TestHarness::sInstance = NULL;

TestHarness *TestHarness::GetInstance()
{
	if (!sInstance)
		sInstance = new TestHarness();
	return sInstance;
}

void TestHarness::RunTests()
{
	// Get some useful data.
	StageFrame *frame = wxGetApp().GetStageFrame();
	FancyStatusBar *status_bar =
		dynamic_cast<FancyStatusBar*>(frame->GetStatusBar());
	ASSERT(status_bar);

	// Prepare to run tests.
	frame->SetStatusText("Running tests...");
	status_bar->SetProgress(0.0);
	status_bar->SetProgressColor(FancyStatusBar::DEFAULT_PROGRESS_COLOR);

	// Run all the tests in the global registry.
	TestRegistry *registry = TestRegistry::GetGlobalRegistry();
	TestRunReport::ptr report = registry->RunAllTests();

	// Display the results of the tests to the user.
	status_bar->SetProgress(1.0);
	frame->SetStatusText(report->GetSummary().c_str());
	if (!report->AnyTestFailed())
		status_bar->SetProgressColor(*wxGREEN);
	else
	{
		status_bar->SetProgressColor(*wxRED);
		(new TestReportFrame(wxGetApp().GetStageFrame(), report))->Show();
	}
}
