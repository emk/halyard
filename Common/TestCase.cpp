// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"

#include "TestCase.h"

REGISTER_TEST_CASE_FILE(TestCase);

// Static variables definitions.
TestRegistry *TestRegistry::sGlobalRegistry = NULL;


//=========================================================================
//  TestCase Methods
//=========================================================================

void TestCase::Run() {
	SetUp();
	try {
		Test();
	} catch (...) {
		TearDown();
		throw;
	}
	TearDown();
}


//=========================================================================
//  TestCaseFactory Methods
//=========================================================================

TestCaseFactory::TestCaseFactory(const char *inName, TestRegistry *inRegistry)
	: mName(inName)
{
	inRegistry->RegisterTestCaseFactory(this);
}


//=========================================================================
//  TestCaseReport Methods
//=========================================================================

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

std::string TestCaseReport::GetSummaryIfInteresting() const {
	std::ostringstream out;
	if (GetTestResult() == TEST_FAILED) {
		out << GetErrorFile() << ":" << GetErrorLine() << ": "
			<< GetName() << ": " << GetErrorMessage() << std::endl;
	}
	return out.str();
}


//=========================================================================
//  TestRunReport Methods
//=========================================================================

void TestRunReport::AddTestCaseReport(TestCaseReport::ptr inTestCaseReport) {
	mResultCount[inTestCaseReport->GetTestResult()]++;
	mTestCaseReports.push_back(inTestCaseReport);
}

std::string TestRunReport::GetSummary() {
	std::ostringstream out;
	if (AnyTestFailed())
		out << "FAIL: " << GetNumTestsFailed() << " failed, ";
	else
		out << "OK: ";
	out << GetNumTestsPassed() << " passed, "
		<< GetNumTestsSkipped() << " skipped";
	return out.str();
}


//=========================================================================
//  TestRegistry Methods
//=========================================================================

TestRegistry *TestRegistry::GetGlobalRegistry() {
	if (sGlobalRegistry == NULL)
		sGlobalRegistry = new TestRegistry;
	return sGlobalRegistry;
}

void TestRegistry::RegisterTestCaseFactory(TestCaseFactory *inFactory) {
	mTestCaseFactories.push_back(inFactory);
}

TestRunReport::ptr TestRegistry::RunAllTests(ITestProgressMeter *inMeter) {
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


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES


//-------------------------------------------------------------------------
//  TestCase Tests
//-------------------------------------------------------------------------

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
			CHECK(failure.GetErrorMessage() == WantMsg(),
				  "Didn't get the right message.");
		}
		CHECK(caught_exception, "Didn't catch exception.");
	}
};

#define TEST_FAILING_TEST_CASE(NAME, EXCEPTION, WANT_MSG) \
    class Test##NAME \
		: public TestFailingTestCase<NAME, EXCEPTION> { \
			std::string WantMsg() { return WANT_MSG; } \
	}; \
    REGISTER_TEST_CASE(Test##NAME)

// *** Make sure tests actually get run.
BEGIN_UNREGISTERED_TEST_CASE(WasRunTest, BootstrapTestCase) {
	mTestPassed = true;
} END_UNREGISTERED_TEST_CASE(WasRunTest);
TEST_BOOTSTRAP_TEST_CASE(WasRunTest);

// *** Make sure FAIL_TEST throws an exception.
BEGIN_UNREGISTERED_TEST_CASE(FailThrowsExceptionTest, TestCase) {
	FAIL_TEST("my message");
} END_UNREGISTERED_TEST_CASE(WasRunTest);
TEST_FAILING_TEST_CASE(FailThrowsExceptionTest, TestFailed, "my message");

// *** Make sure CHECK_THROWN blocks exceptions.
BEGIN_UNREGISTERED_TEST_CASE(PassCheckThrownTest, BootstrapTestCase) {
	CHECK_THROWN(TException, THROW("Random exception"));
	mTestPassed = true;
} END_UNREGISTERED_TEST_CASE(PassCheckThrownTest);
TEST_BOOTSTRAP_TEST_CASE(PassCheckThrownTest);

// *** Make sure CHECK_THROWN fails if no exception occurs.
BEGIN_UNREGISTERED_TEST_CASE(FailCheckThrownTest, TestCase) {
	CHECK_THROWN(TestFailed, 2 + 2);
} END_UNREGISTERED_TEST_CASE(FailCheckThrownTest);
TEST_FAILING_TEST_CASE(FailCheckThrownTest, TestFailed,
					   "Expected an exception: 2 + 2");

// *** Make sure CHECK_THROWN fails if the wrong exception occurs.
BEGIN_UNREGISTERED_TEST_CASE(CheckThrownChecksTypeTest, TestCase) {
	CHECK_THROWN(TestFailed, THROW("unexpected exception type"));
} END_UNREGISTERED_TEST_CASE(CheckThrownChecksTypeTest);
TEST_FAILING_TEST_CASE(CheckThrownChecksTypeTest, TException,
					   "unexpected exception type");

BEGIN_UNREGISTERED_TEST_CASE(PassCheckEqualTest, BootstrapTestCase) {
	CHECK_EQ(2, 1 + 1);
	mTestPassed = true;
} END_UNREGISTERED_TEST_CASE(PassCheckEqualTest);
TEST_BOOTSTRAP_TEST_CASE(PassCheckEqualTest);

BEGIN_UNREGISTERED_TEST_CASE(FailCheckEqualTest, BootstrapTestCase) {
	CHECK_EQ(1, 1 + 1);
} END_UNREGISTERED_TEST_CASE(FailCheckEqualTest);
TEST_FAILING_TEST_CASE(FailCheckEqualTest, TestFailed,
					   "expected 1 == 1 + 1, got: 1, 2");

BEGIN_UNREGISTERED_TEST_CASE(FailStringCheckEqualTest, BootstrapTestCase) {
	CHECK_EQ(std::string("foo"), "bar");
} END_UNREGISTERED_TEST_CASE(FailStringCheckEqualTest);
TEST_FAILING_TEST_CASE(FailStringCheckEqualTest, TestFailed,
					   "expected std::string(\"foo\") == \"bar\", "
					   "got: foo, bar");

BEGIN_UNREGISTERED_TEST_CASE(PassCheckNotEqualTest, BootstrapTestCase) {
	CHECK_NE(1, 1 + 1);
	mTestPassed = true;
} END_UNREGISTERED_TEST_CASE(PassCheckNotEqualTest);
TEST_BOOTSTRAP_TEST_CASE(PassCheckNotEqualTest);

BEGIN_UNREGISTERED_TEST_CASE(FailCheckNotEqualTest, BootstrapTestCase) {
	CHECK_NE(2, 1 + 1);
} END_UNREGISTERED_TEST_CASE(FailCheckNotEqualTest);
TEST_FAILING_TEST_CASE(FailCheckNotEqualTest, TestFailed,
					   "expected 2 != 1 + 1, got: 2, 2");

class WasSetUpTest : public BootstrapTestCase {
public:
	void SetUp()    { mTestPassed = true; };
	void Test()     { CHECK_EQ(mTestPassed, true); }
};
TEST_BOOTSTRAP_TEST_CASE(WasSetUpTest);

class TearDownTestCase : public BootstrapTestCase {
public:
	void TearDown() { mTestPassed = true; }	
};

BEGIN_UNREGISTERED_TEST_CASE(WasTornDownTest, TearDownTestCase) {
	CHECK_EQ(mTestPassed, false);
} END_UNREGISTERED_TEST_CASE(WasTornDownTest);
TEST_BOOTSTRAP_TEST_CASE(WasTornDownTest);

BEGIN_UNREGISTERED_TEST_CASE(WasTornDownAfterFailingTest, TearDownTestCase) {
	FAIL_TEST("test failed");
} END_UNREGISTERED_TEST_CASE(WasTornDownAfterFailingTest);
BEGIN_TEST_CASE(TestWasTornDownAfterFailingTest, TestCase) {
	WasTornDownAfterFailingTest test;
	CHECK_THROWN(TestFailed, test.Run());
	CHECK_EQ(test.mTestPassed, true);
} END_TEST_CASE(TestWasTornDownAfterFailingTest);


//-------------------------------------------------------------------------
//  TestFactory/TestRegistry/Test*Report Tests
//-------------------------------------------------------------------------

BEGIN_UNREGISTERED_TEST_CASE(SimplePassingTestCase, TestCase) {
	CHECK_EQ(2, 2);
} END_UNREGISTERED_TEST_CASE(SimplePassingTestCase);

BEGIN_UNREGISTERED_TEST_CASE(SimpleFailingTestCase, TestCase) {
	CHECK_EQ(1, 1 + 1);
} END_UNREGISTERED_TEST_CASE(SimpleFailingTestCase);

BEGIN_TEST_CASE(TestRegistryRegisterTest, TestCase) {
	TestRegistry registry;
	CHECK_EQ(registry.begin(), registry.end());
	TestCaseFactoryImpl<SimplePassingTestCase>
		created_factory("SimplePassingTestCase", &registry);
    CHECK_NE(registry.begin(), registry.end());
	TestCaseFactory *registered_factory = *registry.begin();
	CHECK_EQ(&created_factory, registered_factory);
	CHECK_EQ(registered_factory->GetName(), "SimplePassingTestCase");
	shared_ptr<TestCase> test(registered_factory->Create());
	test->Run();
} END_TEST_CASE(TestRegistryRegisterTest);

BEGIN_TEST_CASE(TestRegistryRunAllTest, TestCase) {
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
} END_TEST_CASE(TestRegistryRunAllTest);

BEGIN_TEST_CASE(TestRegistryRunAllOk, TestCase) {
	TestRegistry registry;
	TestCaseFactoryImpl<SimplePassingTestCase>
		factory1("SimplePassingTestCase", &registry);
	TestRunReport::ptr report = registry.RunAllTests();
	CHECK_EQ(report->GetSummary(), "OK: 1 passed, 0 skipped"); 
} END_TEST_CASE(TestRegistryRunAllOk);

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

BEGIN_TEST_CASE(TestRegistryProgressMeterTest, TestCase) {
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
} END_TEST_CASE(TestRegistryProgressMeterTest);

#endif // BUILD_TEST_CASES
