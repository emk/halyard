// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "FiveLApp.h"
#include "TestHarness.h"
#include "StageFrame.h"
#include "FancyStatusBar.h"


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

TestHarness::TestHarness()
{
	mFrame = wxGetApp().GetStageFrame();
	mStatusBar = dynamic_cast<FancyStatusBar*>(mFrame->GetStatusBar());
	ASSERT(mStatusBar);
}

void TestHarness::UpdateTestProgress(int inTestIndex, int inTestCount,
									 const TestCaseReport &inReport)
{
	mStatusBar->SetProgress((inTestIndex + 1.0) / inTestCount);
	if (inReport.GetTestResult() == TEST_FAILED)
		mStatusBar->SetProgressColor(*wxRED);
}

void TestHarness::RunTests()
{
	// Prepare to run tests.
	mFrame->SetStatusText("Running tests...");
	mStatusBar->SetProgress(0.0);
	mStatusBar->SetProgressColor(FancyStatusBar::DEFAULT_PROGRESS_COLOR);

	// Run all the tests in the global registry.
	TestRegistry *registry = TestRegistry::GetGlobalRegistry();
	TestRunReport::ptr report = registry->RunAllTests();

	// Display the results of the tests to the user.
	mStatusBar->SetProgress(1.0);
	mFrame->SetStatusText(report->GetSummary().c_str());
	if (!report->AnyTestFailed())
		mStatusBar->SetProgressColor(*wxGREEN);
	else
		(new TestReportFrame(mFrame, report))->Show();
}
