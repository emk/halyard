// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <iostream>
#include <sstream>

#include "TestCase.h"
#include "FiveLApp.h"
#include "TestHarness.h"
#include "Stage.h"
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
