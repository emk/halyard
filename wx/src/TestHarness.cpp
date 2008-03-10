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

#include "AppHeaders.h"

#include "FiveLApp.h"
#include "TestHarness.h"
#include "StageFrame.h"
#include "FancyStatusBar.h"

// This is as good a place to reference extra test case files as any.
// TODO - Figure out what to with list of test cases in TestAll.cpp.
REFERENCE_TEST_CASE_FILE(BinMsg);
REFERENCE_TEST_CASE_FILE(BufferSpan);


//=========================================================================
//  TestReportFrame
//=========================================================================

/// A simple window for displaying a TestRunReport.
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
