// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "FiveLApp.h"
#include "TestHarness.h"
#include "Stage.h"
#include "FancyStatusBar.h"

void TestHarness::StartTests()
{
	StageFrame *frame = wxGetApp().GetStageFrame();
	FancyStatusBar *status_bar =
		dynamic_cast<FancyStatusBar*>(frame->GetStatusBar());
	ASSERT(status_bar);

	frame->SetStatusText("Running tests...");
	frame->SetStatusText("All tests passed.");
}

void TestHarness::Idle()
{
	
}
