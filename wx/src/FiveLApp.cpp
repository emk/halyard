#include <wx/wx.h>

#include "TStartup.h"

#include "FiveLApp.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL

IMPLEMENT_APP(FiveLApp)

FiveLApp::FiveLApp()
    : mHaveOwnEventLoop(false)
{
    // Do nothing here but set up instance variables.  Real work should
    // happen in FiveLApp::OnInit, below.
}

void FiveLApp::IdleProc()
{
    // Dispatch all queued events and return from the IdleProc.
    while (wxGetApp().Pending())
        wxGetApp().Dispatch();
}

bool FiveLApp::OnInit()
{
    // Get the 5L runtime going...
    ::InitializeCommonCode();

    StageFrame *frame = new StageFrame("wx5L", wxSize(640, 480));
    frame->Show();
    SetTopWindow(frame);
    return TRUE;
}

int FiveLApp::MainLoop()
{
    // Try to create a SchemeInterpreterManager.
    TInterpreterManager *manager =
        ::MaybeGetSchemeInterpreterManager(&FiveLApp::IdleProc);

    if (manager)
    {
        // WARNING - No Scheme function may ever be called above this
        // point on the stack!
        FIVEL_SET_STACK_BASE();

        // Run our own event loop.
        SetExitOnFrameDelete(FALSE);
        mHaveOwnEventLoop = true;
		IdleProc();
        manager->Run();
        delete manager;
        manager = NULL;
    }
    else
    {
	wxLogError("Could not find a \"Runtime\" directory.");

        // Run the built-in main loop instead.
        return wxApp::MainLoop();
    }

    // XXX - Is this good enough?  This return value is platform-specific,
    // and it's not always 0.
    return 0;
}

void FiveLApp::ExitMainLoop()
{
    if (mHaveOwnEventLoop)
    {
        // Ask the TInterpreterManager to begin an orderly shutdown.
        ASSERT(TInterpreterManager::HaveInstance());
        TInterpreterManager::GetInstance()->RequestQuitApplication();
    }
    else
    {
        // Handle things normally.
        wxApp::ExitMainLoop();
    }
}
