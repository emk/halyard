// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/fs_inet.h>
#include <wx/xrc/xmlres.h>

#include "TStartup.h"
#include "TDeveloperPrefs.h"

#include "AppConfig.h"
#if CONFIG_HAVE_QUICKTIME
#	include "TQTMovie.h"
#endif // CONFIG_HAVE_QUICKTIME
#include "AppGlobals.h"
#include "FiveLApp.h"
#include "Log5L.h"
#include "Stage.h"
#include "dlg/StartupDlg.h"
#include "TWxPrimitives.h"
#if CONFIG_HAVE_QUAKE2
#	include "TQuake2Primitives.h"
#endif // CONFIG_HAVE_QUAKE2

// Provided by auto-generated resources.cpp file.
extern void InitXmlResource();

USING_NAMESPACE_FIVEL

IMPLEMENT_APP(FiveLApp)

FiveLApp::FiveLApp()
    : mHaveOwnEventLoop(false), mStageFrame(NULL)
{
    // Do nothing here but set up instance variables.  Real work should
    // happen in FiveLApp::OnInit, below.
}

void FiveLApp::IdleProc()
{
	// We don't allow more than five consecutive idles before returning to
	// Scheme.  Returning to Scheme is very fast, and we may need to
	// process a jump command.
	int max_idles = 5;

    // Dispatch all queued events and return from the IdleProc.  Note that
    // the '||' operator short-circuts, and will prevent us from calling
    // ProcessIdle until the pending queue is drained.  See the ProcessIdle
    // documentation for an explanation of when and why we might need to
    // call it multiple times.
    while (wxGetApp().Pending() || (wxGetApp().ProcessIdle() && --max_idles))
    {
        if (wxGetApp().Pending())
            wxGetApp().Dispatch();
    }
}

bool FiveLApp::OnInit()
{
	// Name our application.
	SetAppName("Tamale");

    // Get the 5L runtime going.
    ::InitializeCommonCode();
    ::RegisterWxPrimitives();
#if CONFIG_HAVE_QUAKE2
	::RegisterQuake2Primitives();
#endif // CONFIG_HAVE_QUAKE2

    // Send copies of all wxWindows logging messages to our traditional 5L
    // logs.  This gives us a single copy of everything.
    // TODO - How do we clean up these resources?
    wxLog::SetActiveTarget(new wxLogGui());
    new wxLogChain(new Log5L());

    // Configure some useful trace masks for debugging the application.
    // Comment these out to disable a particular kind of tracing.
    //wxLog::AddTraceMask(TRACE_STAGE_DRAWING);
    //wxLog::AddTraceMask(wxTRACE_Messages);
    //wxLog::SetTraceMask(wxTraceMessages);

#if CONFIG_HAVE_QUICKTIME
    // Start up QuickTime.
    TQTMovie::InitializeMovies();
#endif // CONFIG_HAVE_QUICKTIME

    // Initialize some optional wxWindows features.
    ::wxInitAllImageHandlers();
    wxFileSystem::AddHandler(new wxInternetFSHandler);

	// Load our resources (they're linked into our application).
	// TODO - Only initialize the handlers we need; this will greatly
	// reduce application size.
	wxXmlResource::Get()->InitAllHandlers();
	InitXmlResource();

    // Create and display our stage frame.
    //mStageFrame = new StageFrame(wxSize(640, 480));
    mStageFrame = new StageFrame(wxSize(800, 600));
    mStageFrame->Show();
	// Enable this to go to full-screen mode *almost* immediately.
	// TODO - You'll see the standard window for a small fraction of a
	// second.  Fixing this will require tweaking wxWindows.
	//mStageFrame->ShowFullScreen(TRUE);
    SetTopWindow(mStageFrame);

    return TRUE;
}

int FiveLApp::OnExit()
{
    // Shut down QuickTime.  wxWindows guarantees to have destroyed
    // all windows and frames by this point.
#if CONFIG_HAVE_QUICKTIME
    TQTMovie::ShutDownMovies();
#endif // CONFIG_HAVE_QUICKTIME

    // XXX - Undocumented return value, trying 0 for now.
    return 0;
}

int FiveLApp::MainLoop()
{
	// WARNING - No Scheme function may ever be called above this
    // point on the stack!
    FIVEL_SET_STACK_BASE();
        
    // Create a SchemeInterpreterManager.
    TInterpreterManager *manager =
		GetSchemeInterpreterManager(&FiveLApp::IdleProc);

	// Prompt for a program to open.  We can't do this until the
	// TInterpreterManager is loaded.  (We need to do this inside
	// a block to make sure the object is destroyed quickly.)
	{
		StartupDlg startup_dialog(mStageFrame);
		startup_dialog.ShowModal();
	}

	// Run our own event loop.
	SetExitOnFrameDelete(FALSE);
	mHaveOwnEventLoop = true;
	IdleProc();
	manager->Run();
	delete manager;
	manager = NULL;

	// We could run the built-in event loop using the following call,
	// if there were some reason the above code would not work (perhaps
	// because of a starup error):
	//    return wxApp::MainLoop();

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

Stage *FiveLApp::GetStage()
{
    return GetStageFrame()->GetStage();
}
