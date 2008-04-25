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
#include <wx/image.h>
#include <wx/fs_inet.h>
#include <wx/xrc/xmlres.h>
#include <wx/evtloop.h>
#include <wx/sysopt.h>
#include <wx/stdpaths.h>

#include "TVersion.h"
#include "TStartup.h"
#include "TDeveloperPrefs.h"
#include "CrashReporter.h"

#include "AppConfig.h"
#if CONFIG_HAVE_FANCYCRASHREPORT
#include "FancyCrashReporter.h"
#endif // CONFIG_HAVE_FANCYCRASHREPORT
#if CONFIG_HAVE_QUICKTIME
#	include "TQTMovie.h"
#endif // CONFIG_HAVE_QUICKTIME
#include "AppGlobals.h"
#include "HalyardApp.h"
#include "AppLog.h"
#include "GuiUtil.h"
#include "StageFrame.h"
#include "dlg/StartupDlg.h"
#include "TWxPrimitives.h"
#if CONFIG_HAVE_QUAKE2
#	include "TQuake2Primitives.h"
#endif // CONFIG_HAVE_QUAKE2
#if CONFIG_HAVE_AUDIOSTREAMS
#	include "AudioStream.h"
#endif // CONFIG_HAVE_AUDIOSTREAMS
#include "Downloader.h"
#include "Stage.h"
#include "CommandLine.h"
#include "CommonWxConv.h"

// Provided by auto-generated resources.cpp file.
extern void InitXmlResource();

using namespace Halyard;


//=========================================================================
//  HalyardApp Methods
//=========================================================================

bool HalyardApp::sHandlingFatalError = false;
AppLog *HalyardApp::sAppLog = NULL;

IMPLEMENT_APP(HalyardApp)

BEGIN_EVENT_TABLE(HalyardApp, wxApp)
    EVT_ACTIVATE_APP(HalyardApp::OnActivateApp)
END_EVENT_TABLE()

HalyardApp::HalyardApp()
    : mHaveOwnEventLoop(false), mLogsAreInitialized(false), mStageFrame(NULL),
      mShouldLaunchUpdateInstaller(false)
{
    // Do nothing here but set up instance variables.  Real work should
    // happen in HalyardApp::OnInit, below.
}

void HalyardApp::Heartbeat() {
    // This should internally call the Windows function ::PeekMessage,
    // which is sufficient to mark our application as alive.
    (void) Pending();
}

void HalyardApp::LaunchUpdateInstallerBeforeExiting() {
    mShouldLaunchUpdateInstaller = true;
}

void HalyardApp::LaunchUpdateInstaller() {
    // PORTABILITY - We need to make the UpdateInstaller work elsewhere.
    // XXX - Extract application exe name from wxWidgets?
    // XXX - These paths must match the ones in updater.ss and in
    // FancyCrashReporter.cpp.
    FileSystem::Path src_root(FileSystem::GetScriptLocalDataDirectory());
    FileSystem::Path dst_root(FileSystem::GetBaseDirectory());
    FileSystem::Path updates_dir(src_root.AddComponent("Updates"));
    FileSystem::Path updater(updates_dir.AddComponent("UpdateInstaller"));
	FileSystem::Path halyard(dst_root.AddComponent("Halyard"));

	std::vector<std::string> clItems;
	clItems.push_back(src_root.ToNativePathString()); // Update data here.
	clItems.push_back(dst_root.ToNativePathString()); // Copy to here.
	clItems.push_back(halyard.ToNativePathString());   // Command to run after.
	clItems.push_back(dst_root.ToNativePathString()); //   Arg for command.

	CommandLine cl(clItems);
    if (!CommandLine::ExecAsync(updater.ToNativePathString(), cl))
        gLog.FatalError("Error installing update.");
}

void HalyardApp::IdleProc(bool inBlock)
{
	if (wxGetApp().HaveStage() && !wxGetApp().GetStage()->IsIdleAllowed())
		THROW("Tried to call (idle) at an unsafe time");

	// Constraints:
	//   1) Cannot leave until all pending events are dispatched,
	//      or else GUI responsiveness will drop dramatically.
	//   2) Cannot make a *blocking* call to Dispatch() until
	//      all outstanding idles have been processed, or
	//      else idle event processing will be delayed indefinitely.
	//   3) Must not process idles if events are pending, or else
	//      the application may freeze.
	//   4) If we're not in blocking mode, must place an upper limit
	//      on how many idles we process before returning to
	//      Scheme, or an infinitely repeating idler (such as wxQuake2)
	//      will prevent us from ever returning.

	// Run the idle loop, getting events from the user and giving time to
	// process idles. Block will be true when we want to block and only 
	// process user events, never going back to scheme until an event tells us
	// to.

	if (inBlock)
	{
		while (!wxGetApp().Pending() && wxGetApp().ProcessIdle())
			;
		wxGetApp().Dispatch();
	}

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

void HalyardApp::PrepareForCrash() {
    sHandlingFatalError = true;
    if (sAppLog)
        sAppLog->SilentlyLogNonFatalErrors();
    ShowSystemWindows();
}

void HalyardApp::ErrorDialog(const wxString &inTitle, const wxString &inMessage)
{
    // TODO: Several of the callers of this function should be
    // calling the new TLogger::EnvironmentError instead.
    wxMessageDialog dlg(NULL, inMessage, inTitle, wxOK|wxICON_ERROR);
    dlg.ShowModal();
}

void HalyardApp::ReportFatalException(std::exception &e) {
    if (mLogsAreInitialized) {
        TException::ReportFatalException(e);
    } else {
        ErrorDialog(wxT("Unexpected Error"), wxString(e.what(), wxConvLocal));
        exit(1);
    }
}

void HalyardApp::ReportFatalException() {
    if (mLogsAreInitialized) {
        TException::ReportFatalException();
    } else {
        ErrorDialog(wxT("Unexpected Error"), wxT("An unknown error occurred."));
        exit(1);
    }
}

void HalyardApp::OnUnhandledException() {
    ReportFatalException();
}

void HalyardApp::OnFatalException() {
    // We're dead.  I'd like to call ReportFatalException here (it does
    // better cleanup), but I haven't thought through the consequences.
    // So I'm leaving this alone.
    CrashReporter::GetInstance()->CrashNow();
}

#ifdef __WXDEBUG__

void HalyardApp::OnAssert(const wxChar *file, int line, const wxChar *cond,
                        const wxChar *msg)
{
    if (mLogsAreInitialized) {
        // We have a crash report, so don't even bother to pop up a dialog
        // before crashing.  We *do* append this to our application log
        // just in case the assertion actually occurred in the crash
        // reporter--at least we'll record it somewhere.
        wxString message;
        message << wxT("Assertion: ");
        if (msg)
            message << msg << wxT(": ");
        message << cond << wxT(" (at ") << file << wxT(":") << line 
                << wxT(")");
        std::string msg_string(message.mb_str());
        gLog.FatalError("wxWidgets %s", msg_string.c_str());
    } else {
        // We don't have a logger yet, so let wxWidgets handle this error.
        wxApp::OnAssert(file, line, cond, msg);
    }
}

#endif // __WXDEBUG__

#if defined __WXMSW__ && !wxCHECK_VERSION(2,6,2)

#include <windows.h>

// If we don't have at least wxWidgets 2.6.2 (IIRC), then there's no way to
// surpress the wxWidgets event loop, because Vadim hadn't merged the final
// crash reporting support at that point.  So let's just do this using
// native Win32 APIs.
//
// WARNING - This function _must not_ call Assert, Error, FatalError, etc.,
// or else it will cause an infinite loop.
static void SafeAlert(TLogger::LogLevel level, const char *message) {
	uint32 alertType = MB_TASKMODAL | MB_OK;
	switch (level) {
		case TLogger::LEVEL_LOG:
		case TLogger::LEVEL_CAUTION:
			alertType |= MB_ICONINFORMATION;
			break;

		case TLogger::LEVEL_ERROR:
			alertType |= MB_ICONSTOP;
			break;
	}
	::MessageBox(NULL, message, NULL, alertType);
}

#else // !(defined __WXMSW__ && !wxCHECK_VERSION(2,6,2))

// WARNING - This function _must not_ call Assert, Error, FatalError, etc.,
// or else it will cause an infinite loop.
//
// XXX - This function really needs to try to turn off the wxWidgets event
// loop, but I can't currently find the code for doing that.  So
// "SafeAlert" probably isn't as safe as we'd really like.
static void SafeAlert(TLogger::LogLevel inLevel, const char *inMessage) {\
    long style = wxOK;
    switch (inLevel) {
        case TLogger::LEVEL_LOG:
        case TLogger::LEVEL_CAUTION:
            style |= wxICON_ERROR;
            break;

        case TLogger::LEVEL_ERROR:
            style |= wxICON_INFORMATION;
            break;
    };
    wxMessageDialog dlg(NULL, ToWxString(inMessage), wxT("Halyard"), style);
    dlg.ShowModal();
}

#endif // !(defined __WXMSW__ && !wxCHECK_VERSION(2,6,2))

bool HalyardApp::OnInit() {
    // All code in this routine should be protected by an
    // exception-trapping block of some sort, because wxWidgets has weak
    // internal exception handling and doesn't tend to report exception
    // strings properly.
    //
    // Partway down this routine, we'll set mLogsAreInitialized to true,
    // allowing a more advanced error reporting system to take over.
    BEGIN_EXCEPTION_TRAPPER();

    // Name our application.
    SetAppName(wxString(SHORT_NAME, wxConvLocal));
    
    // Turn off default use of wxCLIP_CHILDREN.  While wxCLIP_CHILDREN
    // is a very nice feature, we currently rely on Stage repaints
    // overwriting MovieWindowQT objects to make them "transparent"
    // until the movie starts playing.  Yes, this is hackish behavior,
    // but right now, we're tuned for it.
    wxSystemOptions::SetOption(wxT("msw.window.no-clip-children"), 1);

    // Figure out where we can store log files and such.
    wxString dir(wxStandardPaths::Get().GetUserDataDir());
    if (!::wxDirExists(dir) && !::wxMkdir(dir)) {
        // We need to use this error dialog routine because we haven't
        // initialized our logging subsystem yet, and we can't call
        // gLog.FatalError(...).
        ErrorDialog(wxT("Cannot Create Directory"),
                    wxT("Unable to create ") + dir);
        return false;
    }
    FileSystem::SetAppDataDirectory(std::string(dir.mb_str()));
    
    // Figure out where we can store our large, machine-specific files.
    // TODO - Refactor out code shared with above, if it will actually gain
    // us anything.
    wxString local_dir(wxStandardPaths::Get().GetUserLocalDataDir());
    if (!::wxDirExists(local_dir) && !::wxMkdir(local_dir)) {
        ErrorDialog(wxT("Cannot Create Directory"),
                    wxT("Unable to create ") + local_dir);
        return false;
    }
    FileSystem::SetAppLocalDataDirectory(std::string(local_dir.mb_str()));

    // Use a GUI alert dialog.
    TLogger::RegisterAlertDisplayFunction(SafeAlert);

    // Get the Halyard runtime going.
#if CONFIG_HAVE_FANCYCRASHREP
    ::InitializeCommonCode(new FancyCrashReporter());
#else
    ::InitializeCommonCode(new CrashReporter());
#endif // CONFIG_HAVE_FANCYCRASHREP
    mLogsAreInitialized = true;

    ::RegisterWxPrimitives();
#if CONFIG_HAVE_QUAKE2
    ::RegisterQuake2Primitives();
#endif // CONFIG_HAVE_QUAKE2
    
    // Send all wxWindows logging messages to our Halyard
    // logs.  This gives us a single copy of everything.
    // TODO - How do we clean up these resources?
    sAppLog = new AppLog();
    wxLog::SetActiveTarget(sAppLog);
    
    // Configure some useful trace masks for debugging the application.
    // Comment these out to disable a particular kind of tracing.
    //wxLog::AddTraceMask(TRACE_STAGE_DRAWING);
    //wxLog::AddTraceMask(wxTRACE_Messages);
    //wxLog::SetTraceMask(wxTraceMessages);
    
#if CONFIG_HAVE_QUICKTIME
    // Start up QuickTime.
    try {
        TQTMovie::InitializeMovies();
    } catch (std::exception &) {
        // We don't call gLog.FatalError here, because this isn't
        // really an error, it's a configuration problem, and we don't
        // really want crash reports about it.
        ErrorDialog(wxT("QuickTime Unavailable"),
                    wxT("Could not set up QuickTime. Please ")
                    wxT("make sure QuickTime is properly installed."));
        return false;
    }
    RegisterQuickTimePrimitives();
#endif // CONFIG_HAVE_QUICKTIME
    
#if CONFIG_HAVE_AUDIOSTREAMS
    // Start up our audio synthesis layer.
    AudioStream::InitializeStreams();
#endif // CONFIG_HAVE_AUDIOSTREAMS

    // Initialize some optional wxWindows features.
    ::wxInitAllImageHandlers();
    wxFileSystem::AddHandler(new wxInternetFSHandler);
    
    // Load our resources (they're linked into our application).
    // TODO - Only initialize the handlers we need; this will greatly
    // reduce application size.
    wxXmlResource::Get()->InitAllHandlers();
    InitXmlResource();
    
    // Parse our command-line arguments.
	size_t ac; wxChar **av;
    for (ac = argc-1, av = argv+1; ac > 0; --ac, ++av) {
        wxString arg(av[0]);
        if (ac >= 2 && arg == wxT("-e")) {
            wxString next(av[1]);
            --ac, ++av;
            TInterpreterManager::SetInitialCommand(std::string(next.mb_str()));
        } else {
            TInterpreterManager::SetRuntimeMode(true);
            mArgScript = arg;
        }
    }

    // Make sure we restore the taskbar, etc., before exiting with
    // an error message.
    TLogger::RegisterExitPrepFunction(&PrepareForCrash);

    // Create and display our stage frame.
    //mStageFrame = new StageFrame(wxSize(640, 480));
    mStageFrame = new StageFrame(wxSize(800, 600));
    mStageFrame->Show();
    // Enable this to go to full-screen mode *almost* immediately.
    // TODO - You'll see the standard window for a small fraction of a
    // second.  Fixing this will require tweaking wxWindows.
    // TODO - Well, the behavior looks slightly better with the current
    // version of wxWidgets.  We'll probably need to tweak this some more.
    if (TInterpreterManager::IsInRuntimeMode())
        mStageFrame->ShowFullScreen(TRUE);
    SetTopWindow(mStageFrame);

    END_EXCEPTION_TRAPPER(ReportFatalException);
    
    return true;
}

int HalyardApp::OnExit() {
    BEGIN_EXCEPTION_TRAPPER();

    // Make sure we put back the taskbar, etc.
    ShowSystemWindows();

#if CONFIG_HAVE_AUDIOSTREAMS
	// Shut down our audio synthesis layer.
	AudioStream::ShutDownStreams();
#endif // CONFIG_HAVE_AUDIOSTREAMS
    
    // Shut down QuickTime.  wxWindows guarantees to have destroyed
    // all windows and frames by this point.
#if CONFIG_HAVE_QUICKTIME
    TQTMovie::ShutDownMovies();
#endif // CONFIG_HAVE_QUICKTIME
    
    END_EXCEPTION_TRAPPER(ReportFatalException);

    // Launch the UpdateInstaller if we've been asked to do so.
    if (mShouldLaunchUpdateInstaller)
        LaunchUpdateInstaller();

    // XXX - Undocumented return value, trying 0 for now.
    return 0;
}

// If we're building with our custom-patched version of wxWidgets 2.6.1p1,
// we need to do this the hard way.
#if defined __WXMSW__ && HAVE_CUSTOM_WXWIDGETS

namespace {
    // HACK - Do the song and dance required to get a custom event loop running
    // with wxWidgets 2.6.  This is a gross hack, and requires
    // patching wxWidgets itself.  A better interface is available on some
    // platforms in wxWidgets 2.8; see below.
    class StEventLoopSetup {
        wxEventLoop **m_evtloop_var;
        
    public:
        StEventLoopSetup(wxEventLoop **evtloop_var)
            : m_evtloop_var(evtloop_var)
        {
            *m_evtloop_var = new wxEventLoop;
            (*m_evtloop_var)->StartRunning(); // Call our patch.
        }

        ~StEventLoopSetup() {
            (*m_evtloop_var)->StopRunning(); // Call our patch.
            delete *m_evtloop_var;
            *m_evtloop_var = NULL;
        }
    };
};

int HalyardApp::MainLoop() {
    // Create a wxEventLoop object to process events.  This became
    // necessary in wxWindows 2.5.x or so.
    StEventLoopSetup setup(&m_mainLoop);
    return MainLoopInternal();
}

// This version of the event loop should work on some wxWidgets 2.8
// platforms, but probably not all of them.  If it works on your platform,
// please add it to the conditional.
#elif wxCHECK_VERSION(2,8,0) && ((defined __WXMAC_CARBON__) && !wxMAC_USE_RUN_APP_EVENT_LOOP)

#include "wx/ptr_scpd.h"

class HalyardEventLoop : public wxEventLoop {
public:
    virtual int Run() {
        ASSERT(!IsRunning());
        wxEventLoopActivator activate(wx_static_cast(wxEventLoop *, this));
        return wxGetApp().MainLoopInternal();
    }
};

wxDEFINE_TIED_SCOPED_PTR_TYPE(wxEventLoop);

int HalyardApp::MainLoop() {
    wxEventLoopTiedPtr mainLoop(&m_mainLoop, new HalyardEventLoop);
    return m_mainLoop->Run();
}

#else
#error "No implementation of HalyardApp::MainLoop on this platform"
#endif

int HalyardApp::MainLoopInternal() {
    bool error = false;

    BEGIN_EXCEPTION_TRAPPER();
        
    // Create a SchemeInterpreterManager.
    TInterpreterManager *manager =
		GetSchemeInterpreterManager(&HalyardApp::IdleProc);

	Downloader *downloader = new Downloader();

    // Check to see if we have any command-line arguments.
    if (TInterpreterManager::IsInRuntimeMode()) {
        // Open the specified document directly.
        mStageFrame->OpenDocument(mArgScript);
    } else {
        // Prompt for a program to open.  We can't do this until the
        // TInterpreterManager is loaded.  (We need to do this inside
        // a block to make sure the dialog is destroyed quickly.)
		StartupDlg startup_dialog(mStageFrame);
		startup_dialog.ShowModal();
	}

	// Run our own event loop.
	SetExitOnFrameDelete(FALSE);
	mHaveOwnEventLoop = true;
	IdleProc(false);
	manager->Run();
    error = manager->ExitedWithError();
	delete downloader;
	downloader = NULL;
	delete manager;
	manager = NULL;

	// We could run the built-in event loop using the following call,
	// if there were some reason the above code would not work (perhaps
	// because of a starup error):
	//    return wxApp::MainLoop();

    END_EXCEPTION_TRAPPER(ReportFatalException);

    // Return our application's exit code.  This function is documented as
    // returning zero, but the exit code is really passed onto OnRun, the
    // return value of which is documented to be the application's exit
    // code.
    return error ? 1 : 0;
}

void HalyardApp::ExitMainLoop()
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
        // XXX - It's not clear that this branch of the 'if' conditional
        // does anything sane and/or useful any more.
        wxApp::ExitMainLoop();
    }
}

Stage *HalyardApp::GetStage()
{
    return GetStageFrame()->GetStage();
}

void HalyardApp::OnActivateApp(wxActivateEvent &event) {
    // XXX - If we're dealing with a fatal error, and the window tries to
    // maximize, ignore this event completely, so our window stops
    // deiconizing while trying to run the crash reporter, causing all
    // sorts of headaches.  This code should become unncessary when we
    // upgrade to the latest version of wxWidgets 2.6.x, which prevents
    // events from being processed while the crash reporter is displayed.
    //
    // This is a band-aid because we don't want to make a more systematic
    // and disruptive fix this close to a ship date.
    if (sHandlingFatalError && event.GetActive())
        return;

    // If we're being deactivated, and we have a full-screen window,
    // iconize it so the user can see the desktop.  Note that we also need
    // to deiconize the StageFrame here (in at least some cases) to prevent
    // a bug where the user can't deiconize the window.
	if (mStageFrame && mStageFrame->IsFullScreen())
		mStageFrame->Iconize(!event.GetActive());
    event.Skip();
}
