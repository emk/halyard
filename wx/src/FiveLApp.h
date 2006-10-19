// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#ifndef FiveLApp_H
#define FiveLApp_H

class StageFrame;
class Stage;
class Log5L;

/// Our main application object.
class FiveLApp : public wxApp
{
    DECLARE_EVENT_TABLE();

    //////////
    /// Set to true when we are in the middle of a crash and we don't want
    /// to try to do anything complicated.
    ///
    static bool sHandlingFatalError;

    //////////
    /// Our custom logger which redirects wxWidget errors to our own
    /// logging subsystem.
    ///
    static Log5L *sLog5L;

    //////////
    /// The name of the script we were passed on the command-line.
    ///
    wxString mArgScript;

	//////////
	/// Do we have our own, custom event loop?  Or should we use wxWindows'
	/// event loop?
	///
    bool mHaveOwnEventLoop;

    //////////
    /// Do we have a working error-reporting and logging sub-system yet?
    ///
    bool mLogsAreInitialized;

	//////////
	/// Our StageFrame.
	///
	StageFrame *mStageFrame;

    //////////
    /// Are we planning to launch the UpdateInstaller before we exit?
    ///
    bool mShouldLaunchUpdateInstaller; 

	//////////
	/// This procedure is called periodically by the script interpreter
	/// to give time to the GUI.
	///
    static void IdleProc(bool inBlock);

    //////////
    /// Do any cleanup we need to do before we crash.
    ///
    static void PrepareForCrash();

    //////////
    /// Display a generic error dialog.
    ///
    void ErrorDialog(const char* inTitle, const char *inMessage);

    //////////
    /// Report a fatal exception to whatever layer of error-handling
    /// machinery is prepared to receive it.
    ///
    void ReportFatalException(std::exception &e);

    //////////
    /// Report a fatal exception to whatever layer of error-handling
    /// machinery is prepared to receive it.
    ///
    void ReportFatalException();    

    //////////
    /// Launch the UpdateInstaller shortly before the program exits.
    ///
    void LaunchUpdateInstaller();

public:
    FiveLApp();

    //////////
    /// Notify the system that this application is still alive.  This
    /// should prevent "Not Responding" timeouts on Windows.  (Normally
    /// you can accomplish the same result by calling Idle, but that
    /// will also process events, which this function doesn't do.)
    ///
    void Heartbeat();

    //////////
    /// Tell the application to run the UpdateInstaller when it exits.
    ///
    void LaunchUpdateInstallerBeforeExiting();
    
	//////////
	/// We perform normal application initialization in OnInit. 
	///
    virtual bool OnInit();

	//////////
	/// We perform normal application cleanup in OnExit.
	///
    virtual int OnExit();

    //////////
    /// wxWidgets calls this function if any exceptions appear to
    /// be otherwise unhandled. Because this function doesn't get
    /// any information about the underlying exception, it can't
    /// give a particularly informative message.
    ///
    virtual void OnUnhandledException();

    //////////
    /// wxWidgets calls this function if a fatal signal (Unix) or
    /// structured exception (Windows) occurs.  These errors are extremely
    /// nasty--they include segfaults and such--and we will immediately
    /// begin engine shutdown.
    ///
    virtual void OnFatalException();

#ifdef __WXDEBUG__
    //////////
    /// Called whenever an assertion occurs in wxWidgets.
    ///
    virtual void OnAssert(const wxChar *file, int line, const wxChar *cond,
                          const wxChar *msg);
#endif // __WXDEBUG__

	//////////
    /// We attempt to replace the standard main loop with one that
	/// calls TInterpreterManager::Run.
	///
    virtual int MainLoop();

	//////////
	/// Because we've chosen to provide our own main loop, we need to
	/// override the standard "request application shutdown" function.
	///
    virtual void ExitMainLoop();

	//////////
	/// Remove the stage frame from this application.  This should only
	/// be called by the StageFrame.
	///
	void DetachStageFrame() { mStageFrame = NULL; }
	
	//////////
	/// Get the StageFrame for this application.
	///
	StageFrame *GetStageFrame() { ASSERT(mStageFrame); return mStageFrame; }
	
	//////////
	/// Get the Stage for this application.
	///
	Stage *GetStage();

    //////////
    /// Return true if and only if this application has a stage.
    ///
    bool HaveStage() { return mStageFrame != NULL; } 

    //////////
    /// We provide an OnActivateApp handler so we can hide full-screen
    /// windows and fix our screen resolution when we're not in the
    /// foreground.
    ///
    void OnActivateApp(wxActivateEvent &event);
};

//////////
/// Declare a 'FiveLApp &wxGetApp()' method.
///
DECLARE_APP(FiveLApp)

#endif // FiveLApp_H
