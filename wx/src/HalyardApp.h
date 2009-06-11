// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef HalyardApp_H
#define HalyardApp_H

class StageFrame;
class Stage;
class AppLog;
class wxFileConfig;
class wxConfigBase;

/// Our main application object.
class HalyardApp : public wxApp
{
    DECLARE_EVENT_TABLE();

    /// Set to true when we are in the middle of a crash and we don't want
    /// to try to do anything complicated.
    static bool sHandlingFatalError;

    /// Our custom logger which redirects wxWidget errors to our own
    /// logging subsystem.
    static AppLog *sAppLog;

    /// The name of the script we were passed on the command-line.
    wxString mArgScript;

    /// Do we have our own, custom event loop?  Or should we use wxWindows'
    /// event loop?
    bool mHaveOwnEventLoop;

    /// Do we have a working error-reporting and logging sub-system yet?
    bool mLogsAreInitialized;

    /// Our StageFrame.
    StageFrame *mStageFrame;

    /// Are we planning to launch the UpdateInstaller before we exit?
    bool mShouldLaunchUpdateInstaller; 

    /// A local configuration file for the currently loaded project.
    shared_ptr<wxFileConfig> mUserConfig;

    /// This procedure is called periodically by the script interpreter
    /// to give time to the GUI.
    static void IdleProc(bool inBlock);

    /// Do any cleanup we need to do before we crash.
    static void PrepareForCrash();

    /// Display a generic error dialog.
    void ErrorDialog(const wxString &inTitle, const wxString &inMessage);

    /// Report a fatal exception to whatever layer of error-handling
    /// machinery is prepared to receive it.
    void ReportFatalException(std::exception &e);

    /// Report a fatal exception to whatever layer of error-handling
    /// machinery is prepared to receive it.
    void ReportFatalException();    

    /// Launch the UpdateInstaller shortly before the program exits.
    void LaunchUpdateInstaller();

    /// Return the name of our configuration file.
    wxString UserConfigFilename();

    /// Read in the current user configuration, if we haven't already, and
    /// it's actually possible to do so.
    ///
    /// For our user config, we use a single object that is available
    /// for the duration of the program, rather than creating a new
    /// config object each time we use it.  This makes it easier to
    /// allow HalyardApp to write the config out to a file, as a
    /// non-standard wxFileConfig doesn't know where to save.  This
    /// also means that if someone manually edits the config file
    /// while running the program, the changes won't be picked up and
    /// in fact will likely be overwritten.
    void MaybeLoadUserConfig();
    
public:
    HalyardApp();

    /// Notify the system that this application is still alive.  This
    /// should prevent "Not Responding" timeouts on Windows.  (Normally
    /// you can accomplish the same result by calling Idle, but that
    /// will also process events, which this function doesn't do.)
    void Heartbeat();

    /// Tell the application to run the UpdateInstaller when it exits.
    void LaunchUpdateInstallerBeforeExiting();
    
    /// We perform normal application initialization in OnInit. 
    virtual bool OnInit();

    /// We perform normal application cleanup in OnExit.
    virtual int OnExit();

    /// wxWidgets calls this function if any exceptions appear to
    /// be otherwise unhandled. Because this function doesn't get
    /// any information about the underlying exception, it can't
    /// give a particularly informative message.
    virtual void OnUnhandledException();

    /// wxWidgets calls this function if a fatal signal (Unix) or
    /// structured exception (Windows) occurs.  These errors are extremely
    /// nasty--they include segfaults and such--and we will immediately
    /// begin engine shutdown.
    virtual void OnFatalException();

#ifdef __WXDEBUG__
    /// Called whenever an assertion occurs in wxWidgets.
    virtual void OnAssert(const wxChar *file, int line, const wxChar *cond,
                          const wxChar *msg);
#endif // __WXDEBUG__

#if wxCHECK_VERSION(2,9,0)

    /// In wxWidgets 2.9, we can override this to replace the standard
    /// event loop with our own.
    virtual wxAppTraits *CreateTraits();

#elif wxCHECK_VERSION(2,8,0)

    /// We attempt to replace the standard main loop with one that
    /// calls TInterpreterManager::Run.
    virtual int MainLoop();

#endif // wxCHECK_VERSION...

    /// The cross-platform portion of our main loop.  The return value is
    /// whatever wxWidgets thinks we should return from wxApp::MainLoop, a
    /// subject on which the documentation has been shifty.
    int MainLoopInternal();

    /// Because we've chosen to provide our own main loop, we need to
    /// override the standard "request application shutdown" function.
    virtual void ExitMainLoop();

    /// Remove the stage frame from this application.  This should only
    /// be called by the StageFrame.
    void DetachStageFrame() { mStageFrame = NULL; }
    
    /// Get the StageFrame for this application.
    StageFrame *GetStageFrame() { ASSERT(mStageFrame); return mStageFrame; }
    
    /// Get the Stage for this application.
    Stage *GetStage();

    /// Return true if and only if this application has a stage.
    bool HaveStage() { return mStageFrame != NULL; } 

    /// Make sure that a file exists at the given pathname.  If it does
    /// not, create an empty file there.
    static void EnsureFileExists(const wxString &inFilename);

    /// Do we currently have a user configuration object?  This will
    /// be false until we've loaded a project and set up our paths.
    bool HaveUserConfig();

    /// Get the current user configuration object.
    shared_ptr<wxConfigBase> GetUserConfig();

    /// Write out our user configuration file.
    void SaveUserConfig();

    /// We provide an OnActivateApp handler so we can hide full-screen
    /// windows and fix our screen resolution when we're not in the
    /// foreground.
    void OnActivateApp(wxActivateEvent &event);
};

//////////
/// Declare a 'HalyardApp &wxGetApp()' method.
///
DECLARE_APP(HalyardApp)

#endif // HalyardApp_H
