// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef FiveLApp_H
#define FiveLApp_H

//////////
// Enumerations for menu items, toolbar buttons, and other command-
// generating widgets.
//
enum {
    FIVEL_EXIT = 1,
    FIVEL_RELOAD_SCRIPT = 100,
    FIVEL_ABOUT = 200,

    FIVEL_SHOW_LOG = 300,
    FIVEL_SHOW_FULL_SCREEN,
    FIVEL_SHOW_XY
};

class FiveLApp : public wxApp
{
	//////////
	// Do we have our own, custom event loop?  Or should we use wxWindows'
	// event loop?
	//
    bool mHaveOwnEventLoop;

	//////////
	// This procedure is called periodically by the script interpreter
	// to give time to the GUI.
	//
    static void IdleProc();

public:
    FiveLApp();

	//////////
	// We perform normal application initialization in OnInit. 
	//
    virtual bool OnInit();

	//////////
    // We attempt to replace the standard main loop with one that
	// calls TInterpreterManager::Run.
	//
    virtual int MainLoop();

	//////////
	// Because we've chosen to provide our own main loop, we need to
	// override the standard "request application shutdown" function.
	//
    virtual void ExitMainLoop();
};

//////////
// Declare a 'FiveLApp &wxGetApp()' method.
//
DECLARE_APP(FiveLApp)

#endif // FiveLApp_H
