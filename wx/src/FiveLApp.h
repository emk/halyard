// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef FiveLApp_H
#define FiveLApp_H

class StageFrame;
class Stage;

class FiveLApp : public wxApp
{
	//////////
	// Do we have our own, custom event loop?  Or should we use wxWindows'
	// event loop?
	//
    bool mHaveOwnEventLoop;

	//////////
	// Our StageFrame.
	//
	StageFrame *mStageFrame;

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
	// We perform normal application cleanup in OnExit.
	//
    virtual int OnExit();

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

	//////////
	// Remove the stage frame from this application.  This should only
	// be called by the StageFrame.
	//
	void DetachStageFrame() { mStageFrame = NULL; }
	
	//////////
	// Get the StageFrame for this application.
	//
	StageFrame *GetStageFrame() { ASSERT(mStageFrame); return mStageFrame; }
	
	//////////
	// Get the Stage for this application.
	//
	Stage *GetStage();
};

//////////
// Declare a 'FiveLApp &wxGetApp()' method.
//
DECLARE_APP(FiveLApp)

#endif // FiveLApp_H
