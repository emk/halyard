// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Quake2Engine_H
#define Quake2Engine_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUAKE2

#include "Widget.h"
#include "wxquake2.h"

class TCallback;

//////////
// Our local "value-added" subclass of wxQuake2Window.  This class
// is a singleton, and once created, it remains permanently attached
// to the Stage object (although we may hide and show it).
//
class Quake2Engine : public wxQuake2Window
{
	typedef std::map<std::string,TCallback*> CallbackMap;

	static bool sHasBeenCreated;
    static Quake2Engine *sInstance;

	//////////
	// All the Scheme callbacks registered by this class.
	//
	CallbackMap mCallbackMap;

	//////////
	// Delete all the callbacks in our callback map.
	//
	void DeleteCallbacks();

    //////////
    // Create a new Quake 2 window on the specified stage.
    //
    Quake2Engine(Stage *inStage, const std::string &inGame);

public:
	~Quake2Engine();

	//////////
	// We need to intercept Show(FALSE) to blit a copy of the onscreen
	// image to our offscreen buffer.
	//
    virtual bool Show(bool show = TRUE);

    //////////
    // Notify the Quake2Engine that the script is being reloaded.
    //
	void NotifyScriptReload();

	//////////
	// Register a Scheme callback as a Quake 2 command.
	//
	// [in] inName - The name of the callback to register.
	// [in] inCallback - The callback.  The callback will be deleted
	//      by this class.
	//
	void RegisterCallback(const std::string &inName, TCallback *inCallback);
	
	//////////
	// Return true if and only if our singleton has been initialized.
	//
	static bool IsInitialized() { return sHasBeenCreated; }

	//////////
	// Initialize the engine, and attach a wxQuake2Window to the stage.
	//
	static void Initialize(const std::string &inGame);

	//////////
	// Get the single instance of the engine object.
	//
	static Quake2Engine *GetInstance() { ASSERT(sInstance); return sInstance; }

protected:
	virtual void HandleCommand();
};

#endif // CONFIG_HAVE_QUAKE2
#endif // Quake2Engine_H
