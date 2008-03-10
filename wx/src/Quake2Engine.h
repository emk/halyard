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

#ifndef Quake2Engine_H
#define Quake2Engine_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUAKE2

#include "Widget.h"
#include "wxquake2.h"

class TCallback;

//////////
/// Our local "value-added" subclass of wxQuake2Window.  This class
/// is a singleton, and once created, it remains permanently attached
/// to the Stage object (although we may hide and show it).
///
class Quake2Engine : public wxQuake2Window, public Halyard::TReloadNotified
{
	typedef std::map<std::string,Halyard::TCallbackPtr> CallbackMap;

	static bool sHasBeenCreated;
    static Quake2Engine *sInstance;

	//////////
	/// All the Scheme callbacks registered by this class.
	///
	CallbackMap mCallbackMap;

	//////////
	/// Delete all the callbacks in our callback map.
	///
	void DeleteCallbacks();

    //////////
    /// Create a new Quake 2 window on the specified stage.
    ///
    Quake2Engine(Stage *inStage, const std::string &inGame,
                 const std::string &inDriver);

    DECLARE_EVENT_TABLE();

	void OnMouseEvent(wxMouseEvent &inEvent);

public:
	~Quake2Engine();

	//////////
	/// We need to intercept Show(FALSE) to blit a copy of the onscreen
	/// image to our offscreen buffer.
	///
    virtual bool Show(bool show = TRUE);

    //////////
    /// Notify the Quake2Engine that the script is being reloaded.
    ///
	void NotifyReloadScriptStarting();

	//////////
	/// Register a Scheme callback as a Quake 2 command.
	///
	/// \param inName  The name of the callback to register.
	/// \param inCallback  The callback.  The callback will be deleted
	///      by this class.
	///
	void RegisterCallback(const std::string &inName, Halyard::TCallbackPtr inCallback);
	
	//////////
	/// Return true if and only if our singleton has been initialized.
	///
	static bool IsInitialized() { return sHasBeenCreated; }

	//////////
	/// Initialize the engine, and attach a wxQuake2Window to the stage.
	///
	static void Initialize(const std::string &inGame,
                           const std::string &inDriver);

    //////////
    /// Return true if an instance has been created.
    ///
    static bool HaveInstance() { return sInstance != NULL; }

	//////////
	/// Get the single instance of the engine object.
	///
	static Quake2Engine *GetInstance() { ASSERT(sInstance); return sInstance; }

    //////////
    /// Are we displaying the Quake 2 engine right now?
    ///
    static bool IsDisplayed() {
        return HaveInstance() && GetInstance()->IsShown();
    }

protected:
	virtual void HandleCommand();
    virtual void HandleBinMsg(unsigned char *buffer, size_t size);
};

#endif // CONFIG_HAVE_QUAKE2
#endif // Quake2Engine_H
