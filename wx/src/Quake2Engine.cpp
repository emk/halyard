// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include "TCommon.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "Quake2Engine.h"
#include "FileSystem.h"

using FileSystem::GetBaseDirectory;

#if CONFIG_HAVE_QUAKE2

// TODO - Do we want to allow the Quake 2 to refresh the screen while
// another window is displayed over it?
// TODO - Definitely broken in the presence of Reload Script.

Quake2Engine::Quake2Engine(Stage *inStage)
    : wxQuake2Window(inStage, -1, wxDefaultPosition,
					 wxQuake2Window::MODE_640_480,
					 GetBaseDirectory().ToNativePathString().c_str(),
					 "tamale")
{
	ASSERT(!sHasBeenCreated && !sInstance);
	sInstance = this;
	sHasBeenCreated = true;

	// TODO - Do something reasonable about stages which aren't 640x480.
}

Quake2Engine::~Quake2Engine()
{
	DeleteCallbacks();
	sInstance = NULL;
}

void Quake2Engine::NotifyScriptReload()
{
	// Hide the Quake 2 window, if necessary.  If the reloaded script wants
	// Quake 2 to be visible, it will show it.
	if (IsShown())
		Hide();

	// Delete all registered callbacks.
	DeleteCallbacks();
}

void Quake2Engine::DeleteCallbacks()
{
	CallbackMap::iterator i = mCallbackMap.begin();
	for (; i != mCallbackMap.end(); ++i)
		delete i->second;
	mCallbackMap.clear();
}

void Quake2Engine::RegisterCallback(const std::string &inName,
									TCallback *inCallback)
{
	// Check to see if this command is currently in our map.
	CallbackMap::iterator found = mCallbackMap.find(inName);
	if (found == mCallbackMap.end())
	{
		// We don't currently have a command with this name,
		// so assume we probably have to register it with Quake 2.
		RegisterCommand(inName.c_str());
	}
	else
	{
		// Delete an existing command with this name.
		delete found->second;
		mCallbackMap.erase(found);
	}

	// Add the new command to our map.
	mCallbackMap.insert(std::pair<std::string,TCallback*>(inName, inCallback));
}

void Quake2Engine::HandleCommand()
{
	wxString command = CommandArgv(0);
	CallbackMap::iterator found = mCallbackMap.find(command.mb_str());
	if (found == mCallbackMap.end())
		wxQuake2Window::HandleCommand();
	else
		// FIXME - Add support for command arguments.
		found->second->Run();
}

void Quake2Engine::Initialize()
{
	new Quake2Engine(wxGetApp().GetStage());
}

#endif CONFIG_HAVE_QUAKE2
