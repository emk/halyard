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

#include "HalyardApp.h"
#include "Stage.h"
#include "Quake2Engine.h"
#include "FileSystem.h"
#include "TPrimitives.h"
#include "DrawingArea.h"
#include "TStateDB.h"
#include "BinMsgConv.h"

using namespace Halyard;
using FileSystem::GetBaseDirectory;

#if CONFIG_HAVE_QUAKE2

BEGIN_EVENT_TABLE(Quake2Engine, wxQuake2Window)
    EVT_MOTION(Quake2Engine::OnMouseEvent)
	EVT_LEFT_DOWN(Quake2Engine::OnMouseEvent)
	EVT_LEFT_DCLICK(Quake2Engine::OnMouseEvent)
	EVT_LEFT_UP(Quake2Engine::OnMouseEvent)
END_EVENT_TABLE()

// TODO - Do we want to allow the Quake 2 to refresh the screen while
// another window is displayed over it?
// TODO - Definitely broken in the presence of Reload Script.

Quake2Engine::Quake2Engine(Stage *inStage, const std::string &inGame,
                           const std::string &inDriver)
    : wxQuake2Window(inStage, -1, wxDefaultPosition,
					 wxQuake2Window::MODE_800_600,
					 GetBaseDirectory().ToNativePathString().c_str(),
					 inGame.c_str(), inDriver.c_str())
{
	ASSERT(!sHasBeenCreated && !sInstance);
	sInstance = this;
	sHasBeenCreated = true;

	// XXX - Forcibly hide Quake 2, even if it was previously shown
	// elsewhere.  This is an experimental hack to see if I can
	// get background level-loading to work.
	Hide();

	// TODO - Do something reasonable about stages which aren't 800x600.
}

Quake2Engine::~Quake2Engine()
{
	DeleteCallbacks();
	sInstance = NULL;
}

void Quake2Engine::OnMouseEvent(wxMouseEvent &inEvent)
{
	// We redirect mouse events to the underlying stage.  We only receive
	// these events if mouse interaction is enabled.
	GetParent()->GetEventHandler()->ProcessEvent(inEvent);
}

bool Quake2Engine::Show(bool show)
{
	if (!show && IsShown() && wxGetApp().HaveStage())
	{
		// Attempt to save our displayed graphics back into our Stage's
		// offscreen buffer.
		wxClientDC client_dc(this);
		wxGetApp().GetStage()->GetBackgroundDrawingArea()->DrawDCContents(client_dc);
	}

	// Pass the request along to our parent.
	return wxQuake2Window::Show(show);
}


void Quake2Engine::NotifyReloadScriptStarting()
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
	mCallbackMap.clear();
}

void Quake2Engine::RegisterCallback(const std::string &inName,
									TCallbackPtr inCallback)
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
		mCallbackMap.erase(found);
	}

	// Add the new command to our map.
	mCallbackMap.insert(std::pair<std::string,TCallbackPtr>(inName, inCallback));
}

void Quake2Engine::HandleCommand()
{
	wxString command = CommandArgv(0);
	CallbackMap::iterator found = mCallbackMap.find(command.mb_str());
	if (found == mCallbackMap.end())
		wxQuake2Window::HandleCommand();
	else
	{
		TCallbackPtr callback = found->second;

		TValueList args;
		int argc = CommandArgc();
		for (int i = 1; i < argc; i++)
			args.push_back(CommandArgv(i).mb_str());
		callback->Run(args);
	}
}

void Quake2Engine::HandleBinMsg(unsigned char *buffer, size_t size)
{
    BEGIN_EXCEPTION_TRAPPER() {
        BinMsg msg(buffer, size);
        TValueList args(msg.GetArgs());
        if (msg.GetName() == "state") {
            if (args.size() != 2)
                THROW("Malformed 'state' message");
            gStateDB.Set("/quake2/" + std::string(args[0]), args[1]);
        }    
    } END_EXCEPTION_TRAPPER(TException::ReportException);
}

void Quake2Engine::Initialize(const std::string &inGame,
                              const std::string &inDriver)
{
	new Quake2Engine(wxGetApp().GetStage(), inGame, inDriver);
}

#endif CONFIG_HAVE_QUAKE2
