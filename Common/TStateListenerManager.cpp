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

#include "CommonHeaders.h"
#include "TStateListenerManager.h"
#include "TInterpreter.h"

using namespace Halyard;

TStateListenerManager Halyard::gStateListenerManager;

uint32 TStateListenerManager::CallbackListener::sNextSerialNumber = 0;

TStateListenerManager::CallbackListener::CallbackListener(
    const std::string &inName,
    TCallbackPtr inCallback)
    : mSerialNumber(sNextSerialNumber++), mName(inName), mCallback(inCallback)
{
}

void TStateListenerManager::CallbackListener::NotifyStateChanged() {
    TValueList args;
    args.push_back(TSymbol(mName));
    args.push_back(mSerialNumber);
    mCallback->Run(args);
}

void TStateListenerManager::NotifyReloadScriptStarting() {
    mListeners.clear();
}

void TStateListenerManager::NotifyInterpreterStopped() {
    mListeners.clear();
}

void TStateListenerManager::RegisterListener(const std::string &inName,
                                             TCallbackPtr inListener)
{
    CallbackListenerPtr ptr(new CallbackListener(inName, inListener));
    mListeners.insert(ListenerMap::value_type(inName, ptr));
    ptr->NotifyStateChanged();
}

void TStateListenerManager::UnregisterListeners(const std::string &inName) {
    mListeners.erase(inName);
}

shared_ptr<TStateListener>
TStateListenerManager::FindListener(const std::string &inName,
                                    uint32 inSerialNumber)
{
    std::pair<ListenerMap::iterator,ListenerMap::iterator> found =
        mListeners.equal_range(inName);
    for (ListenerMap::iterator i = found.first; i != found.second; ++i)
        if (i->second->GetSerialNumber() == inSerialNumber)
            return i->second;

    // The user shouldn't normally be able to trigger this error unless
    // they do something really sneaky.
    THROW("Invalid name/serial # pair for: " + inName);
}


