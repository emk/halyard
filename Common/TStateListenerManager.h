// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#ifndef StateListenerManager_H
#define StateListenerManager_H

#include "TStateDB.h"
#include "TInterpreter.h"

BEGIN_NAMESPACE_FIVEL

//////////
/// A TStateListenerManager keeps track of lots of little free-floating
/// TStateListener objects.  These should theoretically be associated with
/// C++ Node objects, but we don't have a Node class corresponding to
/// the interpreter's <node> class.
///
class TStateListenerManager : public TReloadNotified  {
    class CallbackListener : public TStateListener {
        static uint32 sNextSerialNumber;
        uint32 mSerialNumber;
        std::string mName;
        TCallbackPtr mCallback;

    public:
        CallbackListener(const std::string &inName, TCallbackPtr inCallback);
        uint32 GetSerialNumber() { return mSerialNumber; }
        virtual void NotifyStateChanged();
    };

    typedef shared_ptr<CallbackListener> CallbackListenerPtr;
    typedef std::multimap<std::string,CallbackListenerPtr> ListenerMap;

    ListenerMap mListeners;
	
public:
    //////////
    /// Unregister all TStateListeners in the manager.
    ///
    void NotifyReloadScriptStarting();

    //////////
    /// Register inListener under inName.  You may use inName for more
    /// than one listener; all listeners will be stored.
    ///
    void RegisterListener(const std::string &inName, TCallbackPtr inListener);

    //////////
    /// Unregister all listeners matching inName.
    ///
    void UnregisterListeners(const std::string &inName);

    //////////
    /// Find the listener with the specified name and serial number.
    ///
    shared_ptr<TStateListener> FindListener(const std::string &inName,
                                            uint32 inSerialNumber);
};

extern TStateListenerManager gStateListenerManager;

END_NAMESPACE_FIVEL

#endif // StateListenerManager_H
