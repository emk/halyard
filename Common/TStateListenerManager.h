// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef StateListenerManager_H
#define StateListenerManager_H

#include "TStateDB.h"

BEGIN_NAMESPACE_FIVEL

//////////
// A TStateListenerManager keeps track of lots of little free-floating
// TStateListener objects.  These should theoretically be associated with
// C++ Node objects, but we don't have a Node class corresponding to
// the interpreter's <node> class.
//
class TStateListenerManager  {
    class CallbackListener : public TStateListener {
        static int sNextSerialNumber;
        int mSerialNumber;
        std::string mName;
        TCallbackPtr mCallback;

    public:
        CallbackListener(const std::string &inName, TCallbackPtr inCallback);
        int GetSerialNumber() { return mSerialNumber; }
        virtual void NotifyStateChanged();
    };

    typedef boost::shared_ptr<CallbackListener> CallbackListenerPtr;
    typedef std::multimap<std::string,CallbackListenerPtr> ListenerMap;

    ListenerMap mListeners;
	
public:
    //////////
    // Unregister all TStateListeners in the manager.
    //
    void NotifyScriptReload();

    //////////
    // Register inListener under inName.  You may use inName for more
    // than one listener; all listeners will be stored.
    //
    void RegisterListener(const std::string &inName, TCallbackPtr inListener);

    //////////
    // Unregister all listeners matching inName.
    //
    void UnregisterListeners(const std::string &inName);

    //////////
    // Find the listener with the specified name and serial number.
    //
    boost::shared_ptr<TStateListener> FindListener(const std::string &inName,
                                                   int inSerialNumber);
};

extern TStateListenerManager gStateListenerManager;

END_NAMESPACE_FIVEL

#endif // StateListenerManager_H
