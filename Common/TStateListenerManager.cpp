// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TStateListenerManager.h"
#include "TInterpreter.h"

USING_NAMESPACE_FIVEL

TStateListenerManager gStateListenerManager;

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

void TStateListenerManager::NotifyScriptReload() {
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

boost::shared_ptr<TStateListener>
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


