// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeRtCallback.h"
#include "TStateDB.h"
#include "TStateListenerManager.h"

USING_NAMESPACE_FIVEL

void TSchemeRtCallback::Run(TStateListener *inListener) {
    // TODO - Implement simple eval.
    int32 seconds = gStateDB.Get(inListener, "/system/clock/seconds");
    gStateDB.Set("/blinker/on?", (seconds % 2) == 0);
}

TSchemeRtCallback::TSchemeRtCallback(const std::string &inGetterName,
                                     TValue inBindings,
                                     TValue inCode)
    : mGetterName(inGetterName), mCode(inCode)
{
    // Copy our bindings into a more reasonable data structure.
	typedef const TValueList &ConstTValueListRef;
    ConstTValueListRef bindings(inBindings);
    TValueList::const_iterator i = bindings.begin();
    for (; i != bindings.end(); ++i) {
        ConstTValueListRef name_and_value(*i);
        if (name_and_value.size() != 2)
            THROW("Malformed binding specification");
        std::string name(name_and_value[0]);
        TValue value(name_and_value[1]);
        if (mBindings.find(name) != mBindings.end())
            THROW("Duplicate binding: " + name);
        mBindings.insert(BindingMap::value_type(name, value));
    }
}

void TSchemeRtCallback::Run(const TValueList &inArguments) {
    if (inArguments.size() != 2)
        THROW("Expected name, serial number as arguments");
    boost::shared_ptr<TStateListener> listener =
        gStateListenerManager.FindListener(TSymbol(inArguments[0]).GetName(),
                                           inArguments[1]);
    Run(listener.get());
}

