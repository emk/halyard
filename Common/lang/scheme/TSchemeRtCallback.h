// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TSchemeRtCallback_H
#define TSchemeRtCallback_H

#include "TInterpreter.h"

class TStateListener;

class TSchemeRtCallback : public TCallback {
    typedef std::map<std::string,TValue> BindingMap;

    std::string mGetterName;
    BindingMap mBindings;
    TValueList mCode;

    void Run(TStateListener *inListener);

public:
    TSchemeRtCallback(const std::string &inGetterName,
                      TValue inBindings,
                      TValue inCode);

	virtual void Run(const TValueList &inArguments);
};

#endif // TSchemeRtCallback_H
