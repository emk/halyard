// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TSchemeRtCallback_H
#define TSchemeRtCallback_H

#include "TInterpreter.h"
#include "TStateDB.h"

//////////
// We'll use this type to refer to TValueLists without copying them.  We
// need a typedef because we'll be using this type in places where the
// full type name would be unparseable.  Yes, C++ can be silly.
//
typedef const TValueList &ConstTValueListRef;

//////////
// A TSchemeRtCallback is a special kind of callback that can safely be
// called from "real time" code where we can't afford to run the garbage
// collector.
//
// "Real time" does not actually mean "fast"; it means "within a
// predictable, bounded time".  C++ code can finish running within a
// predictable amount of time, but PLT Scheme code cannot.  The solution:
// Write a lightweight Scheme-like interpreter using TValue as the
// fundamental data type.
//
class TSchemeRtCallback : public TCallback {
public:
    class Primitive : public TCallback {
        std::string mName;
    
    protected:
        void CheckArity(int inWantedArgCount, const TValueList &inArgs);

    public:
        Primitive(const std::string &inName) : mName(inName) {}
        std::string GetName() { return mName; }
    };

private:
    class Environment : boost::noncopyable {
        typedef std::map<std::string,TValue> BindingMap;

        Environment *mParent;
        BindingMap mBindings;

    public:
        Environment() : mParent(NULL) {}
        Environment(Environment *inParent) : mParent(inParent) {}

        void CheckFormArity(const std::string &inName,
                            int inWantedArgCount,
                            const TValueList &inForm);

        void AddPrimitive(Primitive *inPrimitive);
        void AddBinding(const std::string &inName, const TValue &inValue);
        TValue GetBinding(const std::string &inName);
        
        TValue Eval(const TValue &inCode);
        TValue EvalList(const TValue &inCode);
        TValue EvalSet(ConstTValueListRef inForm);
        TValue EvalBody(TValueList::const_iterator inBegin,
                        TValueList::const_iterator inEnd);
    };
    
    static bool sIsGlobalEnvInitialized;
    static Environment sGlobalEnv;

    std::string mGetterName;
    Environment mEnv;
    TValueList mCode;

    void EnsureGlobalEnvInitialized();
    TValue Run(TStateListenerPtr inListener);

public:
    TSchemeRtCallback(const std::string &inGetterName,
                      TValue inBindings,
                      TValue inCode);

	virtual TValue Run(const TValueList &inArguments);
};

#endif // TSchemeRtCallback_H
