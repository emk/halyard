// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
#include "TSchemeRtCallback.h"
#include "TStateListenerManager.h"

USING_NAMESPACE_FIVEL

bool TSchemeRtCallback::sIsGlobalEnvInitialized = false;
TSchemeRtCallback::Environment TSchemeRtCallback::sGlobalEnv;


//=========================================================================
//  Primitives
//=========================================================================
//  Our primitive functions are all subclasses of TCallback, allowing us
//  to store them as TValues.

#define BEGIN_REALTIME_PRIMITIVE(CLASSNAME, NAME, ARITY) \
    class CLASSNAME : public TSchemeRtCallback::Primitive { \
    public: \
        CLASSNAME() : TSchemeRtCallback::Primitive(NAME) {} \
        virtual TValue Run(const TValueList &inArgs) { \
            CheckArity((ARITY), inArgs);

#define END_REALTIME_PRIMITIVE() \
        } \
    }

void TSchemeRtCallback::Primitive::CheckArity(int inWantedArgCount,
                                              const TValueList &inArgs)
{
    int arg_count = inArgs.size();
    if (arg_count != inWantedArgCount) {
        std::ostringstream msg;
        msg << GetName() << ": wanted " << inWantedArgCount
            << " arguments, got " << arg_count;
        THROW(msg.str());
    }
}

/// A TSchemeRtCallback which fetches a value from the TStateDB.
class StateDbGet : public TSchemeRtCallback::Primitive {
    TStateListenerPtr mListener;

public:
    StateDbGet(TStateListenerPtr inListener)
        : TSchemeRtCallback::Primitive("state-db"),
          mListener(inListener) {}

    virtual TValue Run(const TValueList &inArgs) {
        CheckArity(1, inArgs);
        return gStateDB.Get(mListener.get(), TSymbol(inArgs[0]).GetName());
    }
};

BEGIN_REALTIME_PRIMITIVE(StateDbSet, "set-state-db!", 2) {
    gStateDB.Set(TSymbol(inArgs[0]).GetName(), inArgs[1]);
    return TNull();
} END_REALTIME_PRIMITIVE();

BEGIN_REALTIME_PRIMITIVE(EvenP, "even?", 1) {
    return (int32(inArgs[0]) % 2) == 0;
} END_REALTIME_PRIMITIVE();


//=========================================================================
//  TSchemeRtCallback::Environment
//=========================================================================
//  Our interpreter is actually implemented as methods on our Environment
//  object, which represents a lexical environment.  Yes, this is probably
//  the first time you've seen a Scheme-like language implemented using
//  the STL.  You are allowed to be slightly horrified.

void TSchemeRtCallback::Environment::CheckFormArity(const std::string &inName,
                                                    int inWantedArgCount,
                                                    const TValueList &inForm)
{
    int arg_count = inForm.size() - 1;
    if (arg_count != inWantedArgCount) {
        std::ostringstream msg;
        msg << inName << ": wanted " << inWantedArgCount
            << " arguments, got " << arg_count;
        THROW(msg.str());
    }
}

void TSchemeRtCallback::Environment::AddPrimitive(Primitive *inPrimitive) {
    AddBinding(inPrimitive->GetName(), TCallbackPtr(inPrimitive));
}

void TSchemeRtCallback::Environment::AddBinding(const std::string &inName,
                                                const TValue &inValue)
{
    if (mBindings.find(inName) != mBindings.end())
        THROW("Tried to declare realtime variable twice: " + inName);
    mBindings.insert(BindingMap::value_type(inName, inValue));
}

TValue TSchemeRtCallback::Environment::GetBinding(const std::string &inName) {
    BindingMap::iterator found = mBindings.find(inName);
    if (found != mBindings.end())
        return found->second;
    else if (mParent)
        return mParent->GetBinding(inName);
    else
        THROW("Undefined realtime variable: " + inName);
}

TValue TSchemeRtCallback::Environment::Eval(const TValue &inCode) {
    switch (inCode.GetType()) {
        case TValue::TYPE_SYMBOL:
            return GetBinding(TSymbol(inCode).GetName());

        case TValue::TYPE_LIST:
            return EvalList(inCode);

        default:
            // Assume everything else self-evaluates.
            return inCode;
    }
}

TValue TSchemeRtCallback::Environment::EvalList(const TValue &inCode) {
    ConstTValueListRef code(inCode);

    // Empty lists self-evaluate to empty lists for compatibility with
    // mzscheme.  I don't know if this is required by R5RS.
    if (code.size() == 0)
        return TValueList();

    // Check for special forms.
    TValue op_expr = code[0];
    if (op_expr.GetType() == TValue::TYPE_SYMBOL) {
        std::string op_name = TSymbol(op_expr).GetName();
        if (op_name == "quote") {
            CheckFormArity("quote", 1, code);
            return code[1];
        } else if (op_name == "set!") {
            return EvalSet(code);
        }
    }

    // Call regular functions.
    TCallbackPtr op(Eval(op_expr).GetCallbackPtr());
    TValueList args;
    args.reserve(code.size() - 1); // For performance.
    TValueList::const_iterator i = code.begin();
    for (++i; i != code.end(); ++i)
        args.push_back(Eval(*i));
    return op->Run(args);
}

TValue TSchemeRtCallback::Environment::EvalSet(ConstTValueListRef inForm) {
    CheckFormArity("set!", 2, inForm);
    TValue place(inForm[1]);
    TValue value_expr(inForm[2]);
    if (place.GetType() != TValue::TYPE_LIST) {
        // TODO - Fix.
        THROW("Regular SET! not yet implemented");
    } else {
        // Expand a generalized setter into a function call.
        //
        // TODO - Should we do this during a macro expansion pass, or is it
        // acceptable to run this code for every generalized 'SET!'?
        ConstTValueListRef place_list(place);
        if (place_list.size() == 0)
            THROW("First argument to SET! may not be ()");
        std::string getter_name = TSymbol(place_list[0]).GetName();
        TValueList new_code;
        new_code.push_back(TSymbol("set-" + getter_name + "!"));
        TValueList::const_iterator i = place_list.begin();
        for (++i; i != place_list.end(); ++i)
            new_code.push_back(*i);
        new_code.push_back(value_expr);
        return Eval(new_code);
    }
}

TValue TSchemeRtCallback::Environment::EvalBody(
    TValueList::const_iterator inBegin,
    TValueList::const_iterator inEnd)
{
    TValue result;
    for (TValueList::const_iterator i = inBegin; i != inEnd; ++i)
        result = Eval(*i);
    return result;
}


//=========================================================================
//  TSchemeRtCallback
//=========================================================================
//  This is essentially a specialized closure class for the realtime
//  interpreter.

TSchemeRtCallback::TSchemeRtCallback(const std::string &inGetterName,
                                     TValue inBindings,
                                     TValue inCode)
    : mGetterName(inGetterName), mEnv(&sGlobalEnv), mCode(inCode)
{
    // Make sure we've set up our global bindings.
    EnsureGlobalEnvInitialized();

    // Copy our bindings into a more reasonable data structure.
    ConstTValueListRef bindings(inBindings);
    TValueList::const_iterator i = bindings.begin();
    for (; i != bindings.end(); ++i) {
        ConstTValueListRef name_and_value(*i);
        if (name_and_value.size() != 2)
            THROW("Malformed binding specification");
        mEnv.AddBinding(name_and_value[0], name_and_value[1]);
    }
}

void TSchemeRtCallback::EnsureGlobalEnvInitialized() {
    if (!sIsGlobalEnvInitialized) {
        sGlobalEnv.AddPrimitive(new StateDbSet);
        sGlobalEnv.AddPrimitive(new EvenP);

        sIsGlobalEnvInitialized = true;
    }
}

TValue TSchemeRtCallback::Run(TStateListenerPtr inListener) {
    Environment env(&mEnv);
    env.AddBinding(mGetterName,
	               TCallbackPtr(new StateDbGet(inListener)));
    return env.EvalBody(mCode.begin(), mCode.end());
}

TValue TSchemeRtCallback::Run(const TValueList &inArguments) {
    if (inArguments.size() != 2)
        THROW("Expected name, serial number as arguments");
    shared_ptr<TStateListener> listener =
        gStateListenerManager.FindListener(TSymbol(inArguments[0]).GetName(),
                                           inArguments[1]);
    return Run(listener);
}
