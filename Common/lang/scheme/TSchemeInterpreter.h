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

#if !defined (_TSchemeInterpreter_h_)
#define _TSchemeInterpreter_h_

#include "TInterpreter.h"
#include "TPrimitives.h"
#include "FileSystem.h"
#include "TSchemePtr.h"
#include "TSchemeArgs.h"
#include "TSchemeReg.h"
#include "TSchemeCallback.h"
#include "TSchemeConv.h"

BEGIN_NAMESPACE_HALYARD

class TPercent;


//////////
/// A TInterpreterManager for our Scheme interpreter.  This handles
/// reloading scripts and other fun stuff that involves creating
/// and destroying interpreters.
///
class TSchemeInterpreterManager : public TInterpreterManager
{
    bool mHaveInitializedScheme;
	TSchemePtr<Scheme_Env> mGlobalEnv;
    shared_ptr<ScriptEditorDB> mScriptEditorDB;

public:
	TSchemeInterpreterManager(SystemIdleProc inIdleProc);

    virtual void InitialSetup();
    virtual ScriptEditorDB *GetScriptEditorDBInternal();

private:
    void InitializeScheme();
    static Scheme_Object *GetRuntimeDirectory(int inArgc,
                                              Scheme_Object **inArgv);
    static Scheme_Object *SetCollectsPath(int inArgc, Scheme_Object **inArgv);
	void LoadFile(const FileSystem::Path &inFile);

protected:
	virtual void MakeInterpreter();
};


//////////
/// The interface to our Scheme interpreter.  Since this is a singleton
/// class, we store a lot of variables as static data to ease implementation.
///
class TSchemeInterpreter : public TInterpreter
{
	friend class TSchemeInterpreterManager;
	friend class TSchemeCallback;

	TSchemePtr<Scheme_Env> mGlobalEnv;
	TSchemePtr<Scheme_Env> mScriptEnv;
	TSchemePtr<Scheme_Object> mLoaderModule;
	TSchemePtr<Scheme_Object> mKernelModule;

    typedef Scheme_Env *EnvPtr;

    // Compare two Scheme pointers for equality in a GC-safe fashion.
    //
    // MANUAL GC PROOF REQUIRED - We don't need to use a TSchemeReg because
    // we pass our arguments straight through.  We also assume that it's
    // safe to cast pointers of type Scheme_Env, etc, to Scheme_Object when
    // performing this comparison.
    template <typename T>
    bool Eq(T *left, T *right) {
        int result = scheme_eq(reinterpret_cast<Scheme_Object*>(left),
                               reinterpret_cast<Scheme_Object*>(right));
        // Manually convert to bool to avoid compiler warning.
        return result ? true : false;
    }

    struct BucketKey {
        // We use these enumerations to represent all possible environments
        // and modules, because we can't rely on stable pointers to the
        // underlying Scheme objects across a garbage collection.  Of
        // course, this means that we _must_ discard all BucketKey objects
        // when shutting down a TInterpreter, because it invalidates
        // anything found in SCRIPT_ENV.
        enum Env { GLOBAL_ENV, SCRIPT_ENV };
        enum Module { LOADER_MODULE, KERNEL_MODULE };

        Env env;
        Module module;
        std::string name;

        BucketKey(Env inEnv, Module inModule, std::string inName)
            : env(inEnv), module(inModule), name(inName) {}
        
        bool operator<(const BucketKey &inRight) const {
            return ((env < inRight.env)
                    || (env == inRight.env
                        && (module < inRight.module ||
                            (module == inRight.module
                             && name < inRight.name))));
        }
    };
    typedef std::map<BucketKey, TSchemePtr<Scheme_Bucket> > BucketMap;
    BucketMap mBucketMap;

    enum Thread {
        INITIAL_THREAD,
        SWITCHING_TO_SANDBOX_THREAD,
        SANDBOX_THREAD
    };
    Thread mCurrentThread;

    bool mScriptIsLoaded;

	void InitializeModuleNames();

    Scheme_Bucket *FindBucket(Scheme_Env *inEnv,
                              Scheme_Object *inModule,
                              const char *inFuncName);

    /// Call a primitive function.  This function may only use PLT-style
    /// error handling, *not* C++ exceptions.
	static Scheme_Object *CallPrim(int inArgc, Scheme_Object **inArgv);
    /// This function may only use C++ exceptions, *not* PLT-style error
    /// handling.
    bool CallPrimInternal(const char *inPrimName,
                          int inArgc, Scheme_Object **inArgv,
                          Scheme_Object **outResult,
                          char *outErrorMessage,
                          size_t inErrorMessageMaxLength);

    /// Call load-script in stage1.ss.
    Scheme_Object *LoadScript();

public:
	TSchemeInterpreter(Scheme_Env *inGlobalEnv);
	virtual ~TSchemeInterpreter();

    static TSchemeInterpreter *GetSchemeInterpreter();

	Scheme_Object *CallSchemeEx(Scheme_Env *inEnv,
                                Scheme_Object *inModule,
                                const char *inFuncName,
                                int inArgc, Scheme_Object **inArgv);
    Scheme_Object *CallSchemeExHelper(Scheme_Object *inFunc,
                                      int inArgc,
                                      Scheme_Object **inArgv);
	Scheme_Object *CallScheme(const char *inFuncName,
									 int inArgc, Scheme_Object **inArgv);
	Scheme_Object *CallSchemeSimple(const char *inFuncName);

    /// A static version of CallScheme for use by other classes.  This cuts
    /// down on the number of times we need to look up our singleton object
    /// elsewhere in the codebase.
	static Scheme_Object *CallSchemeStatic(const char *inFuncName,
                                           int inArgc, Scheme_Object **inArgv);

	// For documentation of these virtual methods, see TInterpreter.h.
	virtual void Run();
	virtual void KillInterpreter();
	virtual void Stop();
	virtual bool IsStopped();
	virtual void Go(const char *inCard);
    virtual bool CanSuspend();
	virtual void Pause(void);
	virtual void WakeUp(void);
	virtual bool Paused(void);
	virtual void KillCurrentCard(void);
	virtual void JumpToCardByName(const char *inName);
	virtual void LoadGroup(const char *inName);
	virtual std::string CurCardName(void);
	virtual bool IsValidCard(const char *inCardName);
	virtual bool Eval(const std::string &inExpression,
					  std::string &outResultText);
	virtual bool MaybeHandleCaution(const std::string &inMessage);
    virtual IdentifierList GetBuiltInIdentifiers();
    virtual TScriptIdentifier::Type IdentifierType(const std::string &type_str);
};

END_NAMESPACE_HALYARD

#endif // TSchemeInterpreter
