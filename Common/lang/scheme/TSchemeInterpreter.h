// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (_TSchemeInterpreter_h_)
#define _TSchemeInterpreter_h_

#include "TInterpreter.h"
#include "TPrimitives.h"
#include "FileSystem.h"
#include "TSchemePtr.h"
#include "TSchemeCallback.h"

BEGIN_NAMESPACE_FIVEL


//////////
// A TInterpreterManager for our Scheme interpreter.  This handles
// reloading scripts and other fun stuff that involves creating
// and destroying interpreters.
//
class TSchemeInterpreterManager : public TInterpreterManager
{
	Scheme_Env *mGlobalEnv;

public:
	TSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc);

	virtual void BeginScript();

private:
	void LoadFile(const FileSystem::Path &inFile);

protected:
	virtual TInterpreter *MakeInterpreter();
};


//////////
// The interface to our Scheme interpreter.  Since this is a singleton
// class, we store a lot of variables as static data to ease implementation.
//
class TSchemeInterpreter : public TInterpreter
{
	friend class TSchemeInterpreterManager;
	friend class TSchemeCallbackArgumentList;
	friend class TSchemeCallback;
	friend class TSchemeArgumentList;

	static Scheme_Env *sGlobalEnv;
	static Scheme_Env *sScriptEnv;
	static TSchemePtr<Scheme_Object> sLoaderModule;
	static TSchemePtr<Scheme_Object> sKernelModule;

    struct BucketKey {
        TSchemePtr<Scheme_Env> env;
        TSchemePtr<Scheme_Object> module;
        std::string name;

        BucketKey(TSchemePtr<Scheme_Env> inEnv,
                  TSchemePtr<Scheme_Object> inModule,
                  std::string inName)
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
    static BucketMap sBucketMap;

	static SystemIdleProc sSystemIdleProc;

	static void InitializeModuleNames();

    static Scheme_Bucket *FindBucket(Scheme_Env *inEnv,
                                     Scheme_Object *inModule,
                                     const char *inFuncName);

	static Scheme_Object *Call5LPrim(int inArgc, Scheme_Object **inArgv);

	static Scheme_Object *CallSchemeEx(Scheme_Env *inEnv,
									   Scheme_Object *inModule,
									   const char *inFuncName,
									   int inArgc, Scheme_Object **inArgv);

	static Scheme_Object *CallScheme(const char *inFuncName,
									 int inArgc, Scheme_Object **inArgv);

	static Scheme_Object *CallSchemeSimple(const char *inFuncName);

	static Scheme_Object *MakeSchemePoint(const TPoint &inPoint);
	static Scheme_Object *MakeSchemeRect(const TRect &inRect);
	static Scheme_Object *MakeSchemeColor(const GraphicsTools::Color &inColor);

public:
	TSchemeInterpreter(Scheme_Env *inGlobalEnv);
	virtual ~TSchemeInterpreter();

	void DoIdle(bool block) { ASSERT(sSystemIdleProc); (*sSystemIdleProc)(block); }

	// For documentation of these virtual methods, see TInterpreter.h.
	virtual void Run(SystemIdleProc inIdleProc);
	virtual void KillInterpreter();
	virtual void Stop();
	virtual bool IsStopped();
	virtual void Go(const char *inCard);
	virtual void Pause(void);
	virtual void WakeUp(void);
	virtual bool Paused(void);
	virtual void Timeout(const char *inName, int32 inTime);
	virtual void Nap(int32 inTime);
	virtual bool Napping(void);
	virtual void KillNap(void);
	virtual void KillCurrentCard(void);
	virtual void JumpToCardByName(const char *inName);
	virtual std::string CurCardName(void);
	virtual std::string PrevCardName(void);
	virtual bool IsValidCard(const char *inCardName);
	virtual bool Eval(const std::string &inExpression,
					  std::string &outResultText);
};

//////////
// A list of Scheme values, for use by the argument-parsing system.  This
// is an abstract class with several subclasses, because mzscheme has several
// different kinds of lists which we might want to process.
//
class TSchemeArgumentList : public TArgumentList
{
	void TypeCheckFail();
	void TypeCheck(Scheme_Type inType, Scheme_Object *inValue);
	void TypeCheckStruct(const char *inPredicate, Scheme_Object *inVal);
	int32 GetInt32Member(const char *inName, Scheme_Object *inVal);

public:
	TSchemeArgumentList() {}

protected:
	//////////
	// Fetch the next argument.
	//
	virtual Scheme_Object *GetNextArg() = 0;
	
	// For documentation of these virtual methods, see TPrimitives.h.
	virtual std::string GetStringArg();
	virtual std::string GetSymbolArg();
	virtual int32 GetInt32Arg();
	virtual uint32 GetUInt32Arg();
	virtual bool GetBoolArg();
	virtual double GetDoubleArg();
	virtual TPoint GetPointArg();
	virtual TRect GetRectArg();
	virtual TPolygon GetPolygonArg();
	virtual GraphicsTools::Color GetColorArg();
	virtual void GetValueOrPercentArg(bool &outIsPercent,
									  int32 &outValue);
	virtual TCallback *GetCallbackArg();
	virtual TArgumentList *GetListArg();
};


//////////
// A list of Scheme_Object values stored as an argc,argv pair.  This is
// the format which mzscheme uses to pass function arguments on the stack.
//
class TSchemeArgvList : public TSchemeArgumentList
{
	//////////
	// The number of arguments in mArgv.
	//
	int mArgc;

	//////////
	// The arguments passed to a primitive.  We don't need to use a
	// TSchemePtr here, because mzscheme protects this object from
	// collection.
	//
	Scheme_Object **mArgv;

	//////////
	// The number of arguments processed so far.
	//
	int mArgsReturned;

public:
	TSchemeArgvList(int inArgc, Scheme_Object **inArgv)
		: mArgc(inArgc), mArgv(inArgv), mArgsReturned(0) {}

	virtual bool HasMoreArguments() { return mArgsReturned < mArgc; }

protected:
	Scheme_Object *GetNextArg()
	{
		if (!HasMoreArguments())
			throw TException(__FILE__, __LINE__, "Not enough arguments");
		return mArgv[mArgsReturned++];
	}
};

END_NAMESPACE_FIVEL

#endif // TSchemeInterpreter
