// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TSchemeInterpreter.h"
#include "TException.h"

USING_NAMESPACE_FIVEL

static const char *CALL_5L_PRIM = "%call-5l-prim";

#define UNIMPLEMENTED \
	throw TException(__FILE__, __LINE__, "Scheme: Not yet implemented")


//=========================================================================
//	Scheme Primitives
//=========================================================================

DEFINE_5L_PRIMITIVE(SchemeExit)
{
	// Ask the interpreter manager to shut us down.
	TInterpreterManager::GetInstance()->RequestQuitApplication();
}

DEFINE_5L_PRIMITIVE(SchemeIdle)
{
	// Recover our Scheme interpreter.
	TSchemeInterpreter *scheme_interp =
		dynamic_cast<TSchemeInterpreter*>(TSchemeInterpreter::GetInstance());
	ASSERT(scheme_interp);

	// Call our stored idle procedure.
	scheme_interp->DoIdle();
}


//=========================================================================
//	TSchemeInterpreterManager Methods
//=========================================================================

TSchemeInterpreterManager::TSchemeInterpreterManager(
	TInterpreter::SystemIdleProc inIdleProc)
	: TInterpreterManager(inIdleProc)
{
	// Install our primitives.
	REGISTER_5L_PRIMITIVE(SchemeExit);
	REGISTER_5L_PRIMITIVE(SchemeIdle);

	// Initialize the Scheme interpreter.
	mGlobalEnv = scheme_basic_env();

	// Make a module to hold functions exported by the engine.
	Scheme_Object *modname = scheme_intern_symbol("#%fivel-engine");
	Scheme_Env *engine_mod = scheme_primitive_module(modname, mGlobalEnv);

	// Provide a way for Scheme code to call 5L primitives.
	Scheme_Object *call_5l_prim =
		scheme_make_prim_w_arity(&TSchemeInterpreter::Call5LPrim,
								 CALL_5L_PRIM, 1, -1);
	scheme_add_global(CALL_5L_PRIM, call_5l_prim, engine_mod);

	// Finish creating our engine module.
	scheme_finish_primitive_module(engine_mod);

	// Install our system loader.
	FileSystem::Path fivel_collection =
		FileSystem::GetRuntimeDirectory().AddComponent("5L");
	LoadFile(fivel_collection.AddComponent("5L-Loader.ss"));
}

void TSchemeInterpreterManager::LoadFile(const FileSystem::Path &inFile)
{
	// Make sure the file exists.
	std::string name = inFile.ToNativePathString();
	if (!inFile.DoesExist())
		gLog.FatalError("Cannot open required support file <%s>.",
						name.c_str());

	// Load the file.
	if (!scheme_load(name.c_str()))
	{
		// An error occurred in the code below, so give an error message.
		// Note that this error message isn't very helpful, so we shouldn't
		// use this routine to load ordinary user code--just the kernel
		// and its support files.
		std::string error_msg = "Error loading file <" + name + ">";
		throw TException(__FILE__, __LINE__, error_msg.c_str());
	}
}

TInterpreter *TSchemeInterpreterManager::MakeInterpreter()
{
	return new TSchemeInterpreter(mGlobalEnv);
}


//=========================================================================
//	TSchemeInterpreter Methods
//=========================================================================

Scheme_Env *TSchemeInterpreter::sGlobalEnv = NULL;
Scheme_Env *TSchemeInterpreter::sScriptEnv = NULL;
TInterpreter::SystemIdleProc TSchemeInterpreter::sSystemIdleProc = NULL;

TSchemeInterpreter::TSchemeInterpreter(Scheme_Env *inGlobalEnv)
{
	// Declare a dummy argument list for calling Scheme functions.
	Scheme_Object *args;

	// Remember our global environment.
	sGlobalEnv = inGlobalEnv;

	// Create a new script environment, and store it where we can find it.
	sScriptEnv = NULL;
	CallSchemeEx(sGlobalEnv, "5L-Loader", "new-script-environment", 0, &args);
	sScriptEnv = scheme_get_env(scheme_config);

	// Load our kernel and script.
	Scheme_Object *result = 
		CallSchemeEx(sGlobalEnv, "5L-Loader", "load-script", 0, &args);
	if (!SCHEME_FALSEP(result))
	{
		ASSERT(SCHEME_STRINGP(result));
		throw TException(__FILE__, __LINE__, SCHEME_STR_VAL(result));
	}
}

TSchemeInterpreter::~TSchemeInterpreter()
{
	// We don't actually shut down the Scheme interpreter.  But we'll
	// reinitialize it later if we need to.
}

Scheme_Object *TSchemeInterpreter::Call5LPrim(int inArgc,
											  Scheme_Object **inArgv)
{
	ASSERT(sScriptEnv != NULL);

	// The interpreter checks the arity for us, but we need to check the
	// argument types.  The various error-reporting functions call
	// scheme_longjmp, to implement non-local exits in C.  We need to be
	// careful about exceptions and scheme_longjmp; there's no guarantee
	// that they play nicely together.
	ASSERT(inArgc >= 1);
	if (!SCHEME_SYMBOLP(inArgv[0]))
		scheme_wrong_type(CALL_5L_PRIM, "symbol", 0, inArgc, inArgv);
	const char *prim_name = SCHEME_SYM_VAL(inArgv[0]);

	if (!gPrimitiveManager.DoesPrimitiveExist(prim_name))
		scheme_signal_error("Unknown 5L primitive: %V", inArgv[0]);

	// We need these to get information back out of our try block.
	bool have_result = false, have_error = false;
	std::string result, error, errormsg;

	// WARNING - Don't signal any Scheme errors from inside this try block.
	// (I don't know whether it's portable to call scheme_longjmp from
	// inside a try block.)  It's OK to signal Scheme errors in our catch
	// blocks, AFAIK.
	try
	{
		// Clear our error-handling variables.
		gVariableManager.SetString("_error", "");
		gVariableManager.SetString("_errormsg", "");

		// Marshal our argument list and call the primitive.
		TSchemeArgvList arg_list(inArgc - 1, inArgv + 1);
		gPrimitiveManager.CallPrimitive(prim_name, arg_list);

		// Make sure all our arguments were used.
		if (arg_list.HasMoreArguments())
			throw TException(__FILE__, __LINE__, "Too many arguments");
		
		// Get our various result variables here (inside the try block),
		// and save them until we're allowed to raise Scheme errors
		// (outside the try block).
		if (!gVariableManager.IsNull("_result"))
		{
			have_result = true;
			result = gVariableManager.GetString("_result");
		}
		else if (!gVariableManager.IsNull("_error"))
		{
			have_error = true;
			error = gVariableManager.GetString("_error");
			errormsg = gVariableManager.GetString("_errormsg");
		}
	}
	catch (std::exception &e)
	{
		scheme_signal_error("%s: 5L engine error: %s", prim_name, e.what());
	}
	catch (...)
	{
		scheme_signal_error("%s: Unknown 5L engine error", prim_name);
	}

	// Figure out what we should pass back to Scheme.
	if (have_error)
		scheme_signal_error("%s: %s: %s", prim_name,
							error.c_str(), errormsg.c_str());
	else if (have_result)
		// TODO - Support more data types as soon as TVariableManager does.
		return scheme_make_sized_string(const_cast<char*>(result.c_str()),
										result.length(), true);

	return scheme_false;
}

Scheme_Object *TSchemeInterpreter::CallSchemeEx(Scheme_Env *inEnv,
												const char *inModuleName,
												const char *inFuncName,
												int inArgc,
												Scheme_Object **inArgv)
{
	Scheme_Object *result = scheme_false;

	// Save our jump buffer.
	mz_jmp_buf save;
	memcpy(&save, &scheme_error_buf, sizeof(mz_jmp_buf));
	
	// Install a Scheme exception handler.  See the MzScheme internals
	// manual for more information on how this works; it appears to be
	// built around the arcane setjmp and longjmp functions in C.
	if (scheme_setjmp(scheme_error_buf))
	{
		// Scheme tried to throw an exception out of inFuncName, which
		// is forbidden.  So we want to die with a fatal error.  (Note
		// that we can't get the error message from C; this exception
		// should have been caught and handled by Scheme.)
		gLog.FatalError("Scheme kernel threw unexpected exception");
	}
	else
	{
		// Call the function.  Note that scheme_module_bucket will look
		// up names in the module's *internal* namespace, not its official
		// export namespace.
		// TODO - Is this a performance bottleneck?
		Scheme_Object *mod = scheme_intern_symbol(inModuleName);
		Scheme_Object *sym = scheme_intern_symbol(inFuncName);
		Scheme_Bucket *bucket = scheme_module_bucket(mod, sym, -1, inEnv);
		ASSERT(bucket != NULL);
		Scheme_Object *f = static_cast<Scheme_Object*>(bucket->val);
		if (f)
			result = scheme_apply(f, inArgc, inArgv);
		else
			gLog.FatalError("Can't find %s in %s", inFuncName, inModuleName);
	}

	// Restore our jump buffer and exit.
	memcpy(&scheme_error_buf, &save, sizeof(mz_jmp_buf));
	return result;
}

Scheme_Object *TSchemeInterpreter::CallScheme(const char *inFuncName,
											  int inArgc,
											  Scheme_Object **inArgv)
{
	// Under normal circumstances, we only want to call functions defined
	// in the kernel, which is running in the script's namespace.
	return CallSchemeEx(sScriptEnv, "5L-Kernel", inFuncName,
						inArgc, inArgv);
}

Scheme_Object *TSchemeInterpreter::CallSchemeSimple(const char *inFuncName)
{
	// Call a function with no arguments.
	Scheme_Object *junk = scheme_false;
	return CallScheme(inFuncName, 0, &junk);
}

void TSchemeInterpreter::Run(SystemIdleProc inIdleProc)
{
	sSystemIdleProc = inIdleProc;
	(void) CallSchemeSimple("%kernel-run");
}

void TSchemeInterpreter::KillInterpreter(void)
{
	(void) CallSchemeSimple("%kernel-kill-interpreter");
}

void TSchemeInterpreter::Pause(void)
{
    (void) CallSchemeSimple("%kernel-pause");
}

void TSchemeInterpreter::WakeUp(void)
{
    (void) CallSchemeSimple("%kernel-wake-up");
}

bool TSchemeInterpreter::Paused(void)
{
	Scheme_Object *o = CallSchemeSimple("%kernel-paused?");
	return SCHEME_FALSEP(o) ? false : true;
}

void TSchemeInterpreter::Timeout(const char *inName, int32 inTime)
{
	Scheme_Object *args[2];
	args[0] = scheme_make_string(inName);
	args[1] = scheme_make_integer_value(inTime);
	(void) CallScheme("%kernel-timeout", 2, args);
}

void TSchemeInterpreter::Nap(int32 inTime)
{
	Scheme_Object *args[1];
	args[0] = scheme_make_integer_value(inTime);
	(void) CallScheme("%kernel-nap", 1, args);
}

bool TSchemeInterpreter::Napping(void)
{
	Scheme_Object *o = CallSchemeSimple("%kernel-napping?");
	return SCHEME_FALSEP(o) ? false : true;
}

void TSchemeInterpreter::KillNap(void)
{
    (void) CallSchemeSimple("%kernel-kill-nap");
}

void TSchemeInterpreter::KillCurrentCard(void)
{
    (void) CallSchemeSimple("%kernel-kill-current-card");
}

void TSchemeInterpreter::JumpToCardByName(const char *inName)
{
	Scheme_Object *args[1];
	args[0] = scheme_make_string(inName);
	(void) CallScheme("%kernel-jump-to-card-by-name", 1, args);
}

std::string TSchemeInterpreter::CurCardName(void)
{
	Scheme_Object *o = CallSchemeSimple("%kernel-current-card-name");
	if (!SCHEME_STRINGP(o))
		gLog.FatalError("Current card name must be string");
	return SCHEME_STR_VAL(o);
}

std::string TSchemeInterpreter::PrevCardName(void)
{
	Scheme_Object *o = CallSchemeSimple("%kernel-previous-card-name");
	if (!SCHEME_STRINGP(o))
		gLog.FatalError("Previous card name must be string");
	return SCHEME_STR_VAL(o);
}


//=========================================================================
//	TSchemeCallback Methods
//=========================================================================

void TSchemeCallback::Run()
{
	Scheme_Object *args[1];
	args[0] = mCallback;	
	TSchemeInterpreter::CallScheme("%kernel-run-callback", 1, args);
}


//=========================================================================
//	TSchemeArgumentList Methods
//=========================================================================

void TSchemeArgumentList::TypeCheckFail()
{
	throw TException(__FILE__, __LINE__, "Argument type mismatch");
}

void TSchemeArgumentList::TypeCheck(Scheme_Type inType, Scheme_Object *inValue)
{
	if (inType != SCHEME_TYPE(inValue))
		TypeCheckFail();
}

void TSchemeArgumentList::TypeCheckStruct(const char *inPredicate,
										  Scheme_Object *inVal)
{
	Scheme_Object *b = TSchemeInterpreter::CallScheme(inPredicate, 1, &inVal);
	if (SCHEME_FALSEP(b))
		TypeCheckFail();
}

int32 TSchemeArgumentList::GetInt32Member(const char *inName,
										  Scheme_Object *inVal)
{
	Scheme_Object *val = TSchemeInterpreter::CallScheme(inName, 1, &inVal);
	if (!SCHEME_EXACT_INTEGERP(val))
		TypeCheckFail();
	long result;
	if (!scheme_get_int_val(val, &result))
		TypeCheckFail();
	return result;
}

std::string TSchemeArgumentList::GetStringArg()
{
	Scheme_Object *arg = GetNextArg();
	if (SCHEME_SYMBOLP(arg))
	{
		return SCHEME_SYM_VAL(arg);
	}
	else
	{
		TypeCheck(scheme_string_type, arg);
		return std::string(SCHEME_STR_VAL(arg), SCHEME_STRLEN_VAL(arg));
	}
}

int32 TSchemeArgumentList::GetInt32Arg()
{
	Scheme_Object *arg = GetNextArg();
	if (!SCHEME_EXACT_INTEGERP(arg))
		TypeCheckFail();
	long result;
	if (!scheme_get_int_val(arg, &result))
		TypeCheckFail();
	return result;
}

uint32 TSchemeArgumentList::GetUInt32Arg()
{
	Scheme_Object *arg = GetNextArg();
	if (!SCHEME_EXACT_INTEGERP(arg))
		TypeCheckFail();
	unsigned long result;
	if (!scheme_get_unsigned_int_val(arg, &result))
		TypeCheckFail();
	return result;
}

bool TSchemeArgumentList::GetBoolArg()
{
	Scheme_Object *arg = GetNextArg();
	return SCHEME_FALSEP(arg) ? false : true;	
}

double TSchemeArgumentList::GetDoubleArg()
{
	Scheme_Object *arg = GetNextArg();
	if (!SCHEME_REALP(arg))
		TypeCheckFail();
	return scheme_real_to_double(arg);
}

TPoint TSchemeArgumentList::GetPointArg()
{
	Scheme_Object *arg = GetNextArg();
	TypeCheckStruct("point?", arg);
	return TPoint(GetInt32Member("point-x", arg),
				  GetInt32Member("point-y", arg));
}

TRect TSchemeArgumentList::GetRectArg()
{
	Scheme_Object *arg = GetNextArg();
	TypeCheckStruct("rect?", arg);
	return TRect(GetInt32Member("rect-top", arg),
				 GetInt32Member("rect-left", arg),
				 GetInt32Member("rect-bottom", arg),
				 GetInt32Member("rect-right", arg));
}

GraphicsTools::Color TSchemeArgumentList::GetColorArg()
{
	Scheme_Object *arg = GetNextArg();
	TypeCheckStruct("color?", arg);
	return GraphicsTools::Color(GetInt32Member("color-red", arg),
								GetInt32Member("color-green", arg),
								GetInt32Member("color-blue", arg),
								GetInt32Member("color-alpha", arg));
}

void TSchemeArgumentList::GetValueOrPercentArg(bool &outIsPercent,
											   int32 &outValue)
{
	Scheme_Object *arg = GetNextArg();
	Scheme_Object *b = TSchemeInterpreter::CallScheme("percent?", 1, &arg);
	if (SCHEME_FALSEP(b))
	{
		outIsPercent = false;
		if (!SCHEME_EXACT_INTEGERP(arg))
			TypeCheckFail();
		long result;
		if (!scheme_get_int_val(arg, &result))
			TypeCheckFail();
		outValue = result;
	}
	else
	{
		outIsPercent = true;
		outValue = GetInt32Member("percent-value", arg);
	}
}

TCallback *TSchemeArgumentList::GetCallbackArg()
{
	Scheme_Object *arg = GetNextArg();
	if (!SCHEME_PROCP(arg))
		TypeCheckFail();
	return new TSchemeCallback(arg);
}

TArgumentList *TSchemeArgumentList::GetListArg()
{
    UNIMPLEMENTED;
	return NULL; //new TSchemeArgumentList();
}

