// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"

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

	// Should our idle loop block until any events are received from 
	// the user?
	bool block;

	inArgs >> block;

	// Call our stored idle procedure and let the GUI run for a bit.
	scheme_interp->DoIdle(block);

	// Logging this primitive would only clutter the debug log.  We need
	// to do this *after* calling DoIdle, so that it doesn't get
	// confused with any internal primitive calls.
	::SkipPrimitiveLogging();
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
}

void TSchemeInterpreterManager::BeginScript()
{
	// Let our parent class set things up.
	TInterpreterManager::BeginScript();

	// Set the current Scheme directory.  This must be an absolute path,
	// so we check for ".", which is the only non-absolute path we
	// expect to receive from GetBaseDirectory.
	std::string base = FileSystem::GetBaseDirectory().ToNativePathString();
	if (base != ".")
		scheme_set_param(scheme_config, MZCONFIG_CURRENT_DIRECTORY,
						 scheme_make_string(base.c_str()));

	// Install our system loader.
	FileSystem::Path fivel_collection =
		FileSystem::GetRuntimeDirectory().AddComponent("5L");
	LoadFile(fivel_collection.AddComponent("loader.ss"));
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
TSchemePtr<Scheme_Object> TSchemeInterpreter::sLoaderModule = NULL;
TSchemePtr<Scheme_Object> TSchemeInterpreter::sKernelModule = NULL;
TInterpreter::SystemIdleProc TSchemeInterpreter::sSystemIdleProc = NULL;
TSchemeInterpreter::BucketMap TSchemeInterpreter::sBucketMap;

TSchemeInterpreter::TSchemeInterpreter(Scheme_Env *inGlobalEnv)
{
	// Declare a dummy argument list for calling Scheme functions.
	Scheme_Object *args;

	// Remember our global environment.
	sGlobalEnv = inGlobalEnv;

	InitializeModuleNames();

	// Create a new script environment, and store it where we can find it.
	sScriptEnv = NULL;
	CallSchemeEx(sGlobalEnv, sLoaderModule, "new-script-environment",
				 0, &args);
	sScriptEnv = scheme_get_env(scheme_config);

	// Load our kernel and script.
	Scheme_Object *result = 
		CallSchemeEx(sGlobalEnv, sLoaderModule, "load-script", 0, &args);
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
    sBucketMap.clear();
}

void TSchemeInterpreter::InitializeModuleNames()
{
	sLoaderModule = scheme_intern_symbol("5L-Loader");
	Scheme_Object *tail = scheme_make_pair(scheme_make_string("5L"),
										   scheme_null);
	sKernelModule =
		scheme_make_pair(scheme_intern_symbol("lib"),
						 scheme_make_pair(scheme_make_string("kernel.ss"),
										  tail));
}

Scheme_Bucket *
TSchemeInterpreter::FindBucket(Scheme_Env *inEnv,
                               Scheme_Object *inModule,
                               const char *inFuncName)
{
    // We keep a local map of known buckets.  We have to do this because we
    // don't want to allocate any memory on the Scheme heap during idle
    // calls (to avoid the risk of GC while playing movies), and
    // scheme_module_bucket relies on the module loader, which sometimes
    // allocates Scheme memory.
    BucketKey key(inEnv, inModule, inFuncName);
    BucketMap::iterator found = sBucketMap.find(key);
    if (found != sBucketMap.end())
        return found->second;
    else {
        Scheme_Object *sym = scheme_intern_symbol(inFuncName);
		Scheme_Bucket *bucket = scheme_module_bucket(inModule, sym, -1,
													 inEnv);
        if (bucket == NULL)
            throw TException(__FILE__, __LINE__,
                             "Scheme module bucket not found");
        sBucketMap.insert(BucketMap::value_type(key, bucket));
        return bucket;
    }
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
	bool have_error = false;
	TVariable::Type result_type;
	std::string res_str, error, errormsg;
	int32 result_long;
	uint32 result_ulong;
	double result_double;
	bool result_bool;
	TPoint result_point;
	TRect result_rect;
	GraphicsTools::Color result_color;

	// WARNING - Don't signal any Scheme errors from inside this try block.
	// (I don't know whether it's portable to call scheme_longjmp from
	// inside a try block.)  It's OK to signal Scheme errors in our catch
	// blocks, AFAIK.
	try
	{
		// Clear our error-handling variables.
		gVariableManager.MakeNull(FIVEL_ERROR_CODE_VAR);
		gVariableManager.MakeNull(FIVEL_ERROR_MSG_VAR);

		// Marshal our argument list and call the primitive.
		TSchemeArgvList arg_list(inArgc - 1, inArgv + 1);
		gPrimitiveManager.CallPrimitive(prim_name, arg_list);

		// Make sure all our arguments were used.
		if (arg_list.HasMoreArguments())
			throw TException(__FILE__, __LINE__, "Too many arguments");
		
		// Get our various result variables here (inside the try block),
		// and save them until we're allowed to raise Scheme errors
		// (outside the try block).
		if (!gVariableManager.IsNull(FIVEL_ERROR_CODE_VAR))
		{
			have_error = true;
			error = gVariableManager.GetString(FIVEL_ERROR_CODE_VAR);
			errormsg = gVariableManager.GetString(FIVEL_ERROR_MSG_VAR);
		}
		else
		{
			TVariable *result = gVariableManager.FindVariable("_result", true);
			result_type = result->GetType();
			switch (result_type)
			{
				case TVariable::TYPE_STRING: 
					res_str = result->GetString();
					break;

				case TVariable::TYPE_SYMBOL:
					res_str = result->GetSymbol();
					break;

				case TVariable::TYPE_LONG:
					result_long = result->GetLong();
					break;

				case TVariable::TYPE_ULONG:
					result_ulong = result->GetULong();
					break;

				case TVariable::TYPE_DOUBLE:
					result_double = result->GetDouble();
					break;

				case TVariable::TYPE_BOOLEAN:
					result_bool = result->GetBoolean();
					break;

				case TVariable::TYPE_POINT:
					result_point = result->GetPoint();
					break;

				case TVariable::TYPE_RECT:
					result_rect = result->GetRect();
					break;

				case TVariable::TYPE_COLOR:
					result_color = result->GetColor();
					break;

				case TVariable::TYPE_NULL:
				case TVariable::TYPE_UNINITIALIZED:
				default:
					/* Do nothing for now. */;
			}
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

	// Do a non-local exit if the primitive returned an error.
	if (have_error)
		scheme_signal_error("%s: %s: %s", prim_name,
							error.c_str(), errormsg.c_str());
	
	// Figure out what we should pass back to Scheme.
	switch (result_type)
	{
		case TVariable::TYPE_UNINITIALIZED:
			scheme_signal_error("%s: _result is broken", prim_name);
			
		case TVariable::TYPE_NULL:
			return scheme_void;

		case TVariable::TYPE_STRING:
			return scheme_make_sized_string(const_cast<char*>(res_str.c_str()),
											res_str.length(), true);

		case TVariable::TYPE_SYMBOL:
			return scheme_intern_symbol(const_cast<char*>(res_str.c_str()));

		case TVariable::TYPE_LONG:
			return scheme_make_integer_value(result_long);

		case TVariable::TYPE_ULONG:
			return scheme_make_integer_value_from_unsigned(result_ulong);

		case TVariable::TYPE_DOUBLE:
		    return scheme_make_double(result_double);

		case TVariable::TYPE_BOOLEAN:
			return result_bool ? scheme_true : scheme_false;

		case TVariable::TYPE_POINT:
			return MakeSchemePoint(result_point);

		case TVariable::TYPE_RECT:
			return MakeSchemeRect(result_rect);

		case TVariable::TYPE_COLOR:
			return MakeSchemeColor(result_color);

		default:
			scheme_signal_error("%s: _result has unsupported type", prim_name);
	}		

	ASSERT(false); // Should not get here.
	return scheme_false;
}

Scheme_Object *TSchemeInterpreter::CallSchemeEx(Scheme_Env *inEnv,
												Scheme_Object *inModule,
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
        Scheme_Bucket *bucket = FindBucket(inEnv, inModule, inFuncName);
		ASSERT(bucket != NULL);
		Scheme_Object *f = static_cast<Scheme_Object*>(bucket->val);
		if (f)
			result = scheme_apply(f, inArgc, inArgv);
		else
			gLog.FatalError("Can't find %s", inFuncName);
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
	return CallSchemeEx(sScriptEnv, sKernelModule, inFuncName,
						inArgc, inArgv);
}

Scheme_Object *TSchemeInterpreter::CallSchemeSimple(const char *inFuncName)
{
	// Call a function with no arguments.
	Scheme_Object *junk = scheme_false;
	return CallScheme(inFuncName, 0, &junk);
}

Scheme_Object *TSchemeInterpreter::MakeSchemePoint(const TPoint &inPoint) {
	Scheme_Object *args[2];
	args[0] = scheme_make_integer_value(inPoint.X());
	args[1] = scheme_make_integer_value(inPoint.Y());
	return CallScheme("point", 2, args);
}

Scheme_Object *TSchemeInterpreter::MakeSchemeRect(const TRect &inRect) {
	Scheme_Object *args[4];
	args[0] = scheme_make_integer_value(inRect.Left());
	args[1] = scheme_make_integer_value(inRect.Top());
	args[2] = scheme_make_integer_value(inRect.Right());
	args[3] = scheme_make_integer_value(inRect.Bottom());
	return CallScheme("rect", 4, args);
}

Scheme_Object *
TSchemeInterpreter::MakeSchemeColor(const GraphicsTools::Color &inColor) {
	Scheme_Object *args[4];
	args[0] = scheme_make_integer_value(inColor.red);
	args[1] = scheme_make_integer_value(inColor.green);
	args[2] = scheme_make_integer_value(inColor.blue);
	args[3] = scheme_make_integer_value(inColor.alpha);
	return CallScheme("color", 4, args);
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

void TSchemeInterpreter::Stop()
{
	ASSERT(!IsStopped());
    (void) CallSchemeSimple("%kernel-stop");
}

bool TSchemeInterpreter::IsStopped()
{
	Scheme_Object *o = CallSchemeSimple("%kernel-stopped?");
	return SCHEME_FALSEP(o) ? false : true;
}

void TSchemeInterpreter::Go(const char *card)
{
	ASSERT(IsStopped());
    (void) CallSchemeSimple("%kernel-go");
	JumpToCardByName(card);
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
	ASSERT(!IsStopped()); // Stopped cards must be resumed by Go().
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

bool TSchemeInterpreter::IsValidCard(const char *inCardName)
{
	Scheme_Object *args[1];
	args[0] = scheme_make_string(inCardName);
	Scheme_Object *o = CallScheme("%kernel-valid-card?", 1, args);
	return SCHEME_FALSEP(o) ? false : true;
}

bool TSchemeInterpreter::Eval(const std::string &inExpression,
							  std::string &outResultText)
{
	Scheme_Object *args[1];
	args[0] = scheme_make_string(inExpression.c_str());
	Scheme_Object *o = CallScheme("%kernel-eval", 1, args);
	if (!SCHEME_PAIRP(o) ||
		!SCHEME_BOOLP(SCHEME_CAR(o)) ||
		!SCHEME_STRINGP(SCHEME_CDR(o)))
		gLog.FatalError("Unexpected result from %kernel-eval");
	outResultText = SCHEME_STR_VAL(SCHEME_CDR(o));
	return SCHEME_FALSEP(SCHEME_CAR(o)) ? false : true;
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
		// TODO - Remove support for passing symbols where strings are wanted.
		gDebugLog.Caution("Symbol '%s passed as string argument.",
						  SCHEME_SYM_VAL(arg));
		return SCHEME_SYM_VAL(arg);
	}
	else
	{
		TypeCheck(scheme_string_type, arg);
		return std::string(SCHEME_STR_VAL(arg), SCHEME_STRLEN_VAL(arg));
	}
}

std::string TSchemeArgumentList::GetSymbolArg()
{
	Scheme_Object *arg = GetNextArg();
	if (!SCHEME_SYMBOLP(arg))
		TypeCheckFail();
	return SCHEME_SYM_VAL(arg);
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

TPolygon TSchemeArgumentList::GetPolygonArg()
{
	Scheme_Object *arg = GetNextArg();
	TypeCheckStruct("polygon?", arg);
	std::vector<TPoint> pts;
	Scheme_Object *scheme_pts = 
		TSchemeInterpreter::CallScheme("polygon-vertices", 1, &arg);
	if (!(SCHEME_PAIRP(scheme_pts) || SCHEME_NULLP(scheme_pts)))
		TypeCheckFail();
	Scheme_Object *current;

	while (!SCHEME_NULLP(scheme_pts))
	{
		current = SCHEME_CAR(scheme_pts);
		TypeCheckStruct("point?", current);
		pts.push_back(TPoint(GetInt32Member("point-x", current),
							 GetInt32Member("point-y", current)));
		scheme_pts = SCHEME_CDR(scheme_pts);
		if (!(SCHEME_PAIRP(scheme_pts) || SCHEME_NULLP(scheme_pts)))
			TypeCheckFail();
	}

	return TPolygon(pts);
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
