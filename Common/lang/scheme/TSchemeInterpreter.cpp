// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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
	std::string error, errormsg;

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
		TValueList inList;
		for (int i=0; i < (inArgc - 1); i++) 
			inList.push_back(SchemeToTValue((inArgv + 1)[i]));
	
		TArgumentList arg_list(inList);
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
			error = gVariableManager.Get(FIVEL_ERROR_CODE_VAR);
			errormsg = gVariableManager.Get(FIVEL_ERROR_MSG_VAR);
		}
		else
		{
			return TValueToScheme(gVariableManager.Get("_result"));
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

Scheme_Object *TSchemeInterpreter::MakeSchemePolygon(const TPolygon &inPoly) {
	std::vector<TPoint> vertices(inPoly.Vertices());
	size_t sz = vertices.size();

	scoped_array<Scheme_Object *> args(new Scheme_Object *[sz]);
	std::vector<TPoint>::iterator i = vertices.begin();
	for (int j = 0; i != vertices.end(); ++j, ++i) {
		args[j] = MakeSchemePoint(*i);
	}
	return CallScheme("polygon", sz, args.get());
}

Scheme_Object *
TSchemeInterpreter::MakeSchemePercent(const TPercent &inPercent) {
	Scheme_Object *args[1];
	args[0] = scheme_make_double(inPercent.GetValue());

	return CallScheme("percent", 1, args);
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

