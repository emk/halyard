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
#include "TSchemeScriptEditorDB.h"

USING_NAMESPACE_FIVEL

static const char *CALL_5L_PRIM = "%call-5l-prim";

#define UNIMPLEMENTED \
	throw TException(__FILE__, __LINE__, "Scheme: Not yet implemented")

namespace {
    /// Like strncpy, but terminates strings which don't fit in 'out'.
    void strncpy_terminated(char *out, const char *in, size_t max) {
        strncpy(out, in, max);
        out[max-1] = '\0';
    }
};


//=========================================================================
//	Scheme Primitives
//=========================================================================

DEFINE_5L_PRIMITIVE(SchemeExit)
{
	// Ask the interpreter manager to shut us down.  This function isn't
	// safe to call when we're being run as part of the interpreter (as
	// opposed to part of the test suites).  Use TamaleExit instead.
	TInterpreterManager::GetInstance()->RequestQuitApplication();
}

// TODO - Rename to Idle and move to TCommonPrimitives.cpp
DEFINE_5L_PRIMITIVE(SchemeIdle)
{
	// Recover our Scheme interpreter.
	TInterpreter *interp = TInterpreter::GetInstance();
	ASSERT(interp);

	// Should our idle loop block until any events are received from 
	// the user?
	bool block;

	inArgs >> block;

	// Call our stored idle procedure and let the GUI run for a bit.
	interp->DoIdle(block);

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
    RegisterSchemeScriptEditorDBPrimitives();

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

ScriptEditorDB *TSchemeInterpreterManager::GetScriptEditorDBInternal() {
    const char *db_path = "definitions.sqlite3";
    if (!mScriptEditorDB && TInterpreter::HaveInstance()) {
        mScriptEditorDB =
            shared_ptr<ScriptEditorDB>(new TSchemeScriptEditorDB(db_path));
    }
    return mScriptEditorDB.get();
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
    // We need to be very careful here, because mixing scheme_signal_error
    // with C++ exceptions will subtly corrupt the C++ exception-handling
    // runtime, and the errors will show up once in every few days of
    // Tamale use (bug #1691, bug #1762, etc) on Win32 systems, at least
    // when compiling with MSVC++.Net.  In particular, the following code
    // is illegal:
    //
    //   scheme_signal_error("dying immediately");
    //   try {
    //   } catch (std::exception &) {
    //   }
    //   return scheme_false;
    //
    // If you disassemble the current function, you should *not* see a line
    // in the function prologue which looks anything like:
    //
    //   005F7595 push offset __ehhandler$?Call5LPrim@TSchemeInterpreter@ \
    //                        FiveL@@CAPAUScheme_Object@@HPAPAU3@@Z (7FD260h)
    //
    // If you see that, you've allowed a 'try' block or a stack-based object
    // with a destructor to sneak into this function.  That stuff *must* go
    // in Call5LPrimInternal below, *not* here.

	ASSERT(sScriptEnv != NULL);

	// The interpreter checks the arity for us, but we need to check the
	// argument types.
	ASSERT(inArgc >= 1);
	if (!SCHEME_SYMBOLP(inArgv[0]))
		scheme_wrong_type(CALL_5L_PRIM, "symbol", 0, inArgc, inArgv);
	const char *prim_name = SCHEME_SYM_VAL(inArgv[0]);

    // Dispatch the primitive call to the routine which is allowed to throw
    // and catch C++ exceptions.
    Scheme_Object *result = NULL;
    char error_message[1024];
    if (Call5LPrimInternal(prim_name, inArgc-1, inArgv+1, &result,
                           error_message, sizeof(error_message))) {
        ASSERT(result);
        return result;
    } else {
        // We have an error, so let's turn into a Scheme exception, now
        // that we're safely away from any 'try/catch' blocks.
        ASSERT(!result);
        ASSERT(strlen(error_message) < sizeof(error_message));
        scheme_signal_error("%s: %s", prim_name, error_message);

        ASSERT(false); // Should not get here.
        return scheme_false;
    }
                           
}

bool TSchemeInterpreter::Call5LPrimInternal(const char *inPrimName,
                                            int inArgc, Scheme_Object **inArgv,
                                            Scheme_Object **outResult,
                                            char *outErrorMessage,
                                            size_t inErrorMessageMaxLength)
{
    // This function may *not* call scheme_signal_error, scheme_wrong_type,
    // or anything else which causes a non-local PLT exit.  See Call5LPrim
    // for more details.
    const char *error_message = NULL;
	try {
		// Marshal our argument list and call the primitive.
		TValueList inList;
		for (int i=0; i < inArgc; i++) 
			inList.push_back(SchemeToTValue(inArgv[i]));	
		TArgumentList arg_list(inList);
		gPrimitiveManager.CallPrimitive(inPrimName, arg_list);

        // Get our result, and return it.
        *outResult = TValueToScheme(gVariableManager.Get("_result"));
        return true;
	} catch (std::exception &e) {
        strncpy_terminated(outErrorMessage, e.what(), inErrorMessageMaxLength);
        return false;    
	} catch (...) {
        // This shouldn't happen--we shouldn't be throwing any exceptions
        // which aren't a subclass of std::exception, and Win32 structured
        // exceptions (e.g., segfaults) should be caught by the crash
        // reporting library.
        strncpy_terminated(outErrorMessage, "Unknown 5L engine error",
                           inErrorMessageMaxLength);
        return false;
	}
}

Scheme_Object *TSchemeInterpreter::CallSchemeEx(Scheme_Env *inEnv,
												Scheme_Object *inModule,
												const char *inFuncName,
												int inArgc,
												Scheme_Object **inArgv)
{
	Scheme_Object *result = scheme_false;

    // Look up the function to call.  (Note that scheme_module_bucket will
    // look up names in the module's *internal* namespace, not its official
    // export namespace.)
    //
    // We do this before calling scheme_setjmp because FindBucket may use
    // some C++ exception-handling machiney (in the STL) and we want to
    // make sure we're still unambiguously in a C++ context when we call
    // it.
    //
    // We might need to be even more paranoid here--see the notes in
    // Call5LPrim for an idea of how evil combining setjmp/longjmp and
    // try/catch/throw can be--but I can't find anything suspicious in the
    // disassembly of this function, so I'm going to leave it fow now.
    Scheme_Bucket *bucket = FindBucket(inEnv, inModule, inFuncName);
    ASSERT(bucket != NULL);

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
		// Call the function.  
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

bool TSchemeInterpreter::CanSuspend()
{
	Scheme_Object *o = CallSchemeSimple("%kernel-can-suspend?");
	return SCHEME_FALSEP(o) ? false : true;
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

std::vector<TScriptIdentifier> TSchemeInterpreter::GetKnownIdentifiers() {
    // Fetch our list of identifiers & types.
    Scheme_Object *raw_ids = CallSchemeSimple("%kernel-get-identifiers");

    // Convert this list into C++ objects.
	std::vector<TScriptIdentifier> ids;
    while (SCHEME_PAIRP(raw_ids)) {
        Scheme_Object *raw_id = SCHEME_CAR(raw_ids);
        Scheme_Object *cdr, *cddr;
        if (!SCHEME_PAIRP(raw_id)
            || !SCHEME_PAIRP((cdr = SCHEME_CDR(raw_id)))
            || !SCHEME_PAIRP((cddr = SCHEME_CDR(cdr)))
            || !SCHEME_NULLP(SCHEME_CDR(cddr)))
            gLog.FatalError("Malformed result from %kernel-get-identifiers");
        Scheme_Object *raw_name = SCHEME_CAR(raw_id);
        Scheme_Object *raw_type = SCHEME_CAR(cdr);
        Scheme_Object *raw_hint = SCHEME_CAR(cddr);
        if (!SCHEME_SYMBOLP(raw_name) || !SCHEME_SYMBOLP(raw_type))
            gLog.FatalError("Malformed result from %kernel-get-identifiers");
        long hint;
        if (!scheme_get_int_val(raw_hint, &hint))
            gLog.FatalError("Malformed result from %kernel-get-identifiers");
		std::string type_str(SCHEME_SYM_VAL(raw_type));
		TScriptIdentifier::Type type = IdentifierType(type_str);
        ids.push_back(TScriptIdentifier(SCHEME_SYM_VAL(raw_name), type, hint));
        raw_ids = SCHEME_CDR(raw_ids);
    }
    if (!SCHEME_NULLP(raw_ids))
        gLog.FatalError("Malformed result from %kernel-get-identifiers");
    return ids;
}

TScriptIdentifier::Type
TSchemeInterpreter::IdentifierType(const std::string &type_str) {
    TScriptIdentifier::Type type = TScriptIdentifier::UNKNOWN;
    if (type_str == "syntax")
        type = TScriptIdentifier::KEYWORD;
    else if (type_str == "function")
        type = TScriptIdentifier::FUNCTION;
    else if (type_str == "variable")
        type = TScriptIdentifier::VARIABLE;
    else if (type_str == "constant")
        type = TScriptIdentifier::CONSTANT;
    else if (type_str == "class")
        type = TScriptIdentifier::CLASS;
    else if (type_str == "template")
        type = TScriptIdentifier::TEMPLATE;
    else if (type_str == "group")
        type = TScriptIdentifier::GROUP;
    else if (type_str == "sequence")
        type = TScriptIdentifier::SEQUENCE;
    else if (type_str == "card")
        type = TScriptIdentifier::CARD;
    else if (type_str == "element")
        type = TScriptIdentifier::ELEMENT;
    return type;
}
