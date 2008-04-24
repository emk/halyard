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

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"
#include "TSchemeScriptEditorDB.h"

using namespace Halyard;

// Declared in TSchemePtr.h.
int Halyard::gTSchemePointerCount = 0;

static const char *CALL_PRIM = "%call-prim";

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

DEFINE_PRIMITIVE(SchemeExit)
{
	// Ask the interpreter manager to shut us down.  This function isn't
	// safe to call when we're being run as part of the interpreter (as
	// opposed to part of the test suites).  Use ExitScript instead.
	TInterpreterManager::GetInstance()->RequestQuitApplication();
}

DEFINE_PRIMITIVE(SchemeSetCollectsDir)
{
    std::string dir;
    inArgs >> dir;

    Scheme_Object *path = NULL;

    TSchemeReg<1> reg;
    reg.local(path);
    reg.done();

    // Set our standard collects dir.  For some reason, we can't actually
    // do this from Scheme.  But it's easier to actually decide what our
    // collects-dir should be from inside loader.ss, where we have path
    // functions available.
    path = scheme_make_path(dir.c_str());
    scheme_set_collects_path(path);
}

//=========================================================================
//	TSchemeInterpreterManager Methods
//=========================================================================

TSchemeInterpreterManager::TSchemeInterpreterManager(SystemIdleProc inIdleProc)
    : TInterpreterManager(inIdleProc)
{
	// Install our primitives.
	REGISTER_PRIMITIVE(SchemeExit);
    REGISTER_PRIMITIVE(SchemeSetCollectsDir);
    RegisterSchemeScriptEditorDBPrimitives();

    Scheme_Object *modname = NULL;
    Scheme_Env *engine_mod = NULL;
    Scheme_Object *call_prim = NULL;

    TSchemeReg<3> reg;
    reg.local(modname);
    reg.local(engine_mod);
    reg.local(call_prim);
    reg.done();

	// Initialize the Scheme interpreter.
	mGlobalEnv = scheme_basic_env();

	// Make a module to hold functions exported by the engine.
	modname = scheme_intern_symbol("#%engine-primitives");
	engine_mod = scheme_primitive_module(modname, mGlobalEnv);

	// Provide a way for Scheme code to call primitives.
	call_prim = scheme_make_prim_w_arity(&TSchemeInterpreter::CallPrim,
                                         CALL_PRIM, 1, -1);
	scheme_add_global(CALL_PRIM, call_prim, engine_mod);

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
    Scheme_Config *current_config = NULL;
    Scheme_Object *current_directory = NULL;

    TSchemeReg<2> reg;
    reg.local(current_config);
    reg.local(current_directory);
    reg.done();

	// Let our parent class set things up.
	TInterpreterManager::BeginScript();

	// Set the current Scheme directory.  This must be an absolute path,
	// so we check for ".", which is the only non-absolute path we
	// expect to receive from GetBaseDirectory.
	std::string base = FileSystem::GetBaseDirectory().ToNativePathString();
	ASSERT(base != ".");
    current_config = scheme_current_config();
    current_directory = scheme_make_path(base.c_str());
	scheme_set_param(current_config, MZCONFIG_CURRENT_DIRECTORY,
					 current_directory);

	// Install our system loader.
	FileSystem::Path halyard_collection =
		FileSystem::GetRuntimeDirectory().AddComponent("halyard");
	LoadFile(halyard_collection.AddComponent("loader.ss"));
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

void TSchemeInterpreterManager::MakeInterpreter()
{
    // We should have only mGlobalEnv at this point: Any more or less is
    // most likely the result of a memory leak somewhere, and will most
    // likely prevent us from GC'ing old copies of the script after a
    // reload.  This is not an assertion, because we _always_ want to
    // perform this check, even if we're not in debug mode.  Any time
    // this condition fails, the engine will become almost unusable
    // for development purposes.
    if (gTSchemePointerCount != 1)
        gLog.FatalError("Leaking TSchemePtr objects: found %d extra",
                        gTSchemePointerCount - 1);

    // Create a new TSchemeInterpreter.  Once this is created, it can be
    // accessed using either TInterpreter::GetInstance() or
    // TSchemeInterpreter::GetSchemeInterpreter().
    //
    // MANUAL GC PROOF REQUIRED - mGlobalEnv can't be GC'd while it is live
    // in this function.
	new TSchemeInterpreter(mGlobalEnv);
}


//=========================================================================
//	TSchemeInterpreter Methods
//=========================================================================

TSchemeInterpreter::TSchemeInterpreter(Scheme_Env *inGlobalEnv)
{
    Scheme_Config *current_config = NULL;
    Scheme_Object *result = NULL;
    Scheme_Object *byte_str = NULL;
    char *raw_str = NULL;

    TSchemeReg<5> reg;
    reg.param(inGlobalEnv);
    reg.local(current_config);
    reg.local(result);
    reg.local(byte_str);
    reg.local(raw_str);
    reg.done();

	// Remember our global environment.
	mGlobalEnv = inGlobalEnv;

	InitializeModuleNames();

	// Create a new script environment, and store it where we can find it.
	mScriptEnv = NULL;
	CallSchemeEx(mGlobalEnv, mLoaderModule, "new-script-environment", 0, NULL);
    current_config = scheme_current_config();
	mScriptEnv = scheme_get_env(current_config);

	// Load our kernel and script.
	result = CallSchemeEx(mGlobalEnv, mLoaderModule, "load-script", 0, NULL);
	if (!SCHEME_FALSEP(result))
	{
		ASSERT(SCHEME_CHAR_STRINGP(result));
		byte_str = scheme_char_string_to_byte_string(result);
        raw_str = SCHEME_BYTE_STR_VAL(byte_str);
		throw TException(__FILE__, __LINE__, raw_str);
	}
}

TSchemeInterpreter::~TSchemeInterpreter()
{
	// We don't actually shut down the Scheme interpreter.  But we'll
	// reinitialize it later if we need to.
}

TSchemeInterpreter *TSchemeInterpreter::GetSchemeInterpreter() {
    TSchemeInterpreter *interp =
        dynamic_cast<TSchemeInterpreter*>(TInterpreter::GetInstance());
    ASSERT(interp != NULL);
    return interp;
}

void TSchemeInterpreter::InitializeModuleNames()
{
    Scheme_Object *halyard_string = NULL;
    Scheme_Object *tail1 = NULL;
    Scheme_Object *lib_symbol = NULL;
    Scheme_Object *kernel_ss_string = NULL;
    Scheme_Object *tail2 = NULL;

    TSchemeReg<5> reg;
    reg.local(halyard_string);
    reg.local(tail1);
    reg.local(lib_symbol);
    reg.local(kernel_ss_string);
    reg.local(tail2);
    reg.done();

	mLoaderModule = scheme_intern_symbol("loader");

    halyard_string   = scheme_make_utf8_string("halyard");
	tail1            = scheme_make_pair(halyard_string, scheme_null);
    lib_symbol       = scheme_intern_symbol("lib");
    kernel_ss_string = scheme_make_utf8_string("kernel.ss");
    tail2            = scheme_make_pair(kernel_ss_string, tail1);
	mKernelModule    = scheme_make_pair(lib_symbol, tail2);
}

Scheme_Bucket *
TSchemeInterpreter::FindBucket(Scheme_Env *inEnv,
                               Scheme_Object *inModule,
                               const char *inFuncName)
{
    Scheme_Object *sym = NULL;
    Scheme_Bucket *bucket = NULL;

    TSchemeReg<4> reg;
    reg.param(inEnv);
    reg.param(inModule);
    reg.local(sym);
    reg.local(bucket);
    reg.done();

    // Map our inEnv and inModule arguments to stable identifiers that are
    // safe to use as std::map keys.  We can't use the underlying pointers
    // as keys, because they might be moved by the GC.  Of course, this
    // forces us to clear mBucketMap every time we destroy a
    // TSchemeInterpreter object.
    BucketKey::Env env;
    typedef Scheme_Env *EnvPtr;
    if (Eq(inEnv, EnvPtr(mGlobalEnv)))
        env = BucketKey::GLOBAL_ENV;
    else if (Eq(inEnv, EnvPtr(mScriptEnv)))
        env = BucketKey::SCRIPT_ENV;
    else
        THROW("Unknown Scheme environment");
    BucketKey::Module module;
    typedef Scheme_Object *ObjectPtr;
    if (Eq(inModule, ObjectPtr(mLoaderModule)))
        module = BucketKey::LOADER_MODULE;
    else if (Eq(inModule, ObjectPtr(mKernelModule)))
        module = BucketKey::KERNEL_MODULE;
    else
        THROW("Unknown Scheme environment");
    
    // We keep a local map of known buckets.  We have to do this because we
    // don't want to allocate any memory on the Scheme heap during idle
    // calls (to avoid the risk of GC while playing movies), and
    // scheme_module_bucket relies on the module loader, which sometimes
    // allocates Scheme memory.
    //
    // TODO - Now that we have a much smarter IDLE system, is this actually
    // relevant?  Or can we just get rid of all this machinery?
    BucketKey key(env, module, inFuncName);
    BucketMap::iterator found = mBucketMap.find(key);
    if (found != mBucketMap.end())
        return found->second;
    else {
        sym = scheme_intern_symbol(inFuncName);
		bucket = scheme_module_bucket(inModule, sym, -1, inEnv);
        if (bucket == NULL)
            throw TException(__FILE__, __LINE__,
                             "Scheme module bucket not found");
        mBucketMap.insert(BucketMap::value_type(key, bucket));
        return bucket;
    }
}

Scheme_Object *TSchemeInterpreter::CallPrim(int inArgc, Scheme_Object **inArgv)
{
    // We need to be very careful here, because mixing scheme_signal_error
    // with C++ exceptions will subtly corrupt the C++ exception-handling
    // runtime, and the errors will show up once in every few days of
    // Halyard use (bug #1691, bug #1762, etc) on Win32 systems, at least
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
    //   005F7595 push offset __ehhandler$?CallPrim@TSchemeInterpreter@
    //                        Halyard@@CAPAUScheme_Object@@HPAPAU3@@Z (7FD260h)
    //
    // If you see that, you've allowed a 'try' block or a stack-based object
    // with a destructor to sneak into this function.  That stuff *must* go
    // in CallPrimInternal below, *not* here.

    // MANUAL GC PROOF REQUIRED - We need to use the low-level PLT
    // MZ_GC_DECL_REG here instead of TSchemeReg, because we don't want the
    // C++ compiler to silently install exception handlers.  Please verify
    // that this function follows the correct protocol.

    Scheme_Object *prim_name_char_str = NULL;
    Scheme_Object *prim_name_byte_str = NULL;
    char *prim_name = NULL;
    Scheme_Object *result = NULL; // Mandatory NULL initialization.

    MZ_GC_DECL_REG(7);            // 3 slots for array, 1 for each var
    MZ_GC_ARRAY_VAR_IN_REG(0, inArgv, inArgc);
    MZ_GC_VAR_IN_REG(3, result);  // Skip two slots used by inArgv.
    MZ_GC_VAR_IN_REG(4, prim_name_char_str);
    MZ_GC_VAR_IN_REG(5, prim_name_byte_str);
    MZ_GC_VAR_IN_REG(6, prim_name);
    MZ_GC_REG();

	// The interpreter checks the arity for us, but we need to check the
	// argument types.
	ASSERT(inArgc >= 1);
	if (!SCHEME_SYMBOLP(inArgv[0]))
		scheme_wrong_type(CALL_PRIM, "symbol", 0, inArgc, inArgv);

    // MANUAL GC PROOF REQUIRED - Here, we need to get a GC-safe pointer to
    // the string contained in argv[0].  This is trickier than it looks,
    // because SCHEME_SYM_VAL actually returns an internal pointer, which
    // isn't safe to hold onto across memory allocations (nasty, huh?).
    //
    // We can't copy this pointer into either a std::string (because that
    // would install an exception handler) or a malloc'd char* (because
    // we'd leak that if we call scheme_signal_error, below).  So we need
    // to very carefully convert this symbol name to a Scheme string
    // allocated on the Scheme heap.
    //
    // This code is adapted from mzscheme/src/symbol.c's implementation of
    // symbol_to_string_prim.  Here, the first argument to
    // scheme_make_sized_offset_utf8_string is a pointer to the symbol
    // itself, and the offset points to the start of the actual string.
    prim_name_char_str =
        scheme_make_sized_offset_utf8_string((char *) (inArgv[0]),
                                             SCHEME_SYMSTR_OFFSET(inArgv[0]),
                                             SCHEME_SYM_LEN(inArgv[0]));
    ASSERT(SCHEME_CHAR_STRINGP(prim_name_char_str));
    prim_name_byte_str = scheme_char_string_to_byte_string(prim_name_char_str);
    ASSERT(SCHEME_BYTE_STRINGP(prim_name_byte_str));
    prim_name = SCHEME_BYTE_STR_VAL(prim_name_byte_str);
    ASSERT(strlen(prim_name) > 0);

    TSchemeInterpreter *interp = GetSchemeInterpreter();

    // Dispatch the primitive call to the routine which is allowed to throw
    // and catch C++ exceptions.
    char error_message[1024];
    if (interp->CallPrimInternal(prim_name, inArgc-1, inArgv+1, &result,
                                 error_message, sizeof(error_message))) {
        ASSERT(result);
        MZ_GC_UNREG();
        return result;
    } else {
        // We have an error, so let's turn into a Scheme exception, now
        // that we're safely away from any 'try/catch' blocks.
        //
        // Note that we don't need to call MZ_GC_UNREG() before calling
        // scheme_signal_error, because the GC frame stack will be unwound
        // by scheme_setjmp.  But this only works with scheme_setjmp, not
        // with C++ exceptions.
        ASSERT(!result);
        ASSERT(strlen(error_message) < sizeof(error_message));
        scheme_signal_error("%s: %s", prim_name, error_message);

        ASSERT(false); // Should not get here.
        MZ_GC_UNREG();
        return scheme_false;
    }
}

bool TSchemeInterpreter::CallPrimInternal(const char *inPrimName, // Scheme heap
                                          int inArgc, Scheme_Object **inArgv,
                                          Scheme_Object **outResult,
                                          char *outErrorMessage,
                                          size_t inErrorMessageMaxLength)
{
    TSchemeReg<1,1> reg;
    reg.param(inPrimName);
    reg.param_array(inArgv, inArgc);
    // MANUAL GC PROOF REQUIRED - *outResult is correctly registered by our
    // caller.
    reg.done();

    // This function may *not* call scheme_signal_error, scheme_wrong_type,
    // or anything else which causes a non-local PLT exit.  See CallPrim
    // for more details.
	try {
        // Only SchemeSetCollectsDir may be called without a mScriptEnv.
        ASSERT(mScriptEnv != NULL ||
               strcmp(inPrimName, "SchemeSetCollectsDir") == 0);

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
        strncpy_terminated(outErrorMessage, "Unknown engine error",
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
    Scheme_Bucket *bucket = NULL;
    Scheme_Object *func = NULL;

    TSchemeReg<4,1> reg;
    reg.param(inEnv);
    reg.param(inModule);
    reg.param_array(inArgv, inArgc);
    reg.local(bucket);
    reg.local(func);
    reg.done();

    // Look up the function to call.  (Note that scheme_module_bucket will
    // look up names in the module's *internal* namespace, not its official
    // export namespace.)
    //
    // We do this here, and not in CallSchemeExHelper, because FindBucket
    // may throw exceptions and it's safer and easier to analyze the code
    // when we separate exception-handling code from code using MZ_GC_REG
    // and scheme_setjmp.
    bucket = FindBucket(inEnv, inModule, inFuncName);
    ASSERT(bucket != NULL);
    func = static_cast<Scheme_Object*>(bucket->val);
    if (!func)
        gLog.FatalError("Can't find %s", inFuncName);
    return CallSchemeExHelper(func, inArgc, inArgv);
}

Scheme_Object *TSchemeInterpreter::CallSchemeExHelper(Scheme_Object *inFunc,
                                                      int inArgc,
                                                      Scheme_Object **inArgv)
{
    // MANUAL GC PROOF REQUIRED - This is the inner part of CallSchemeEx.
    // Because we use scheme_setjmp in this function, we also need to use
    // MZ_GC_DECL_REG.  This allows scheme_setjmp to know how to unwind the
    // GC stack properly.  But this also implies that we can't use C++
    // exceptions, or local variables with destructors.

	Scheme_Object *result = NULL;

    // MANUAL GC PROOF REQUIRED - Double-check these indices and make sure
    // we're obeying the standard PLT garbage-collection protocol.
    MZ_GC_DECL_REG(5);            // 3 for array, 1 for each var
    MZ_GC_VAR_IN_REG(0, inFunc);
    MZ_GC_ARRAY_VAR_IN_REG(1, inArgv, inArgc);
    MZ_GC_VAR_IN_REG(4, result);  // Skip two for array
    MZ_GC_REG();

	// Save our jump buffer.
	mz_jmp_buf save;
	memcpy(&save, &scheme_error_buf, sizeof(mz_jmp_buf));
	
	// Install a Scheme exception handler.  See the MzScheme internals
	// manual for more information on how this works; it appears to be
	// built around the arcane setjmp and longjmp functions in C.
	if (scheme_setjmp(scheme_error_buf)) {
		// Scheme tried to throw an exception out of inFuncName, which
		// is forbidden.  So we want to die with a fatal error.  (Note
		// that we can't get the error message from C; this exception
		// should have been caught and handled by Scheme.)
		gLog.FatalError("Scheme kernel threw unexpected exception");
	} else {
		// Call the function.  
        result = scheme_apply(inFunc, inArgc, inArgv);
	}

	// Restore our jump buffer and exit.
	memcpy(&scheme_error_buf, &save, sizeof(mz_jmp_buf));
    MZ_GC_UNREG();
	return result;
}

Scheme_Object *TSchemeInterpreter::CallScheme(const char *inFuncName,
											  int inArgc,
											  Scheme_Object **inArgv)
{
	// Under normal circumstances, we only want to call functions defined
	// in the kernel, which is running in the script's namespace.
    //
    // MANUAL GC PROOF REQUIRED - It's safe to pass these variables
    // straight through, because we don't do any allocations.
	return CallSchemeEx(mScriptEnv, mKernelModule, inFuncName, inArgc, inArgv);
}

Scheme_Object *TSchemeInterpreter::CallSchemeSimple(const char *inFuncName)
{
	// Call a function with no arguments.
	return CallScheme(inFuncName, 0, NULL);
}

Scheme_Object *TSchemeInterpreter::CallSchemeStatic(const char *inFuncName,
                                                    int inArgc,
                                                    Scheme_Object **inArgv)
{
    // MANUAL GC PROOF REQUIRED - It's safe to pass these variables
    // straight through, because we don't do any allocations.
    return GetSchemeInterpreter()->CallScheme(inFuncName, inArgc, inArgv);
}

void TSchemeInterpreter::Run()
{
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
    // MANUAL GC PROOF REQUIRED - We do no allocation after the call to
    // CallSchemeSimple, so don't need a TSchemeReg here.
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
    // MANUAL GC PROOF REQUIRED - We do no allocation after the call to
    // CallSchemeSimple, so don't need a TSchemeReg here.
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
    // MANUAL GC PROOF REQUIRED - We do no allocation after the call to
    // CallSchemeSimple, so don't need a TSchemeReg here.
	Scheme_Object *o = CallSchemeSimple("%kernel-paused?");
	return SCHEME_FALSEP(o) ? false : true;
}

void TSchemeInterpreter::KillCurrentCard(void)
{
    (void) CallSchemeSimple("%kernel-kill-current-card");
}

void TSchemeInterpreter::JumpToCardByName(const char *inName)
{
    TSchemeArgs<1> args;
    TSchemeReg<0,1> reg;
    reg.args(args);
    reg.done();

	ASSERT(!IsStopped()); // Stopped cards must be resumed by Go().

	args[0] = scheme_make_utf8_string(inName);
	(void) CallScheme("%kernel-jump-to-card-by-name", args.size(), args.get());
}

std::string TSchemeInterpreter::CurCardName(void)
{
    Scheme_Object *name_obj = NULL, *name_byte_str = NULL;
    char *name = NULL;

    TSchemeReg<3> reg;
    reg.local(name_obj);
    reg.local(name_byte_str);
    reg.local(name);
    reg.done();

	name_obj = CallSchemeSimple("%kernel-current-card-name");
	if (!SCHEME_CHAR_STRINGP(name_obj))
		gLog.FatalError("Current card name must be string");
    name_byte_str = scheme_char_string_to_byte_string(name_obj);
    name = SCHEME_BYTE_STR_VAL(name_byte_str);
	return std::string(name);
}

bool TSchemeInterpreter::IsValidCard(const char *inCardName)
{
    Scheme_Object *b = NULL;
    TSchemeArgs<1> args;

    TSchemeReg<1,1> reg;
    reg.local(b);
    reg.args(args);
    reg.done();

	args[0] = scheme_make_utf8_string(inCardName);
	b = CallScheme("%kernel-valid-card?", args.size(), args.get());
	return SCHEME_FALSEP(b) ? false : true;
}

bool TSchemeInterpreter::Eval(const std::string &inExpression,
							  std::string &outResultText)
{
    Scheme_Object *o = NULL, *car = NULL, *cdr = NULL, *byte_str = NULL;
    TSchemeArgs<1> args;

    TSchemeReg<4,1> reg;
    reg.local(o);
    reg.local(car);
    reg.local(cdr);
    reg.local(byte_str);
    reg.args(args);
    reg.done();
    
	args[0] = scheme_make_utf8_string(inExpression.c_str());
	o = CallScheme("%kernel-eval", args.size(), args.get());

	if (!SCHEME_PAIRP(o))
		gLog.FatalError("Unexpected result from %kernel-eval");
    car = SCHEME_CAR(o);
    cdr = SCHEME_CDR(o);
    if (!SCHEME_BOOLP(car) || !SCHEME_CHAR_STRINGP(cdr))
		gLog.FatalError("Unexpected result from %kernel-eval");

	byte_str = scheme_char_string_to_byte_string(cdr);
	outResultText = std::string(SCHEME_BYTE_STR_VAL(byte_str));
	return SCHEME_FALSEP(car) ? false : true;
}

std::vector<TScriptIdentifier> TSchemeInterpreter::GetKnownIdentifiers() {
    Scheme_Object *raw_ids = NULL;

    TSchemeReg<1> reg;
    reg.local(raw_ids);
    reg.done();

    // Fetch our list of identifiers & types.
    raw_ids = CallSchemeSimple("%kernel-get-identifiers");

    // Convert this list into C++ objects.
	std::vector<TScriptIdentifier> ids;
    while (SCHEME_PAIRP(raw_ids)) {
        Scheme_Object *raw_id = NULL, *cdr = NULL, *cddr = NULL;
        Scheme_Object *raw_name = NULL, *raw_type = NULL, *raw_hint = NULL;

        TSchemeReg<6> reg;
        reg.local(raw_id);
        reg.local(cdr);
        reg.local(cddr);
        reg.local(raw_name);
        reg.local(raw_type);
        reg.local(raw_hint);
        reg.done();

        // MANUAL GC PROOF REQUIRED - Read through this whole section
        // slowly and make sure it's obeying all the rules.
        raw_id = SCHEME_CAR(raw_ids);
        if (!SCHEME_PAIRP(raw_id)
            || !SCHEME_PAIRP((cdr = SCHEME_CDR(raw_id)))
            || !SCHEME_PAIRP((cddr = SCHEME_CDR(cdr)))
            || !SCHEME_NULLP(SCHEME_CDR(cddr)))
            gLog.FatalError("Malformed result from %kernel-get-identifiers");
        raw_name = SCHEME_CAR(raw_id);
        raw_type = SCHEME_CAR(cdr);
        raw_hint = SCHEME_CAR(cddr);
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
