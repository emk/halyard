// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "TInterpreter.h"
#include "TStateDB.h"
#include "lang/scheme/StackBase.h"

using namespace Halyard;


//=========================================================================
//  TInterpreter Methods
//=========================================================================

TInterpreter *TInterpreter::sInstance = NULL;

TInterpreter::TInterpreter()
    : mSourceFilesLoaded(0), mSourceFilesExpected(0)
{
    // Set up singleton.
    ASSERT(sInstance == NULL);
    sInstance = this;

    // If we have a CachedConf object (i.e., we're not running the
    // test suites), then load in the expected number of source files.
    TInterpreterManager *manager(TInterpreterManager::GetInstance());
    TInterpreterCachedConf *conf = manager->GetCachedConf();
    if (conf) {
        if (manager->IsLazyLoadingEnabled())
            mSourceFilesExpected = conf->ReadLong("LazySourceFileCount", 0);
        else
            mSourceFilesExpected = conf->ReadLong("SourceFileCount", 0);
    }
}

TInterpreter::~TInterpreter()
{
    sInstance = NULL;
}

void TInterpreter::DestroyInstance() {
    if (sInstance)
        delete sInstance;
    ASSERT(sInstance == NULL);
}

void TInterpreter::NotifyFileLoaded() {
    ++mSourceFilesLoaded;
    if (mSourceFilesLoaded > mSourceFilesExpected)
        mSourceFilesExpected = mSourceFilesLoaded;
}

void TInterpreter::NotifyScriptLoaded() {
    mSourceFilesExpected = mSourceFilesLoaded;

    // If we have a cached configuration (i.e., we're not running the
    // test suites), and we're not in runtime mode, then update our
    // source file count.
    TInterpreterManager *manager(TInterpreterManager::GetInstance());
    TInterpreterCachedConf *conf = manager->GetCachedConf();
    if (conf && !manager->IsInRuntimeMode()) {
        if (manager->IsLazyLoadingEnabled())
            conf->WriteLong("LazySourceFileCount", mSourceFilesExpected);
        else
            conf->WriteLong("SourceFileCount", mSourceFilesExpected);
    }
}

double TInterpreter::GetLoadProgress() {
    if (mSourceFilesExpected == 0)
        return 0.0;
    else
        return (1.0 * mSourceFilesLoaded) / mSourceFilesExpected;
}


//=========================================================================
//  TReloadNotified Methods
//=========================================================================

TReloadNotified::TReloadNotified() {
    TInterpreterManager::AddReloadNotified(this);
}

TReloadNotified::~TReloadNotified() {
    TInterpreterManager::RemoveReloadNotified(this);
}


//=========================================================================
//  TInterpreterCachedConf Methods
//=========================================================================

TInterpreterCachedConf::TInterpreterCachedConf() {
    TInterpreterManager::GetInstance()->RegisterCachedConf(this);
}

TInterpreterCachedConf::~TInterpreterCachedConf() {
    TInterpreterManager::GetInstance()->UnregisterCachedConf(this);
}


//=========================================================================
//  TInterpreterManager Methods
//=========================================================================
//  This class contains a fairly odd state machine, which is used to
//  manage the lifetime of TInterpreter objects and run the main event
//  loop.

TInterpreterManager *TInterpreterManager::sInstance = NULL;
bool TInterpreterManager::sHaveAlreadyCreatedSingleton = false;
std::vector<TReloadNotified*> TInterpreterManager::sReloadNotifiedObjects;
TInterpreterManager::Mode TInterpreterManager::sMode =
    TInterpreterManager::AUTHORING;
bool TInterpreterManager::sIsFirstLoad = true;
bool TInterpreterManager::sHaveInitialCommand = false;
std::string TInterpreterManager::sInitialCommand;

TInterpreterManager::TInterpreterManager(SystemIdleProc inIdleProc)
    : mCachedConf(NULL), mIsInsideStackBase(false), 
      mIsLazyLoadingRequested(false), mShouldConsiderExiting(false)
{
	ASSERT(sHaveAlreadyCreatedSingleton == false);
	sHaveAlreadyCreatedSingleton = true;
	ASSERT(sInstance == NULL);
	sInstance = this;

	// Initialize our member variables.
	mSystemIdleProc = inIdleProc;
	mDone = false;
	mExitedWithError = false;
	mScriptHasBegun = false;
	mLoadScriptFailed = false;
	ResetInitialCardName();
}

TInterpreterManager::~TInterpreterManager()
{
	sInstance = NULL;

	// Don't clear sHaveAlreadyCreatedSingleton--we promise that only
	// one TInterpreterManager can ever be created.
}

void TInterpreterManager::Run()
{
    // STACK MOVE WARNING - See lang/scheme/MZSCHEME-THREADS.txt for details.

	// WARNING - No Scheme function may ever be called above this
    // point on the stack!
    HALYARD_BEGIN_STACK_BASE();
    mIsInsideStackBase = true;

    InitialSetup();

	// Loop until somebody calls RequestQuitApplication, or we exit
	// because of an error (below).
	while (!mDone)
	{
		mShouldConsiderExiting = false;
		try
		{
			// Either create and run an interpreter, or just call the
			// idle procedure.
			if (mScriptHasBegun && !mLoadScriptFailed)
				LoadAndRunScript();
			else
				(*mSystemIdleProc)(true);
		}
		catch (std::exception &e)
		{
			gLog.Error("halyard", "%s.", e.what());
			mShouldConsiderExiting = true;
		}
		catch (...)
		{
			gLog.Fatal("halyard", "Unexpected internal error.");
		}

		// Handle any errors.
		if (mShouldConsiderExiting)
		{
            // Always quit for non-load errors, but only quit for load
            // errors if we're not in authoring mode.
			if (!mLoadScriptFailed ||
                (mLoadScriptFailed && !IsInAuthoringMode()))
			{
				mDone = true; 
                mExitedWithError = true;
			}
		}
	}

    mIsInsideStackBase = false;
    HALYARD_END_STACK_BASE();
}

void TInterpreterManager::DoIdle(bool block) {
    ASSERT(mSystemIdleProc);
    (*mSystemIdleProc)(block);
}

void TInterpreterManager::BeginScript()
{
	mScriptHasBegun = true;
}

void TInterpreterManager::LoadAndRunScript()
{
    // STACK MOVE WARNING - See lang/scheme/MZSCHEME-THREADS.txt for details.
    ASSERT(IsInsideStackBase());

    NotifyReloadScriptStarting();

    // This outer try/catch block makes sure that we dispose of
    // TInterpreter before we leave this function.
	try {
        // This inner try/catch block cleans up after failed script
        // reloads, and rethrows any exceptions.
        try {
            // Create an interpreter object, and load the scripts.
            MakeInterpreter();
        } catch (...) {
            // Tell our main loop that the interpreter object couldn't be
            // opened properly.
            mLoadScriptFailed = true;
            NotifyReloadScriptFailed();
            throw;
        }

        // Run the interpreter until it has finished.  The mDone flag
        // shouldn't be set at this point, because we haven't called
        // RunInitialCommands yet.
		ASSERT(!mDone);
        TInterpreter::GetInstance()->Run();

        // If any lazy loads have failed, they'll call LoadScriptFailed,
        // which will set mLoadScriptFailed and exit from Run.
        if (mLoadScriptFailed) {
            NotifyReloadScriptFailed();
            mShouldConsiderExiting = true;
        }

	} catch (...) {
        TInterpreter::DestroyInstance();
		throw;
	}
    TInterpreter::DestroyInstance();
}

void TInterpreterManager::RunInitialCommands()
{
    ASSERT(IsInsideStackBase());

    TInterpreter *interp = TInterpreter::GetInstance();

    // Let everybody know the script has been successfully loaded.
    NotifyReloadScriptSucceeded();
    
    // If we're in authoring mode, discard any initial command we may have
    // been passed on the command-line.
    if (IsInAuthoringMode())
        sHaveInitialCommand = false;

    // Run our initial command, if we have one.
    if (sHaveInitialCommand) {
        sHaveInitialCommand = false;

        // Note that Eval may cause a load failure if lazy loading is
        // enabled.  See below for how we want to deal with that.
        std::string result;
        if (!interp->Eval(sInitialCommand, result))
            THROW(result.c_str());
    } else {
        // Ask our interpreter to jump to the appropriate card.
        //
        // Note that either IsValidCard or JumpToCardByName may cause a
        // load failure if lazy loading is enabled.  Once that occurs, we
        // don't want to do anything, and we especially don't want to call
        // ResetInitialCardName.
        //
        // Note that the mLoadScriptFailed in the second half of this
        // conditional is needed to prevent resetting the card name after a
        // failed load.
        if (!interp->IsValidCard(mInitialCardName.c_str()) &&
            !mLoadScriptFailed)
            ResetInitialCardName();
        if (!mLoadScriptFailed)
            interp->JumpToCardByName(mInitialCardName.c_str());
    }

    // Reset any special variables.
    if (!mLoadScriptFailed)
        ResetInitialCardName();

    // Once we return from this function, the call to %kernel-check-state
    // inside call-prim will process the state that was set up by any calls to
    // JumpToCardByName or KillInterpreter during our initial command.
}

void TInterpreterManager::LoadScriptFailed() {
    mLoadScriptFailed = true;
    TInterpreter::GetInstance()->KillInterpreter();
}

void TInterpreterManager::RequestQuitApplication() {
    // Be really paranoid about our current state, because we may be called
    // from GUI.
	if (TInterpreter::HaveInstance()) {
        ASSERT(IsInsideStackBase());
        TInterpreter::GetInstance()->KillInterpreter();
    }
	mDone = true;
}

void TInterpreterManager::RequestReloadScript(const char *inGotoCardName)
{
    ASSERT(IsInsideStackBase());
	ASSERT(inGotoCardName != NULL);
	ASSERT(TInterpreter::HaveInstance());
	TInterpreter::GetInstance()->KillInterpreter();
	mInitialCardName = inGotoCardName;
    sIsFirstLoad = false;
}

bool TInterpreterManager::ScriptHasBegun() {
    return mScriptHasBegun;
}

bool TInterpreterManager::FailedToLoad()
{
	return mLoadScriptFailed;
}

void TInterpreterManager::RequestRetryLoadScript()
{
	ASSERT(FailedToLoad());

	// Turn off our load error flag, and Run will take care of the rest.
	mLoadScriptFailed = false;
}

bool TInterpreterManager::IsLazyLoadingEnabled() const {
    return IsInAuthoringMode() && mIsLazyLoadingRequested;
}

void TInterpreterManager::MaybeSetIsLazyLoadingEnabled(bool isEnabled) {
    mIsLazyLoadingRequested = isEnabled;
}

ScriptEditorDB *TInterpreterManager::GetScriptEditorDB() {
    if (HaveInstance())
        return GetInstance()->GetScriptEditorDBInternal();
    else
        return NULL;
}

void TInterpreterManager::AddReloadNotified(TReloadNotified *obj) {
    ASSERT(std::find(sReloadNotifiedObjects.begin(),
                     sReloadNotifiedObjects.end(),
                     obj) == sReloadNotifiedObjects.end());
    sReloadNotifiedObjects.push_back(obj);
}

void TInterpreterManager::RemoveReloadNotified(TReloadNotified *obj) {
    std::vector<TReloadNotified*>::iterator found =
        std::find(sReloadNotifiedObjects.begin(),
                  sReloadNotifiedObjects.end(),
                  obj);
    ASSERT(found != sReloadNotifiedObjects.end());

    sReloadNotifiedObjects.erase(found);
}

void TInterpreterManager::RegisterCachedConf(TInterpreterCachedConf *obj) {
    ASSERT(mCachedConf == NULL);
    mCachedConf = obj;
}

void TInterpreterManager::UnregisterCachedConf(TInterpreterCachedConf *obj) {
    ASSERT(mCachedConf == obj);
    mCachedConf = NULL;
}

void TInterpreterManager::NotifyReloadScriptStarting() {
    ASSERT(IsInsideStackBase());
    std::vector<TReloadNotified*>::iterator i = sReloadNotifiedObjects.begin();
    for (; i != sReloadNotifiedObjects.end(); ++i)
        (*i)->NotifyReloadScriptStarting();

    // We want to do this _after_ everyone else has cleared out their
    // TStateListener objects.
    gStateDB.Clear();
}

void TInterpreterManager::NotifyReloadScriptSucceeded() {
    ASSERT(IsInsideStackBase());
    std::vector<TReloadNotified*>::iterator i = sReloadNotifiedObjects.begin();
    for (; i != sReloadNotifiedObjects.end(); ++i)
        (*i)->NotifyReloadScriptSucceeded();
}

void TInterpreterManager::NotifyReloadScriptFailed() {
    ASSERT(IsInsideStackBase());
    std::vector<TReloadNotified*>::iterator i = sReloadNotifiedObjects.begin();
    for (; i != sReloadNotifiedObjects.end(); ++i)
        (*i)->NotifyReloadScriptFailed();

}

bool TInterpreterManager::ShouldSuppressSplashScreen() {
    // Current policy: Suppress splash screen after first load, so people
    // working in developer mode can see the old stage layout transform
    // directly into the new stage layout when reloading is done.  I'm told
    // this makes it easier to adjust alignments.  We also disable the
    // splash screen if lazy loading is enabled, because the progress bar
    // would be severely confused.
    return (!sIsFirstLoad ||
            TInterpreterManager::GetInstance()->IsLazyLoadingEnabled());
}

void TInterpreterManager::SetInitialCommand(const std::string &inCommand) {
    sHaveInitialCommand = true;
    sInitialCommand = inCommand;
}

