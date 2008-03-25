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

#include "TInterpreter.h"
#include "TDeveloperPrefs.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"

USING_NAMESPACE_FIVEL

//=========================================================================
//  TInterpreter Methods
//=========================================================================

TInterpreter *TInterpreter::sInstance = NULL;

TInterpreter::TInterpreter()
    : mSourceFilesLoaded(0), mSourceFilesExpected(0)

{
    ASSERT(sInstance == NULL);
    sInstance = this;

    // If we have a document (i.e., we're not running the test suites),
    // then load in the expected number of source files.
    Document *doc = TInterpreterManager::GetInstance()->GetDocument();
    if (doc)
        mSourceFilesExpected = doc->GetTamaleProgram()->GetSourceFileCount();
}

TInterpreter::~TInterpreter()
{
    sInstance = NULL;
}

void TInterpreter::NotifyFileLoaded() {
    // For now, suppress calculation of splash-screen related data when
    // we're not showing the splash screen.
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;

    ++mSourceFilesLoaded;
    if (mSourceFilesLoaded > mSourceFilesExpected)
        mSourceFilesExpected = mSourceFilesLoaded;
}

void TInterpreter::NotifyScriptLoaded() {
    // For now, suppress calculation of splash-screen related data when
    // we're not showing the splash screen.
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;

    mSourceFilesExpected = mSourceFilesLoaded;

    // If we have a document (i.e., we're not running the test suites),
    // and we're not in runtime mode, then update our source file count.
    Document *doc = TInterpreterManager::GetInstance()->GetDocument();
    if (doc && !TInterpreterManager::GetInstance()->IsInRuntimeMode())
        doc->GetTamaleProgram()->SetSourceFileCount(mSourceFilesExpected);
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
//  TInterpreterManager Methods
//=========================================================================
//  This class contains a fairly odd state machine, which is used to
//  manage the lifetime of TInterpreter objects and run the main event
//  loop.

TInterpreterManager *TInterpreterManager::sInstance = NULL;
bool TInterpreterManager::sHaveAlreadyCreatedSingleton = false;
std::vector<TReloadNotified*> TInterpreterManager::sReloadNotifiedObjects;
bool TInterpreterManager::sIsInRuntimeMode = false;
bool TInterpreterManager::sIsFirstLoad = true;
bool TInterpreterManager::sHaveInitialCommand = false;
std::string TInterpreterManager::sInitialCommand;
Document *TInterpreterManager::sDocument = NULL;


TInterpreterManager::TInterpreterManager(
	TInterpreter::SystemIdleProc inIdleProc)
{
	ASSERT(sHaveAlreadyCreatedSingleton == false);
	sHaveAlreadyCreatedSingleton = true;
	ASSERT(sInstance == NULL);
	sInstance = this;

	// Initialize our member variables.
	mSystemIdleProc = inIdleProc;
	mInterpreter = NULL;
	mDone = false;
	mExitedWithError = false;
	mScriptIsBegun = false;
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
	// Loop until somebody calls RequestQuitApplication, or we exit
	// because of an error (below).
	while (!mDone)
	{
		bool caught_error = false;
		try
		{
			// Either create and run an interpreter, or just call the
			// idle procedure.
			if (mScriptIsBegun && !mLoadScriptFailed)
				LoadAndRunScript();
			else
				(*mSystemIdleProc)(true);
		}
		catch (std::exception &e)
		{
			gLog.Error("Internal error: %s.", e.what());
			caught_error = true;
		}
		catch (...)
		{
			gLog.FatalError("Unexpected internal error.");
		}

		// Handle any errors.
		if (caught_error)
		{
            // Always quit for non-load errors, but only quit for load
            // errors if we're in runtime mode.
			if (!mLoadScriptFailed || (mLoadScriptFailed && IsInRuntimeMode()))
			{
				mDone = true; 
                mExitedWithError = true;
			}
		}
	}
}

void TInterpreterManager::BeginScript()
{
	mScriptIsBegun = true;
}

void TInterpreterManager::LoadAndRunScript()
{
    NotifyReloadScriptStarting();
	try
	{
		// Create an interpreter object, and ask it to jump to the
		// appropriate card.
		mInterpreter = MakeInterpreter();
        NotifyReloadScriptSucceeded();

        // Run our initial command, if we have one.
        if (sHaveInitialCommand) {
            sHaveInitialCommand = false;
            if (sIsInRuntimeMode) {
                std::string result;
                if (!mInterpreter->Eval(sInitialCommand, result))
                    THROW(result.c_str());
            }
        }

		// Ask our interpreter to jump to the appropriate card.
        if (!mInterpreter->IsValidCard(mInitialCardName.c_str()))
            ResetInitialCardName();
        mInterpreter->JumpToCardByName(mInitialCardName.c_str());

		// Reset any special variables.
		ResetInitialCardName();
	}
	catch (...)
	{
		// Tell our main loop that the interpreter object couldn't be
		// opened properly.
		mLoadScriptFailed = true;
		throw;
	}
	
	// Run the interpreter until it has finished.
	try
	{
		// The mDone flag may have been set by mInitialCommand code.
		if (!mDone)
			mInterpreter->Run(mSystemIdleProc);
	}
	catch (...)
	{
		delete mInterpreter;
		mInterpreter = NULL;
		throw;
	}
	delete mInterpreter;
	mInterpreter = NULL;
}

void TInterpreterManager::RequestQuitApplication()
{
	if (mInterpreter)
		mInterpreter->KillInterpreter();
	mDone = true;
}

void TInterpreterManager::RequestReloadScript(const char *inGotoCardName)
{
	ASSERT(inGotoCardName != NULL);
	ASSERT(mInterpreter);
	mInterpreter->KillInterpreter();
	mInitialCardName = inGotoCardName;
    sIsFirstLoad = false;
}

bool TInterpreterManager::InterpreterHasBegun() {
    return mScriptIsBegun;
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

void TInterpreterManager::NotifyReloadScriptStarting() {
    std::vector<TReloadNotified*>::iterator i = sReloadNotifiedObjects.begin();
    for (; i != sReloadNotifiedObjects.end(); ++i)
        (*i)->NotifyReloadScriptStarting();
}

void TInterpreterManager::NotifyReloadScriptSucceeded() {
    std::vector<TReloadNotified*>::iterator i = sReloadNotifiedObjects.begin();
    for (; i != sReloadNotifiedObjects.end(); ++i)
        (*i)->NotifyReloadScriptSucceeded();
}

void TInterpreterManager::SetRuntimeMode(bool inIsInRuntimeMode) {
    sIsInRuntimeMode = inIsInRuntimeMode;
}

bool TInterpreterManager::IsInRuntimeMode() {
    return sIsInRuntimeMode;
}

bool TInterpreterManager::ShouldSuppressSplashScreen() {
    // Current policy: Suppress splash screen after first load, so people
    // working in developer mode can see the old stage layout transform
    // directly into the new stage layout when reloading is done.  I'm told
    // this makes it easier to adjust alignments.
    return !sIsFirstLoad;
}

void TInterpreterManager::SetInitialCommand(const std::string &inCommand) {
    sHaveInitialCommand = true;
    sInitialCommand = inCommand;
}

