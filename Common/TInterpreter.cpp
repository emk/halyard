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

#include "TInterpreter.h"
#include "TDeveloperPrefs.h"

USING_NAMESPACE_FIVEL

//=========================================================================
//  TInterpreter Methods
//=========================================================================

TInterpreter *TInterpreter::sInstance = NULL;

TInterpreter::TInterpreter()
{
    ASSERT(sInstance == NULL);
    sInstance = this;
}

TInterpreter::~TInterpreter()
{
    sInstance = NULL;
}


//=========================================================================
//  TInterpreterManager Methods
//=========================================================================
//  This class contains a fairly odd state machine, which is used to
//  manage the lifetime of TInterpreter objects and run the main event
//  loop.

TInterpreterManager *TInterpreterManager::sInstance = NULL;
bool TInterpreterManager::sHaveAlreadyCreatedSingleton = false;

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
			gLog.Error("Unexpected internal error.");
			caught_error = true;
		}

		// Handle any errors.
		if (caught_error)
		{
			if (!mLoadScriptFailed)
			{
				// Always quit for non-load errors.
				mDone = true; 
			}
			else if (mLoadScriptFailed &&
					 gDeveloperPrefs.GetPref(REDOSCRIPT) == REDOSCRIPT_OFF)
			{
				// Only quit for load errors if we don't have "redoscript".
				mDone = true;
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
	try
	{
		// Create an interpreter object, and ask it to jump to the
		// appropriate card.
		mInterpreter = MakeInterpreter();
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


