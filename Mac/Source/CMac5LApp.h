// -*- Mode: C++; tab-width: 4; -*-
// ===========================================================================
//	CMac5LApp.h					©1994 Metrowerks Inc. All rights reserved.
// ===========================================================================

#pragma once

#include "THeader.h"
#include "TLogger.h"
#include "CPalette.h"

BEGIN_NAMESPACE_FIVEL

class	CBackWindow;

class	CMac5LApp : public PP::LApplication
{

	public:
							CMac5LApp();
		virtual 			~CMac5LApp();
	
		void				QuitScript(void);
		void				StartUp(void);

		//////////
		// Set up the interpreter and start it running.
		//
		void				InitializeInterpreter();

#ifdef DEBUG
		void				ReDoScript(const char *curCard);

		Boolean				ObeyCommand(PP::CommandT inCommand, void *ioParam);
		void				FindCommandStatus(
								PP::CommandT 		inCommand,
								Boolean&			outEnabled,
								Boolean&			outUsesMark,
								UInt16&				outMark,
								Str255				outName);
#endif
		void				CheckMemory(void);
		void				MaxMemory(void);
		void				DumpMemory(void);
				
		Boolean				AttemptQuitSelf(SInt32 inSaveOption);
		virtual void		HandleAppleEvent(
								const AppleEvent	&inAppleEvent,
								AppleEvent			&outAEReply,
								AEDesc				&outResult,
								SInt32				inAENumber);
		
		void				DoExit(int16 inSide);
		
		virtual void		EventResume		(const EventRecord& inMacEvent);
		virtual void		EventSuspend	(const EventRecord& inMacEvent);							
		//////////
		// A SpecialVariableFunction to get the _system variable.
		//
		static TString		ReadSpecialVariable_system() { return "MacOS"; }
	
		//////////
		// A SpecialVariableFunction to get the _curcard variable.
		//
		static TString	ReadSpecialVariable_curcard();
		
		//////////
		// A SpecialVariableFunction to get the _prevcard variable.
		//
		static TString	ReadSpecialVariable_prevcard();

	protected:
		CBackWindow			*mDisplayWindow;

		void                PrepareToRun();
		void                ProcessEvents();
		static void			MacIdleProc();
		
		void				CleanUp(void);
		void				SetGlobals(void);
		void				DoAEOpenDoc(
								const AppleEvent 	&inAppleEvent,
								AppleEvent			&outARReply,
								long 				inAENumber);
		
	private:
		Rect				mScreenRect;
		int					mBitDepth;
		bool				mScriptRunning;
//		bool				mHaveNewPal;
//		CPalette			*mGraphicsPal;
//		CPalette			*mMoviePal;
//		PaletteHandle		mCurPal;
	
		bool				GetScriptFile(FSSpec *scriptSpec);
		
		static void			EmergencyUnfade();
};

// global application object
extern CMac5LApp *gTheApp;

extern Handle clickSound;
extern WindowPtr gWindow;
extern int FiveLmain();

END_NAMESPACE_FIVEL
