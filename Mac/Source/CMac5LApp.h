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
		// Open a script file.  Scripts are represented without a directory or extension.
		// For example ":Scripts:sample.scr" would be represented as "sample".  Return
		// true if no errors occur.
		//
		bool				OpenScript(const char *inScriptName,
									   const char *inCardName);

#ifdef DEBUG
		void				ReDoScript(const char *curCard);
		void				ReDoReDoScript(void);

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
		TString				mReDoCard;		// card to jump to when redo redoscript
		bool				mReDoReDo;		// waiting for a redo redoscript?
	
		bool				GetScriptFile(FSSpec *scriptSpec);
		
		static void			EmergencyUnfade();
};

// global application object
extern CMac5LApp *gTheApp;

extern Handle clickSound;
extern WindowPtr gWindow;
extern int FiveLmain();

END_NAMESPACE_FIVEL
