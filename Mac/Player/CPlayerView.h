//
// CPlayerView.h
//

#pragma once

#include "KHeader.h"

#include "CPicture.h"
#include "CCard.h"
#include "CGWorld.h"

#include <LMouseTracker.h>

// Video effects
enum FXType 
{
	kFXNone = 0,
	kFXWipeRight,
	kFXWipeLeft,
	kFXWipeUp,
	kFXWipeDown,
	kFXBlinds,
	kFXXFade,
	kFXPushRight,
	kFXPushLeft,
	kFXPushUp,
	kFXPushDown
};

class  CPlayerView : public LView, public LAttachment, public LMouseTracker
{
	public:
		enum { class_ID = FOUR_CHAR_CODE('PlVw') };
		
      					CPlayerView(LStream *inStream);
		static  CPlayerView *CreatePlayerViewStream(LStream *inStream);
						~CPlayerView(void);
						
		void			ColorCard(int16 color);
		void			SetBackPic(KString &picName, Rect inRect);
		
		void			KillScript(void);
       	void			SpendTime(const EventRecord &inMacEvent);
		
		void			Activate(void);
		void 			Deactivate(void);
		
		void			CenterInView();
		void			ReDraw(void);
		void			DrawSelf(void);	
		void			ValidFrame(void);
		//void			Refresh(void);
		
		void 			AddKeyBinding(const char inKeyChar, CCard *inCardToJumpTo);

		void			Blippo(void);
		void			UnBlippo(const FXType inEffect = kFXNone, const int8 inTime = 0);
		void			Lock(const bool inClear = false);
		void			UnLock(const FXType inEffect = kFXNone, const int8 inTime = 0);
		void			Micro(const FXType inEffect = kFXNone);
		
		// The following function returns true if we have bits available to copy.
		bool			BlippoAvailable(void) { return(mHasBlip); }
		bool			IsLocked(void)	{ return(mIsLocked); }

		void			DoNewPalette(CTabHandle inCTabHand);
		
		void			AdjustCursor(Point inPortPt, const EventRecord &inMacEvent);
		void			AdjustMyCursor(void);
#ifdef DEBUG
		void			ShowTZones(void);
		void			KillTZones(void) { DeleteAllSubPanes(); }
#endif
		void			CTouch(void);
		void			CTouch(int16 inLeft, int16 inTop);
				
		CGWorld			*GetGWorld(void) { return(mGWorld); }
		
		uint16			ProcessEvents(bool inEvents);
		void			RestoreEvents(uint16 inEvents);
		void			ProcessTZones(bool inTZones);
		bool			ProcessingEvents(void) { return (mProcessKeys and mProcessTZones); }
		bool			ProcessingKeys(void) { return (mProcessKeys); }
		bool			ProcessingTZones(void) { return (mProcessTZones); }
		void			DoPause(bool inFromKey);
		void			DoResume(bool inFromKey);
		void			DoResetPause(void);
		void			ShowMouse(bool inShowIt);
		
		void			MouseEnter(Point inPortPt, const EventRecord &/* inMacEvent */);
		void			MouseLeave(Point inPortPt, const EventRecord &/* inMacEvent */);
		void			MouseWithin(Point inPortPt, const EventRecord &/* inMacEvent */);
		
	protected:
			
		CGWorld			*mGWorld;
		CGWorld			*mBlippoWorld;
#ifdef DEBUG_5L_LATER
		CGWorld			*mFadeWorld;
#endif
		
		void			ExecuteSelf(MessageT /* inMessage */, void *ioParam);
		
	private:
		bool 			DoKeyBind(const char inKey);

		void 			DoEffect(const FXType inEffect, const int8 inTime);
		void 			DoSlide(const FXType inEffect, const int8 inTime);
		void 			DoWipe(const FXType inEffect, const int8 inTime);
		void 			DoBlinds(const int8 inTime);
		void 			DoXFade(const int8 inTime);
		bool			OverTZone(void);
		
		bool			mActive;
		bool			mIsLocked;
		bool			mHasBlip;
		bool			mProcessKeys;
		bool			mProcessTZones;
		
		int16			mBackColor;
		CPicture		*mBackPic;
		LArray			*mKeyBinds;
		Point			mCursorPos;
		bool			mPauseFromKey;
		bool			mMoviePaused;
};

// sCardKey
// mKeyBinds list element which contains the control caracter and the name of
// the card to jump to when 'alt-<char>' is pressed.

struct sCardKey {
	char		mTheChar;
	KString		*mCardName;
//	CCard		*mCard;
};

// Get the effect from a string
FXType StringToEffect(KString inText);

// Global pointer to the card view.
extern CPlayerView *gPlayerView;

// Some default RGBcolors
const RGBColor kRGB_Black = {0, 0, 0};
const RGBColor kRGB_White = {65535, 65535, 65535};
const RGBColor kRGB_Blue = {0, 0, 61132};

