//
// CPlayerView.cp
//

#include "KHeader.h"

#include <ctype.h>

#include "KLogger.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CCard.h"
#include "CPlayerView.h"
#include "CPlayerInput.h"
#include "CMoviePlayer.h"
#include "CPicture.h"
#include "CMenuUtil.h"
#include "CVariable.h"
#include "CMenuUtil.h"
#include "CPalette.h"
#include "gamma.h"
//#include "CGWorld.h"
//#include <UGworld.h>
#include <OSUtils.h>

#ifdef DEBUG
#include "CModule.h"
#endif
#include "CTouchZone.h"

#ifdef DEBUG
bool gShowTZones = false;
extern WindowPtr gWindow;
extern bool gInFront;
#endif

// externals
extern bool 		gFadedIn;
extern bool			gHideMenuBar;

// 
//	CPlayerView - create a CPlayerView pane from a PPob resource
//
CPlayerView* CPlayerView::CreatePlayerViewStream(LStream *inStream)
{
   return (new CPlayerView(inStream));
}

//
//	CPlayerView - the construct-from-stream constructor
//
CPlayerView::CPlayerView(LStream  *inStream) : LView(inStream), LAttachment(msg_Event, true)
{
	Rect	frame;
	
	CalcLocalFrameRect(frame);
	
	// Allocate the offscreen gworlds
	mGWorld = mBlippoWorld = nil;
	mGWorld = new CGWorld(frame);
	mBlippoWorld = new CGWorld(frame);
#ifdef DEBUG_5L_LATER
	mFadeWorld = new GCWorld(frame);
#endif

	// Get the window's palette
	PaletteHandle thePalette = ::GetNewPalette(128);
	
	// Set the offscreen palette. Don't bother setting the blippo's palette
	// until we actually do it, as it will likely have changed.	
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();
	::SetPalette((GrafPort *) theMacGWorld, thePalette, true);
	
#ifdef DEBUG_5L_LATER
	theMacGWorld = mFadeWorld->GetMacGWorld();
	::SetPalette((GrafPort *) theMacGWorld, thePalette, true);
#endif

	// make sure our primary drawing offscreen world is filled with black
	mGWorld->BeginDrawing();
	::RGBForeColor(&Color_Black);
	::PenMode(patCopy);
	::PaintRect(&frame);
	mGWorld->EndDrawing();
				
	mBackPic 	= nil;
	mBackColor 	= 1;
	mActive 	= false;
	mIsLocked 	= false;
	mHasBlip	= false;
	mProcessKeys = false;
	mProcessTZones = false;
	mPauseFromKey = false;
	mMoviePaused = false;

	// Init the list of style elements
	mKeyBinds = new LArray(sizeof(sCardKey));

	FinishCreate();
}

//
//	~CPlayerView
//
CPlayerView::~CPlayerView()
{
	if (mGWorld != nil)
		delete mGWorld;
		
	if (mBlippoWorld != nil)
		delete mBlippoWorld;
}

// 
//	CenterInView - Center ourselves in the window.
//
void  CPlayerView::CenterInView(void)
{
	LView	*superView;
	Rect	superRect, localRect;
	int32	diff;
	int16	dh, dv;
	
	superView = GetSuperView();
	Assert_(superView != nil);
	superView->CalcLocalFrameRect(superRect);
	
	CalcLocalFrameRect(localRect);
	
	diff = ::DeltaPoint(botRight(superRect), botRight(localRect));
	dh = LoWord(diff);
	dv = HiWord(diff);
	
	MoveBy(dh/2, dv/2, false);
}

//
//	Activate - 
//
void CPlayerView::Activate(void)
{
	if (not mActive)
	{
		LView::Activate();

		if (gHideMenuBar)
			gMenuUtil.HideMenuBar();
					
		mActive = true;
		mPauseFromKey = false;
		mMoviePaused = false;
		
		LPeriodical::StartRepeating();
		AdjustMyCursor();
	}
}

//
//	Deactivate - 
//
void CPlayerView::Deactivate(void)
{
	if (mActive)
	{
		LView::Deactivate();
		
		mActive = false;
		
		ProcessEvents(false);
		
		LPeriodical::StopRepeating();	// cbo_fix - do we want to continue card execution in background??
		//::ShowCursor();		// always show the cursor
	}
}

//
//	SpendTime - Processing time, pass it along to the current card.
//
void CPlayerView::SpendTime(const EventRecord &inMacEvent)
{
	if (mActive)
	{
		gCardManager.CurCardSpendTime();
		
		gMovieManager.SpendTime(inMacEvent);
	}
	
	AdjustMyCursor();
}

//
//	ProcessEvents - turn all event processing on or off
//
uint16 CPlayerView::ProcessEvents(bool inEvents)
{
	uint16	prevEvents = 0;
	
	prevEvents = mProcessTZones;
	prevEvents <<= 8;
	prevEvents |= mProcessKeys;
		
	if (inEvents)
	{
		mProcessTZones = true;
		mProcessKeys = true;
	}
	else
	{
		mProcessTZones = false;
		mProcessKeys = false;
	}
	
	return (prevEvents);
}

void CPlayerView::RestoreEvents(uint16 inEvents)
{
	mProcessKeys = (bool) (inEvents & 0xFF);
	inEvents >>= 8;
	mProcessTZones = inEvents;
}

void CPlayerView::ProcessTZones(bool inTZones)
{
	mProcessTZones = inTZones;

	AdjustMyCursor();
}

void CPlayerView::DoPause(bool inFromKey)
{
	if (not mMoviePaused)
	{
		if (gMovieManager.Playing())
		{
			gMovieManager.Pause();
#ifdef DEBUG
		//	gDebugLog.Log("pausing the movie");
#endif			
			mPauseFromKey = inFromKey;
			mMoviePaused = true;
			
			if (inFromKey)
				ProcessTZones(false);	// turn off touch zone processing
		}
#ifdef DEBUG
		//else
		//	gDebugLog.Log("nothing to pause");
#endif
	}
#ifdef DEBUG
	//else
	//	gDebugLog.Log("movie is already paused");
#endif
}

void CPlayerView::DoResume(bool /* inFromKey */)
{
	if (mMoviePaused)
	{
		mMoviePaused = false;
		
		if (gMovieManager.Paused())
		{
			if (mPauseFromKey)
				ProcessTZones(true);
			
			mPauseFromKey = false;
			
			gMovieManager.Resume();
		}
	}
}

void CPlayerView::DoResetPause(void)
{
	mMoviePaused = false;
}

//
//	ReDraw
//
void CPlayerView::ReDraw(void)
{
	Refresh();
}

//
//	ValidFrame
//
void CPlayerView::ValidFrame(void)
{
	Rect	theFrame;
	
	CalcLocalFrameRect(theFrame);
	ValidPortRect(&theFrame);
}

#ifdef BAD_IDEA
//
//	Refresh - Refresh the screen.
//
void CPlayerView::Refresh(void)
{
	Rect		theFrame;
	bool		did_fade = false;
	
	CalcLocalFrameRect(theFrame);
	
	if ((theConfig->GetBitDepth() <= 8) and (gPaletteManager.HaveNewPal()))
	{
		did_fade = true;
		DoGFade(false, 0, false);
		//::FillRect(&theFrame, &qd.black);
		//::ValidRect(&theFrame);
		
		gPaletteManager.CheckPalette();
	}
	
	mGWorld->CopyImage(GetMacPort(), theFrame);
	
	if (did_fade)
		DoGFade(true, 0, false);	
}
#endif

//
//	DrawSelf - Draw the correct background
//
void  CPlayerView::DrawSelf(void)
{
	Rect  	theFrame;
	int32	subPanes = 0;
	StColorPenState savePenState;

	gPaletteManager.CheckPalette();
	
	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(theFrame);

	if (not mActive)
		return;
	
	if (false)
	// cbo_fix - try this for now		
	//if (gMovieManager.FullScreenPlay())
	{
		::RGBForeColor(&Color_Black);
		::PenMode(patCopy);
		::PaintRect(&theFrame);
		::ValidRect(&theFrame);					// cbo_fix - this is necessary to keep from seeing the 
		// black background when a movie starts up - is there another way to do this?
	}
	else
	{
		// Only draw if we haven't locked the bitmap.
		if (not mIsLocked)
		{
			// Draw the offscreen world
			mGWorld->CopyImage(GetMacPort(), theFrame);
		}
	}
	
	// make sure the movie draws correctly
	if (gMovieManager.MoviePlaying())
		gMovieManager.DrawSelf();
}

//
// Sets the background picture of a card. Pass 'nil' to delete the current one
//
void  CPlayerView::SetBackPic(KString &picName, Rect inRect)
{
	RgnHandle	theClip = nil;
	KPoint		thePt(0, 0);
	//Rect		theFrame;
	KRect		clipRect;
	
	if (picName == "")
		mBackPic = nil;
	else
	{
		mBackPic = gPictureManager.GetPicture(picName);
		//CalcLocalFrameRect(theFrame);
		
		clipRect.Set(inRect);
		
		mGWorld->BeginDrawing();
		
		//theClip = ::NewRgn();
		//::GetClip(theClip);
		//::ClipRect(&inRect);				// clip to the rect that was passed in
		
		mBackPic->Draw(thePt, mGWorld->GetMacGWorld(), clipRect);
		
		//::SetClip(theClip);
		//::DisposeRgn(theClip);				
		
		mGWorld->EndDrawing();
		
		mBackPic->Purge();				// free up memory for the picture
	}
}

//
//	ColorCard
//
void CPlayerView::ColorCard(int16 color)
{
	Rect  theFrame;
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();
	
	mBackColor = color;	

	mGWorld->BeginDrawing();
	CalcLocalFrameRect(theFrame);
	::PmForeColor(mBackColor);
	::PenMode(patCopy);
	::PaintRect(&theFrame);
	mGWorld->EndDrawing();
}


//
//	DoNewPalette
//
void CPlayerView::DoNewPalette(CTabHandle inCTab)
{
	PaletteHandle	thePalHand;
	PaletteHandle	theOldPalHand;
	GWorldPtr		theMacGWorld;
	Rect			theFrame;

	theMacGWorld = mGWorld->GetMacGWorld();
	theOldPalHand = ::GetPalette((GrafPort *) theMacGWorld);

	CalcLocalFrameRect(theFrame);

	(*inCTab)->ctFlags |= 0x4000;			// so will work with offscreen world
	
	// now make the new palette
	thePalHand = ::NewPalette(256, inCTab,  pmCourteous, 0);
	
	// update the gworld with info about the new color table
	::UpdateGWorld(&theMacGWorld, 0, &theFrame, inCTab, nil, 0);
	mGWorld->SetMacGWorld(theMacGWorld);
	
	// update the blippo buffer too!!
	GWorldPtr	theBlippoWorld = mBlippoWorld->GetMacGWorld();
	::UpdateGWorld(&theBlippoWorld, 0, &theFrame, inCTab, nil, 0);
	mBlippoWorld->SetMacGWorld(theBlippoWorld);
	
	::NSetPalette((GrafPort *) theMacGWorld, thePalHand, pmAllUpdates);
	::ActivatePalette((GrafPort *) theMacGWorld);
	
	if (theOldPalHand != nil)
		::DisposePalette(theOldPalHand);
	
#ifdef DEBUG_5L_LATER
	// do it for our fade world
	theMacGWorld = mFadeWorld->GetMacGWorld();
	theOldPalHand = ::GetPalette((GrafPort *) theMacGWorld);
	
	thePalHand = ::NewPalette(256, inCTab, pmCourteous, 0);
	::UpdateGWorld(&theMacGWorld, 0, &theFrame, inCTab, nil, 0);
	mFadeWorld->SetMacGWorld(theMacGWorld);
	
	::NSetPalette((GrafPort *) theMacGWorld, thePalHand, pmAllUpdates);
	
	if (theOldPalHand != nil)
		::DisposePalette(theOldPalHand);
		
	mFadeWorld->BeginDrawing();
	//::PmForeColor(0);
	::RGBForeColor(&Color_Black);
	::PenPat(patCopy);
	::PaintRect(&theFrame);
	mFadeWorld->EndDrawing();
#endif
}

//
//	AdjustCursor - we don't change the cursor for any of our panes (yet).
//
void
CPlayerView::AdjustCursor(
	Point				inPortPt,
	const EventRecord	&/* inMacEvent */)
{
	Point	localPoint = inPortPt;
	
	PortToLocalPoint(localPoint);
	gCursorManager.CheckCursor(localPoint);
}

void 
CPlayerView::MouseEnter(Point inPortPt, const EventRecord &/* inMacEvent */)
{
	Point	localPoint = inPortPt;
	
	PortToLocalPoint(localPoint);
	gCursorManager.SetCursorPos(localPoint);
}

void 
CPlayerView::MouseLeave(Point inPortPt, const EventRecord &/* inMacEvent */)
{
	Point	localPoint = inPortPt;
	
	PortToLocalPoint(localPoint);
	gCursorManager.SetCursorPos(localPoint);
}

void
CPlayerView::MouseWithin(Point inPortPt, const EventRecord &/* inMacEvent */)
{
	Point	localPoint = inPortPt;
	
	PortToLocalPoint(localPoint);
	gCursorManager.SetCursorPos(localPoint);
}

//
//	AdjustMyCursor
//
void CPlayerView::AdjustMyCursor(void)
{
	gCursorManager.CheckCursor();
}

#ifdef DEBUG
//
//	ShowTZones - Outline all TZones in red. Do it directly to the screen so
//			we don't mess up the offscreen world.
//
void CPlayerView::ShowTZones(void)
{
	LArray			&paneList = GetSubPanes();
	LArrayIterator	iterator(paneList, LArrayIterator::from_Start);
	CTouchZone		*theButt;
	
	StColorPenState::Normalize();
	
	while (iterator.Next(&theButt))
	{
		Rect	frameRect;
		
		theButt->CalcLocalFrameRect(frameRect);
		
		::PmForeColor(253);		
		::FrameRect(&frameRect);
	}

	StColorPenState::Normalize();
	
}
#endif

	
//
//	CTouch - Clear the first touch zones that matches the input parameters. 
//
void CPlayerView::CTouch(int16 inLeft, int16 inTop)
{
	LArray			&paneList = GetSubPanes();
	LArrayIterator	iterator(paneList, LArrayIterator::from_Start);
	CTouchZone		*theButt;
	bool			done = false;
	
	while ((not done) and (iterator.Next(&theButt)))
	{
		Rect	frameRect;
		
		theButt->CalcLocalFrameRect(frameRect);
		
		// delete the first touch zone that you come to that has
		// the left and top postions given
		if ((frameRect.left == inLeft) and (frameRect.top == inTop))
		{
			// cbo_fix - this doesn't work ???
//			RemoveSubPane(theButt);
			//theButt->PutInside(nil);		// to remove from our list of panes
			//delete theButt;
			
			// cbo_fix - as a hack, move the button far, far away
			theButt->MoveBy(1000, 1000, false);
			
			done = true;
		}
	}
}

//
//	CTouch - Clear all touch zones.
//
void CPlayerView::CTouch(void)
{
	DeleteAllSubPanes();
}

//
//	ShowMouse - 
//
void CPlayerView::ShowMouse(bool inShowIt)
{
	if (inShowIt)
		gCursorManager.ShowCursor();
	else
		gCursorManager.HideCursor();
}				

//
//	KillScript
//
void CPlayerView::KillScript(void)
{
	//Rect  	theFrame;
	
	mPauseFromKey = false;
	mMoviePaused = false;

	gCardManager.CurCardKill();
	
	if (gMovieManager.Playing())
		gMovieManager.Kill();
	
	DeleteAllSubPanes();
	
	delete mKeyBinds;							// kill keybinds
	mKeyBinds = new LArray(sizeof(sCardKey));	// allocate a new list
	
	// cbo - if we do this, we won't be able to show load screens Deactivate();
	//Refresh();				// to make the screen draw in black
}

//
//	ExecuteSelf - To get events in the attachment chain.
//
void CPlayerView::ExecuteSelf(MessageT /* inMessage */, void *ioParam)
{
	Handle			kchr_rsrc;								/* handle to KCHR resource */
	uint32			key_trans_state;						/* value set by KeyTrans() */
	uint8			key;									/* ASCII value of key pressed */
	int16			key_code;								/* argument to pass to KeyTrans() */
	EventRecord 	*theEvent;
	char			theChar;
	Boolean			keyHandled = false;

	
	theEvent = (EventRecord *) ioParam;
	
	if ((theEvent->what == keyDown) or (theEvent->what == autoKey))
	{
		if (mPauseFromKey)
		{
			DoResume(true);
			keyHandled = true;
		}
		else if ((theEvent->modifiers & cmdKey) or (theEvent->modifiers & optionKey))
		{
			// If the Option key is down, we have to get the real key pressed.
			if ((theEvent->modifiers & optionKey) != 0)
			{
				key_code = (((theEvent->message & keyCodeMask) >> 8) & 0xFF);	/* pick up virtual key code. see IM I-250 */
				kchr_rsrc = ::RGetResource('KCHR', 0);							/* get KCHR resource from ROM */
			
				if (kchr_rsrc != NULL)
				{
					::HLock(kchr_rsrc);
					key_trans_state = 0;										/* see Inside Mac V-195 */
					key = (uint8) (::KeyTranslate((Ptr) *kchr_rsrc, key_code, &key_trans_state) & 0xFF); /* IM says to use low 8 bits of hi-word (i.e., ASCII 1), but it really needs to be ASCII 2 */
					::HUnlock(kchr_rsrc);
					::ReleaseResource(kchr_rsrc);								/* release 'KCHR' resource */
					
					theChar = key;
				}
				else
					theChar = 0;
			}
			else
				theChar = (theEvent->message & charCodeMask);
			
			theChar = tolower(theChar);
			
			// first process Cmd (or Option) . (period) - this will kill naps, audio and video
			if (theChar == '.')
			{
				if (gMovieManager.Playing())
					gMovieManager.Kill();
					
				if (gCardManager.CurCardNapping())
					gCardManager.CurCardWakeUp();
#ifdef DEBUG
				else if ((gModMan->NoVolume()) and (gCardManager.CurCardPaused()))
					gCardManager.CurCardWakeUp();
#endif
								
				keyHandled = true;			// we always swallow this one
			}
			// first give a chance for the keybinds to claim the key, if they don't 
			// then do default actions for certain keys
			else if (not DoKeyBind(theChar))
			{
				switch (theChar)
				{
					// -------------------------------------------------
					//	STANDARD CHARS
					// -------------------------------------------------
					case 'p':							// p -> pause the movie
						DoPause(true);
						
						keyHandled = true;
						break;
					case 'q':							// q -> quit
						gTheApp->DoExit(0);
						
						keyHandled = true;
						break;
					case 'f':							// f -> fast forward the movie or wake up from nap
						if (gMovieManager.Playing())
							gMovieManager.Kill();
						else if (gCardManager.CurCardNapping())
							gCardManager.CurCardWakeUp();
							
						keyHandled = true;
						break;
#ifdef DEBUG
					case '=':
						if (gShowTZones)
						{
							gShowTZones = false;
							Draw(nil);
						}
						else
						{
							ShowTZones();
							gShowTZones = true;
						}
						break;
					case '-':
						if (gInFront)
						{
							gInFront = false;
							::SendBehind(gWindow, nil);
						}
						else
						{
							gInFront = true;
							::BringToFront(gWindow);
						}
						break;
					case '+':
						DoGFade(true, 0);
						break;
#endif
				}
			}
			else
				keyHandled = true;		// keybind did it
		}
		else
		{
			theChar = (theEvent->message & charCodeMask);
			if (theChar == 0x1B)					// Escape key
			{
				// first give a chance for a keybind to do something with it
				if (not DoKeyBind(theChar))
				{
					if (gMovieManager.Playing())
						gMovieManager.Kill();
					
					if (gCardManager.CurCardNapping())
						gCardManager.CurCardWakeUp();
#ifdef DEBUG
					else if ((gModMan->NoVolume()) and (gCardManager.CurCardPaused()))
						gCardManager.CurCardWakeUp();
#endif
				}
								
				keyHandled = true;			// we always handle the escape key
			}
			else if (not HaveInputUp())
			{
				// see if this is an accelerator for a touch zone
				LArray			&paneList = GetSubPanes();
				LArrayIterator	iterator(paneList, LArrayIterator::from_Start);
				CTouchZone		*theButt;
				CTouchZone		*theButtOnly = nil;
				int16			numButts = 0;
				bool			done = false;
				
				// if there is only one buttpcx, enter works to hit it
				while ((iterator.Next(&theButt)) and (not done))
				{
					if (theButt->IsButtPcx())
					{
						numButts++;
						theButtOnly = theButt;
					}
					
					// see if the key is the same as the first char of text
					if (tolower(theChar) == tolower(theButt->FirstChar()))
					{
						theButt->HotSpotAction(-1, true, false);
						Draw(nil);
						theButt->HotSpotResult(-1);
						done = true;
					}
				} 
				
				if ((not done) and (numButts == 1) and (theButtOnly != nil)
				and ((theChar == NEWLINE_CHAR) or (theChar == RETURN_CHAR)))
				{
					theButtOnly->HotSpotAction(-1, true, false);
					Draw(nil);
					theButtOnly->HotSpotResult(-1);
				}
			}	
		}
	}
	
	if (keyHandled)
		mExecuteHost = false;
	else	
		mExecuteHost = true;
}

//
// Perform the binding operation referenced by inKey. Returns true if we
// fomd the ke in the list, false otherwise.
//
bool CPlayerView::DoKeyBind(const char inKey)
{
	LArrayIterator iterator(*mKeyBinds, LArrayIterator::from_Start);
	sCardKey	theBind;
	
	while (iterator.Next(&theBind))
	{
		if (theBind.mTheChar == inKey)
		{
			if (ProcessingKeys())
			{
				// We want to kill movies (both audio and video) when
				// keybinding.
				if (gMovieManager.Playing())
					gMovieManager.Kill();

#ifdef DEBUG
				gDebugLog.Log("keybind hit: key <%c>, jump to <%s>", 
					inKey, (const char *) *(theBind.mCardName));
#endif				
				gCardManager.JumpToCardByName((const char *) *(theBind.mCardName), false);
				return (true);
			}
			else
			{
				// ::SysBeep(30);
				return (false);
			}
		}
	}
	
	return (false);
}

//
// Add a key action to the list of posible keys. If the card name is nil,
// then remove the key from the list.
//
void CPlayerView::AddKeyBinding(const char inKey, CCard *inCardToJumpTo)
{
	int32		bindIdx = 1;
	sCardKey	theBind;
	char		realInKey;
	bool		foundIt = false;
	
	realInKey = inKey;
	
	// Look for key. 
	while  ((not foundIt) and mKeyBinds->FetchItemAt(bindIdx++, &theBind))
		foundIt = (theBind.mTheChar == realInKey);

	if (foundIt)
		bindIdx--;	// if we found it we have already incremented past it
		
	theBind.mTheChar 	= realInKey;
	theBind.mCardName   = new KString;
	*(theBind.mCardName)	= inCardToJumpTo->Name();
	//theBind.mCard 		= inCardToJumpTo;
	
	if (inCardToJumpTo == NULL)	// If no card, delete the key
	{
		if (foundIt)
			mKeyBinds->RemoveItemsAt(1, bindIdx);
		// else couldn't find it, so WGAFF?
	}
	else if (foundIt)	// We want to replace the key action
	{
		mKeyBinds->AssignItemsAt(1, bindIdx, &theBind);
	}
	else				// OK, just a new key. How boring.
		mKeyBinds->InsertItemsAt(1, LArray::index_Last, &theBind);
}


//
// Blippo - save the current offscreen gworld for later restoration (unblippo)
//
void CPlayerView::Blippo(void)
{
	//CTabHandle		theCTab;
	StColorPenState savePenState;
	Rect  			theFrame;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(theFrame);

	// Get the blippo gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mBlippoWorld->GetMacGWorld();
	
	// cbo - now set the color table in DoNewPalette
	// Update the blippo world with the latest device info
	//theCTab = gTheApp->GetCTab();
	//if (theCTab != nil)
	//{
	//	int32 myGWorldFlags = ::UpdateGWorld(&theMacGWorld, 0, &theFrame, theCTab, nil, 0);
	//	mBlippoWorld->SetMacGWorld(theMacGWorld);
	//}
	
	// Copy what we have in our current gworld into the blippo world
	mGWorld->CopyImage((GrafPtr)theMacGWorld, theFrame);
	
	mHasBlip	= true;
}

//
// UnBlippo - Restore the saved bitmap created by Blippo
//
void CPlayerView::UnBlippo(const FXType inEffect, const int8 inTime)
{
	DoEffect(inEffect, inTime);
}

//
// Lock - draw directly into the offscreen world (which we do anyway).
// All we do here is set a flag to indicate that we should not receive
// updates. Passing true in clear will erase the offscreen world to white.
//
void CPlayerView::Lock(const bool /* inClear */)
{
	Draw(nil);		// cbo - make sure the screen is up-to-date
	mIsLocked = true;
	
	gVariableManager.SetLong("_locked", 1);
	
	ProcessEvents(false);
}

//
// UnLock - Unconditional draw of the offscreen gworld.
//
void CPlayerView::UnLock(const FXType inEffect, const int8 inTime)
{
	mIsLocked = false;
	
	gVariableManager.SetLong("_locked", 0);
	
	ProcessEvents(true);
	
	// If we specify an effect, copy the offscreen world to the blippo world,
	// so we can do the effect...
	if (inEffect != kFXNone)
	{
		Blippo();		// copy the offscreen world to the blippo buffer for effects
		
		DoEffect(inEffect, inTime);
	}
	else				//... otherwise just blast & pray...
		Draw(nil);
}

//
// Micro - Transition to/from movies
//
void CPlayerView::Micro(const FXType inEffect)
{
	DoEffect(inEffect, 20);
}

//
// Do the specified visual effect over the specified time.
//
void CPlayerView::DoEffect(const FXType inEffect, const int8 inTime)
{	
	// if they didn't specify a time, just copy the damn bits.
	if (inTime == 0)
	{
		Rect  destFrame;
		StColorPenState savePenState;
		
		// Sets fore & background color for CopyBits.
		StColorPenState::Normalize();
	
		CalcLocalFrameRect(destFrame);
	
		// Get the real gworld from the card view, and draw into it.
		GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();
		mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, destFrame);
		
		Draw(nil);			// make sure it makes its way to the screen
		return;
	}

	switch (inEffect)
	{
		case kFXNone:
		{
			Rect  destFrame;
			StColorPenState savePenState;
			
			// Sets fore & background color for CopyBits.
			StColorPenState::Normalize();
		
			CalcLocalFrameRect(destFrame);
		
			// Get the real gworld from the card view, and draw into it.
			GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();
			mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, destFrame);
			
			Draw(nil);			// make sure it makes its way to the screen
			break;
		}
		
		case kFXWipeRight:
		case kFXWipeLeft:
		case kFXWipeUp:
		case kFXWipeDown:
			DoWipe(inEffect, inTime);
			break;

		case kFXBlinds:
			DoBlinds(inTime);
			break;

		//case kFXXFade:
		//	DoXFade(inTime);
		//	break;
			
		//case kFXPushRight:
		//case kFXPushLeft:
		//case kFXPushUp:
		//case kFXPushDown:
		//	DoSlide(inEffect, inTime);
		//	break;

		default:
			break;
	}
}

//
// Slide the blippo world onto the screen world in the specified direction, over
// the specified time.
//
void CPlayerView::DoSlide(const FXType inEffect, const int8 inTime)
{
	int32		theTick;		// Tick counter for timing.
	int16		ticksPerLoop;	// Number of ticks per loop (1 tick = 1/60 sec.).
	int16		pixelsToMove;	// Total number of pix to move for effect
	int16		pixelsPerLoop;	// Number of pixels to move per iteration
	Rect		screenRect;		// The part of the screen we copy to the blippo buffer.
	Rect		frameRect;		// Final destination rect
	Rect		lockRect;		// The part of the lock buffer we copy to the blippo buffer.
	Rect		destRect1;		// Rect for screen part in blippo.
	Rect		destRect2;		// Rect for lock part in blippo.
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(frameRect);

	// Figure out total number of pixels to move based on direction (should
	// be 640 or 480 for the player view).
	if ((inEffect == kFXPushRight) || (inEffect == kFXPushLeft))
		pixelsToMove = frameRect.right - frameRect.left;
	else
		pixelsToMove = frameRect.bottom - frameRect.top;
	
	// Loop how many times? (ticks)
	// inTime is in tenths of a second and 1 tick = 1/60 sec.
	ticksPerLoop = 3;
	int16 iterations = (inTime * 6) / ticksPerLoop;
		
	// Figure out how many pixels we need to move based on the number
	// of iterations.
	pixelsPerLoop = (pixelsToMove/iterations) + 1;
	
	// Determine where the drawing destination rectangle should
	// initially be. At the same time, set the horizontal and
	// vertical deltas for moving the rectangle.
	screenRect = frameRect;
	destRect1 = frameRect;
	destRect2 = frameRect;
	lockRect = frameRect;
	int16 hDelta = 0;
	int16 vDelta = 0;
	
	switch( inEffect )
	{
		case kFXPushLeft:
			//OffsetRect( &destRect, -pixelsToMove, 0 );
			//destRect.left += (pixelsToMove - pixelsPerLoop);
			//srcRect.right -= (pixelsToMove - pixelsPerLoop);
			hDelta = -pixelsPerLoop;
			break;

		case kFXPushRight:
			screenRect.right -= pixelsPerLoop;
			destRect1.left += pixelsPerLoop;
			destRect2.right -= (pixelsToMove - pixelsPerLoop);
			lockRect.left += (pixelsToMove - pixelsPerLoop);
			
			//OffsetRect( &destRect, pixelsToMove, 0 );
			//destRect.right -= (pixelsToMove - pixelsPerLoop);
			//srcRect.left += (pixelsToMove - pixelsPerLoop);
			hDelta = -pixelsPerLoop;
			break;

		case kFXPushDown:
			//OffsetRect( &destRect, 0, -pixelsToMove );
			//destRect.bottom -= (pixelsToMove - pixelsPerLoop);
			//srcRect.top += (pixelsToMove - pixelsPerLoop);
			vDelta = pixelsPerLoop;
			break;

		case kFXPushUp:
			//OffsetRect( &destRect, 0, pixelsToMove );
			//destRect.top += (pixelsToMove - pixelsPerLoop);
			//srcRect.bottom -= (pixelsToMove - pixelsPerLoop);
			vDelta = -pixelsPerLoop;
			break;
	}
		
	// Get the real gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();
	
	// Loop and move the source onto the image.
	
	EventRecord	theEvent;
	GrafPtr screenPort = (GrafPtr) GetMacPort();
	GrafPtr destPort = (GrafPtr) mBlippoWorld->GetMacGWorld();
	GrafPtr lockPort = (GrafPtr) mGWorld->GetMacGWorld();
	int16	pixelsMoved = pixelsPerLoop;
	
	for ( int16 i = 1; (i <= iterations) and (pixelsMoved < pixelsToMove); ++i )
	{
		gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
		
		// Start counting ticks from here.
		theTick = ::TickCount();		

		// Copy the screen part to the blippo buffer.
		::CopyBits(&(screenPort)->portBits, &(destPort)->portBits,
					&screenRect, &destRect1, srcCopy, nil);
					
		// Copy the lock buffer part to the blippo buffer.	
		::CopyBits(&(lockPort)->portBits, &(destPort)->portBits,
					&lockRect, &destRect2, srcCopy, nil);
					
		// Now blast the blippo world to the screen.
		mBlippoWorld->CopyImage(GetMacPort(), frameRect);

		::OffsetRect(&lockRect, hDelta, vDelta);
					
		pixelsMoved += pixelsPerLoop;
		
		// See if we should wait here.
		while ((theTick + ticksPerLoop) > ::TickCount())
			gMovieManager.SpendTime(theEvent);
			
		//while (theTick == ::TickCount())
		//	gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
	}
	
	// Copy the image directly into the destination to make
	// sure the move is complete.
	
	mGWorld->CopyImage((GrafPtr)theMacGWorld, frameRect);
	Draw(nil);
}

//
// Wipe the blippo world onto the screen world in the specified direction, over
// the specified time.
//
void CPlayerView::DoWipe(const FXType inEffect, const int8 inTime)
{
	int16		pixelsToMove;	// Total number of pix to move for effect
	int16		pixelsPerLoop;	// Number of pixels to move per iteration
	Rect		destRect;		// Destination rect to draw into during loop
	Rect		frameRect;		// Final destination rect
	RgnHandle	maskRgn = NewRgn();
	
	ThrowIf_(maskRgn == NULL);
	
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(frameRect);

	// Figure out total number of pixels to move based on direction (should
	// be 640 or 480 for the player view).
	if ((inEffect == kFXWipeRight) || (inEffect == kFXWipeLeft))
		pixelsToMove = frameRect.right - frameRect.left;
	else
		pixelsToMove = frameRect.bottom - frameRect.top;
		
	// Loop how many times? (ticks)
	// inTime is tenths of a second, and 1 tick = 1/60 sec.
	
	int16 iterations = inTime * 6;
	
	// Figure out how many pixels we need to move based on the number
	// of iterations.
	pixelsPerLoop = (pixelsToMove/iterations) + 1;
	
	// Determine where the drawing destination rectangle should
	// initially be. The rect represents only the 'slice' that we
	// are currently drawing (not the area that we are currently drawing).
	destRect = frameRect;
	int16 hDelta = 0;
	int16 vDelta = 0;

	switch( inEffect )
	{
		case kFXWipeLeft:
			destRect.left += (pixelsToMove - pixelsPerLoop);
			hDelta = -pixelsPerLoop;
			break;
			
		case kFXWipeRight:
			destRect.right -= (pixelsToMove - pixelsPerLoop);
			hDelta = pixelsPerLoop;
			break;
			
		case kFXWipeDown:
			destRect.bottom -= (pixelsToMove - pixelsPerLoop);
			vDelta = pixelsPerLoop;
			break;
			
		case kFXWipeUp:
			destRect.top += (pixelsToMove - pixelsPerLoop);
			vDelta = -pixelsPerLoop;
			break;
	}
	
	// Get the real gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();

	// Get the initial tick count for timing
	int32	theTick = ::TickCount();
	
	// Loop and move the source onto the image. If this loop takes longer
	// than one tick to process then the effect will take longer than we 
	// want it to. If this is a problem, we'll have to change the loop
	// to look at elapsed ticks, and calculate the delta based on that value.
	// Ugh.

	EventRecord	theEvent;
	
	for (int16 i = 0; i < iterations; ++i)
	{
		gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
		
		// Wait for the next tick before we draw.
		while (theTick == ::TickCount())
			gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
			
		// Now copy the source image into the drawing
		// destination rectangle.
		::RectRgn(maskRgn, &destRect);
		mBlippoWorld->CopyImage(GetMacPort(), frameRect, srcCopy, maskRgn);
		
		// Move the rect to the next 'slice'
		::OffsetRect(&destRect, hDelta, vDelta);
		
		// Get the tick for timing
		theTick = ::TickCount();
	}
	
	// Copy the image directly into the destination to make
	// sure the move is complete. (via the offscreen gworld
	// so that it is also current).
	
	mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, frameRect);
	Draw(nil);
	
	if (maskRgn != NULL)
		::DisposeRgn(maskRgn);
}

//
// Do the 'venitian blind' effect.
//
void CPlayerView::DoBlinds(const int8 inTime)
{
	int16		pixToMove;		// Number of pixels to move per iteration
	Rect		viewRect;		// Rect of PlayerView
	Rect		slatRect;		// Rect of a single slat
	int16		numSlats = 10;  // For optimum effect, this should be evenly divisible into the view height
	int16		waitTicks = 1;
	RgnHandle	maskRgn = NewRgn();
	EventRecord	theEvent;

	ThrowIf_(maskRgn == NULL);
		
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(viewRect);
	
	// Calculate Pixels per Slat
	int16 pixPerSlat = (viewRect.bottom - viewRect.top)/numSlats;
	
	// Loop how many times? (ticks)
	// inTime is tenths of a second, and 1 tick = 1/60 sec.
	int16 iterations = inTime * 6;
	
	// Check to see if we're iterating for more pixels than we need. If so,
	// set a wait var to wait that number of ticks between iterations to keep
	// the time accurate.
	if (iterations > pixPerSlat)
	{
		waitTicks = iterations/pixPerSlat;
		pixToMove = 1;
	}
	else
	{
		waitTicks = 1;
		pixToMove = (pixPerSlat/iterations) + 1;
	}
	
	// Get the real gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();

	// Get the initial tick count for timing
	int32	theTick = ::TickCount();
	
	for (int i = 0; i < iterations; ++i)
	{
		gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
		
		// Set the initial slat rect. This will be expanded by pixToMove, and offset
		// 'numSlats' times to create the mask rect.
		::SetRect(&slatRect, viewRect.left, viewRect.top, 
							 viewRect.right, viewRect.top + (pixToMove * i));
		
		// Open a region for drawing
		::OpenRgn();
		
		for (int8 slat = 0; slat < numSlats; ++slat)
		{
			// Frame the slat
			::FrameRect(&slatRect);

			// Step the slat down the screen.
			::OffsetRect(&slatRect, 0, pixPerSlat);
		}
		
		// Now we have all the slats drawn into the mask region.
		::CloseRgn(maskRgn);
		
		// Draw the image, using the mask region we just created.
		mBlippoWorld->CopyImage(GetMacPort(), viewRect, srcCopy, maskRgn);
		

		while ((::TickCount() - theTick) < waitTicks)
			gMovieManager.SpendTime(theEvent);	// cbo_fix - OK?
		
		theTick = ::TickCount();
	}
	
	// Copy the image directly into the destination to make
	// sure the move is complete. (via the offscreen gworld
	// so that it is also current).
	
	mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, viewRect);
	Draw(nil);
	
	if (maskRgn != NULL)
		::DisposeRgn(maskRgn);
}


//
// Fade the blippo world onto the screen world over
// the specified time.
//
void CPlayerView::DoXFade(const int8 inTime)
{
	int32		grayStep;	// Amount to increment RGB each iteration
	Rect		viewRect;	// Final destination rect
	RGBColor	grayColor;	// Color to use for 'blend' mode - shade of gray
	
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();

	CalcLocalFrameRect(viewRect);

	// Loop how many times? (ticks)
	// inTime is tenths of a second, and 1 tick = 1/60 sec.
	int16 iterations = inTime * 6;
	
	// Now figure out how much to increment the blend, based on the above.
	grayStep = 0xFFFF;
	grayStep -= 0x0F00;
	grayStep /= iterations;
	//grayStep = (0xFFFF - 0x0F00)/iterations;
	
	// Get the real gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();

	// Get the initial tick count for timing
	int32	theTick = ::TickCount();
	
	// Set the beginning blend weight. This will be multiplied by
	// speed before being used the first time.
	
	grayColor.blue = grayColor.green = grayColor.red = 0x0F00;
	
	// Get the tick for timing
	theTick = ::TickCount();

	// Loop and copy the source image to the destination
	// blending more and more of the source image in by using
	// a lighter and lighter "blend weight" (set by the OpColor
	// function).

	EventRecord theEvent;
	
	for (int16 i = 0; (i < iterations) and (grayColor.blue < 0xFFFF); ++i)
	{
		while (theTick == ::TickCount())
			gMovieManager.SpendTime(theEvent);
			
		// Set the new blend weight.
		::OpColor(&grayColor);
		
		// Do the blend. Double-buffer it so it looks faster.
		mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, viewRect, blend);
		Draw(nil);
		
		// Lighten the blend weight color.
		grayColor.blue += grayStep;
		grayColor.red = grayColor.green = grayColor.blue;
		
		// Get the tick for timing.
		theTick = ::TickCount();
	}
	
	// Copy the image directly into the destination to make
	// sure the move is complete. (via the offscreen gworld
	// so that it is also current).
	
	mBlippoWorld->CopyImage((GrafPtr)theMacGWorld, viewRect);
	Draw(nil);
	
	// Reset the blend weight to black.
	// This is the "normal" weight.
	
	::OpColor(&kRGB_Black);
}

#ifdef DEBUG_5L_LATER
//
//	DoFade
//
void CPlayerView::DoFade(const int8 inTime, const bool inFadeIn)
{
	int16		grayStep;	// Amount to increment RGB each iteration
	Rect		viewRect;	// Final destination rect
	RGBColor	grayColor;	// Color to use for 'blend' mode - shade of gray
	
	StColorPenState savePenState;

	// Sets fore & background color for CopyBits.
	StColorPenState::Normalize();
	
	// If we are fading in we want to get a copy of 

	CalcLocalFrameRect(viewRect);

	// Loop how many times? (ticks)
	// inTime is tenths of a second, and 1 tick = 1/60 sec.
	int16 iterations = inTime;
	
	// Now figure out how much to increment the blend, based on the above.
	grayStep = (0xFFFF - 0x0F00)/iterations;
	
	// Get the real gworld from the card view, and draw into it.
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();

	// Get the initial tick count for timing
	int32	theTick = ::TickCount();
	
	// Set the beginning blend weight. This will be multiplied by
	// speed before being used the first time.
	
	grayColor.blue = grayColor.green = grayColor.red = 0x0F00;
	
	// Get the tick for timing
	theTick = ::TickCount();

	// Loop and copy the source image to the destination
	// blending more and more of the source image in by using
	// a lighter and lighter "blend weight" (set by the OpColor
	// function).

	for (int16 i = 0; (i < iterations) and (grayColor.blue < 0xFFFF); ++i)
	{
		// Set the new blend weight.
		::OpColor(&grayColor);
		
		// Do the blend. Double-buffer it so it looks faster.
		mFadeWorld->CopyImage((GrafPtr) theMacGWorld, viewRect, blend);
		Draw(nil);
		
		// Lighten the blend weight color.
		grayColor.blue += grayStep;
		grayColor.red = grayColor.green = grayColor.blue;
	}
	
	// Copy the image directly into the destination to make
	// sure the move is complete. (via the offscreen gworld
	// so that it is also current).
	
	mFadeWorld->CopyImage((GrafPtr) theMacGWorld, viewRect);
	Draw(nil);
	
	// Reset the blend weight to black.
	// This is the "normal" weight.
	
	::OpColor(&kRGB_Black);
}
#endif

//
// Translate the string 'inText' to the appropriate effect.
//
FXType StringToEffect(KString inText)
{
	inText.MakeLower();
	
	if (inText == "none")
		return (kFXNone);
	else if (inText == "right")
		return (kFXWipeRight);
	else if (inText == "left")
		return (kFXWipeLeft);
	else if (inText == "up")
		return (kFXWipeUp);
	else if (inText == "down")
		return (kFXWipeDown);
	else if (inText == "blinds")
		return (kFXBlinds);
// keep these out for the time being
//	else if (inText == "xfade")
//		return (kFXXFade);
// cbo - try new effects
//	else if (inText == "pushright")
//		return (kFXPushRight);
//	else if (inText == "pushleft")
//		return (kFXPushLeft);
//	else if (inText == "pushtop")
//		return (kFXPushUp);
//	else if (inText == "pushdown")
//		return (kFXPushDown);
// end of new effects
	else
	{
		gLog.Caution("Illegal/unknown effect :%s", (const char *) inText);
		return (kFXNone);
	}		
}

