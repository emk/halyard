// -*- Mode: C++; tab-width: 4; -*-
//
// CPlayerView.cp
//

#include "THeader.h"

#include <ctype.h>

#include "TLogger.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CPlayerView.h"
#include "CPlayerInput.h"
#include "CMoviePlayer.h"
#include "CPicture.h"
#include "TVariable.h"
#include "CPalette.h"
#include "gamma.h"
//#include "CGWorld.h"
//#include <UGworld.h>
#include <OSUtils.h>
#include <QuickDraw.h>

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

USING_NAMESPACE_FIVEL
using namespace PowerPlant;

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
	mGWorld = new CGWorld(frame, 32);
	mBlippoWorld = new CGWorld(frame, 32);

	// Set the offscreen palette. Don't bother setting the blippo's palette
	// until we actually do it, as it will likely have changed.	
	GWorldPtr theMacGWorld = mGWorld->GetMacGWorld();
	
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

	FinishCreate();
}

//
//	~CPlayerView
//
CPlayerView::~CPlayerView()
{
	DeleteAllKeyBinds();

	if (mGWorld != nil)
		delete mGWorld;
		
	if (mBlippoWorld != nil)
		delete mBlippoWorld;
}

void CPlayerView::DeleteAllKeyBinds()
{
	std::map<char,TCallback*>::iterator iter = mKeyBinds.begin();
	for (; iter != mKeyBinds.end(); ++iter)
		delete iter->second;
	mKeyBinds.clear();
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
		if (TInterpreter::HaveInstance())
			TInterpreter::GetInstance()->Idle();
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
		//	gDebugLog.Log("pausing the movie");
			mPauseFromKey = inFromKey;
			mMoviePaused = true;
			
			if (inFromKey)
				ProcessTZones(false);	// turn off touch zone processing
		}
		//else
		//	gDebugLog.Log("nothing to pause");
	}
	//else
	//	gDebugLog.Log("movie is already paused");
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
		ValidPortRect(&theFrame);		// cbo_fix - this is necessary to keep from seeing the 
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
void  CPlayerView::SetBackPic(TString &picName, Rect inRect)
{
	RgnHandle		theClip = nil;
	FiveL::TPoint	thePt(0, 0);
	//Rect		theFrame;
	FiveL::TRect	clipRect;
	
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
	RGBColor rgbcolor = gPaletteManager.GetColor(mBackColor);
	::RGBForeColor(&rgbcolor);	
	::PenMode(patCopy);
	::PaintRect(&theFrame);
	mGWorld->EndDrawing();
}


//
//	DoNewPalette - This isn't actually all that useful if our offscreen
//				   GWorld is 32 bits deep.
//
void CPlayerView::DoNewPalette(CTabHandle inCTab)
{
	Rect			theFrame;
	
	CalcLocalFrameRect(theFrame);
	theFrame.right -= theFrame.left;
	theFrame.left = 0;
	theFrame.bottom -= theFrame.top;
	theFrame.top = 0;

	(*inCTab)->ctFlags |= 0x4000;			// so will work with offscreen world
		
	// update the gworld with info about the new color table
	UpdateGWorldPalette(mGWorld, inCTab, theFrame);
	
	// update the blippo buffer too!!
	UpdateGWorldPalette(mBlippoWorld, inCTab, theFrame);
}

//
// UpdateGWorldPalette - This updates the actual GWorlds for DoNewPalette 
//
void CPlayerView::UpdateGWorldPalette(CGWorld *inGWorld, CTabHandle inCTab, Rect inFrame)
{
	GWorldPtr 	theMacGWorld = inGWorld->GetMacGWorld();
	GWorldFlags result =
		::UpdateGWorld(&theMacGWorld, 32, &inFrame, inCTab, nil, 0);
	inGWorld->SetMacGWorld(theMacGWorld);

	// If QuickDraw threw out our GWorld and got us a new one, then we need
	// to erase it to black.
	if (result & reallocPix)
	{
		StCGWorldDrawingContext theContext(mGWorld);
		::RGBForeColor(&Color_Black);
		::PenMode(patCopy);
		::PaintRect(&inFrame);
	}
}
		
//
//	AdjustMouseSelf - CodeWarrior calls this when it thinks the mouse might need to be
//                    updated.  We let our built-in cursor manager handle the details.
//
void
CPlayerView::AdjustMouseSelf(Point inPortPt,
					         const EventRecord&  /* inMacEvent */,
					         RgnHandle           /* outMouseRgn */)
{
	Point	localPoint = inPortPt;	
	PortToLocalPoint(localPoint);
	gCursorManager.CheckCursor(localPoint);
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
	CTouchZone			*theButt;
	
	StColorPenState::Normalize();
	
	while (iterator.Next(&theButt))
	{
		Rect	frameRect;
		
		theButt->CalcLocalFrameRect(frameRect);
		
		RGBColor color = gPaletteManager.GetColor(253);
		::RGBForeColor(&color);	
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
	CTouchZone			*theButt;
	bool				done = false;
	
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

	TInterpreter::GetInstance()->KillCurrentCard();
	
	if (gMovieManager.Playing())
		gMovieManager.Kill();
	
	DeleteAllSubPanes();
	
	DeleteAllKeyBinds();
	
	// cbo - if we do this, we won't be able to show load screens Deactivate();
	//Refresh();				// to make the screen draw in black
}

//
//	ExecuteSelf - To get events in the attachment chain.
//
void CPlayerView::ExecuteSelf(MessageT /* inMessage */, void *ioParam)
{
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
		else if (theEvent->modifiers & cmdKey)
		{
			// We used to support holding down the option key to activate a
			// command, but that is (1) non-standard UI on the Mac and (2)
			// requires mucking around in ROM maps that aren't present in Carbon.
			theChar = (theEvent->message & charCodeMask);
			theChar = tolower(theChar);
			
			// first process Cmd (or Option) . (period) - this will kill naps, audio and video
			if (theChar == '.')
			{
				if (gMovieManager.Playing())
					gMovieManager.Kill();
					
				if (TInterpreter::GetInstance()->Napping())
					TInterpreter::GetInstance()->WakeUp();
#ifdef DEBUG
				else if ((gModMan->NoVolume()) and (TInterpreter::GetInstance()->Paused()))
					TInterpreter::GetInstance()->WakeUp();
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
						else if (TInterpreter::GetInstance()->Napping())
							TInterpreter::GetInstance()->WakeUp();
							
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
					
					if (TInterpreter::GetInstance()->Napping())
						TInterpreter::GetInstance()->WakeUp();
#ifdef DEBUG
					else if ((gModMan->NoVolume()) and (TInterpreter::GetInstance()->Paused()))
						TInterpreter::GetInstance()->WakeUp();
#endif
				}
								
				keyHandled = true;			// we always handle the escape key
			}
			else if (not HaveInputUp())
			{
				// see if this is an accelerator for a touch zone
				LArray			&paneList = GetSubPanes();
				LArrayIterator	iterator(paneList, LArrayIterator::from_Start);
				CTouchZone			*theButt;
				CTouchZone			*theButtOnly = nil;
				int16				numButts = 0;
				bool				done = false;
				
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

void CPlayerView::DrawPixMap(GraphicsTools::Point inPoint,
							 GraphicsTools::PixMap &inPixmap)
{
	// We need to draw a portable RGBA pixmap onto a Macintosh RGB
	// Offscreen GWorld.  This is fairly exciting in a variety of
	// alarming ways.
	//
	// See the ancient "Drawing in GWorlds for Speed and Versatility"
	// article from _develop 10_, and the documentation for the PixMap
	// data structure in _Inside Macintosh: Imaging With QuickDraw_.
	
	using GraphicsTools::AlphaBlendChannel;
	using GraphicsTools::Color;
	using GraphicsTools::Distance;
	using GraphicsTools::Point;
	
	// Begin drawing to offscreen GWorld.
	CGWorld *theGWorld = gPlayerView->GetGWorld();
	theGWorld->BeginDrawing();
	
	// Extract our pixmap (and make sure it's the right kind of pixmap).
	PixMapHandle mac_pixmap_handle = theGWorld->GetPortPixMap();
	PixMapPtr mac_pixmap = *mac_pixmap_handle;
	ASSERT(mac_pixmap->rowBytes & 0x8000);		// A PixMap, not a BitMap
	ASSERT(mac_pixmap->packType == 0);          // Unpacked
	ASSERT(mac_pixmap->pixelType == 16); 		// RGBDirect color
	ASSERT(mac_pixmap->pixelSize == 32);        // 32 bits/pixel
	ASSERT(mac_pixmap->cmpCount == 3 ||			// 3 or 4 channels/pixel
		   mac_pixmap->cmpCount == 4);
	ASSERT(mac_pixmap->cmpSize == 8);           // 8 bits/channel
	
	// Clip our pixmap boundaries to fit within our GWorld.
	int gworld_width = mac_pixmap->bounds.right - mac_pixmap->bounds.left;
	int gworld_height = mac_pixmap->bounds.bottom - mac_pixmap->bounds.top;
	Point begin = inPoint;
	begin.x = Max(0, Min(gworld_width, begin.x));
	begin.y = Max(0, Min(gworld_height, begin.y));
	begin = begin - inPoint;
	Point end = inPoint + Point(inPixmap.width, inPixmap.height);
	end.x = Max(0, Min(gworld_width, end.x));
	end.y = Max(0, Min(gworld_height, end.y));
	end = end - inPoint;
	
	// Do some sanity checks on our clipping boundaries.
	ASSERT(begin.x == end.x || // No drawing
		   (0 <= begin.x && begin.x < end.x && end.x <= inPixmap.width));
	ASSERT(begin.y == end.y || // No drawing
		   (0 <= begin.y && begin.y < end.y && end.y <= inPixmap.height));
	
	// Figure out where in memory to begin drawing the first row.
	unsigned char *mac_base_addr =
		(unsigned char*) GetPixBaseAddr(mac_pixmap_handle);
	int mac_row_size = mac_pixmap->rowBytes & 0x3FFF; // Remove flag bits
	unsigned char *mac_row_start =
		(mac_base_addr +
		 (inPoint.y + begin.y) * mac_row_size +
		 (inPoint.x + begin.x) * 4);
	
	// Figure out where in memory to get the data for the first row.
	Color *portable_base_addr = inPixmap.pixels;
	Distance portable_row_size = inPixmap.pitch;
	Color *portable_row_start =
		portable_base_addr + begin.y * portable_row_size + begin.x;
	
	// Draw each row of the pixmap.
	for (int y = begin.y; y < end.y; y++)
	{
		unsigned char *mac_cursor = mac_row_start;
		Color *portable_cursor = portable_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			ASSERT(mac_cursor >= mac_base_addr);
			ASSERT(mac_cursor < mac_base_addr + gworld_height * mac_row_size);
			ASSERT(portable_cursor >= portable_base_addr);
			ASSERT(portable_cursor <
				   portable_base_addr + inPixmap.height * portable_row_size);
		
			// Draw a single pixel.
			GraphicsTools::Color new_color = *portable_cursor;
			mac_cursor[1] = AlphaBlendChannel(mac_cursor[1],
											  new_color.red,
											  new_color.alpha);
			mac_cursor[2] = AlphaBlendChannel(mac_cursor[2],
											  new_color.green,
											  new_color.alpha);
			mac_cursor[3] = AlphaBlendChannel(mac_cursor[3],
											  new_color.blue,
											  new_color.alpha);
			mac_cursor += 4;
			portable_cursor++;
		}
		mac_row_start += mac_row_size;
		portable_row_start += portable_row_size;
	}

	// End drawing to offscreen GWorld.
	theGWorld->EndDrawing();
}

//
// Perform the binding operation referenced by inKey. Returns true if we
// fomd the ke in the list, false otherwise.
//
bool CPlayerView::DoKeyBind(const char inKey)
{
	std::map<char,TCallback*>::iterator found = mKeyBinds.find(inKey);

	if (found != mKeyBinds.end())
	{
		if (ProcessingKeys())
		{
			// We want to kill movies (both audio and video) when
			// keybinding.
			if (gMovieManager.Playing())
				gMovieManager.Kill();
			
			gDebugLog.Log("Key '%c' hit, running callback: %s",
						  found->first,
						  found->second->PrintableRepresentation().c_str());
			found->second->Run();
			return (true);
		}
		else
		{
			// ::SysBeep(30);
			return (false);
		}
	}
	
	return (false);
}

//
// Add a key action to the list of posible keys. If the card name is nil,
// then remove the key from the list.
//
void CPlayerView::AddKeyBinding(const char inKey, TCallback *inCallback)
{
	// Look for an existing keybind for this key, and delete it.
	std::map<char,TCallback*>::iterator found = mKeyBinds.find(inKey);
	if (found != mKeyBinds.end())
	{
		delete found->second;
		mKeyBinds.erase(found);
	}

	// If we have a new keybind, insert it.
	if (inCallback)
		mKeyBinds.insert(std::pair<char,TCallback*>(inKey, inCallback));
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
		::CopyBits(::GetPortBitMapForCopyBits((CGrafPtr) screenPort),
				   ::GetPortBitMapForCopyBits((CGrafPtr) destPort),
				   &screenRect, &destRect1, srcCopy, nil);
					
		// Copy the lock buffer part to the blippo buffer.	
		::CopyBits(::GetPortBitMapForCopyBits((CGrafPtr) lockPort),
				   ::GetPortBitMapForCopyBits((CGrafPtr) destPort),
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

//
// Translate the string 'inText' to the appropriate effect.
//
FXType FiveL::StringToEffect(TString inText)
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

