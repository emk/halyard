//
//	CPicture.cp
//

#include "debug.h"

#include <stdio.h>
#include <PictUtils.h>

#include "Mac5L.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CPicture.h"
#include "CResource.h"
#include "CPictDataFile.h"
#include "CPlayerView.h"
#include "CVariable.h"


/********************

    PICTURE CLASS

********************/

//
// 	CPicture - 
//
CPicture::CPicture(const char *inName, const char *inBaseName, bool inMatte) : CResource(inName)
{
// cbo_mem
	FSSpec		theFSSpec;
	CInfoPBRec	thePictPB;
	OSErr		theErr;

	mPicture = mHiPicture = nil;
	mWidth = mHeight = mHiWidth = mHiHeight = 0;
	mPicBits =  nil;
	mPicWorld = nil;
	mHiPicBits = nil;
	mHiPicWorld = nil;
	mHaveGoodSize = false;
	mMatte = inMatte;
	size = 0;		// in case we don't find it

// cbo_mem
//#ifdef DONT_DO_MEMORY_PROBS	
	// set the size of the picture to be the length of the file
	// this won't be exact but is better than nothing
	//
	theConfig->FillGraphicsSpec(&theFSSpec, inName);

	thePictPB.hFileInfo.ioFDirIndex = 0;
	thePictPB.hFileInfo.ioNamePtr = (unsigned char *) theFSSpec.name;
	thePictPB.hFileInfo.ioVRefNum = theFSSpec.vRefNum;
	thePictPB.hFileInfo.ioDirID = theFSSpec.parID;
	
	theErr = ::PBGetCatInfoSync(&thePictPB);
	
	if (theErr == noErr)
		size = thePictPB.hFileInfo.ioFlLgLen;		// the right field to use?
		
	// remember the base name (without the .pic extension)
	mPicName = inBaseName;
	
	Load();
}

//
//  ~CPicture - Ignore locked bit and purge if it's loaded.
//
CPicture::~CPicture()
{
    if (state > kResUnloaded)
        _Purge();
}

//
//	GetPictureRect - Get rectangle of picture.
//
Rect CPicture::GetPictureRect(void)
{
	Rect	theRect;
	
	theRect.top = 0;
	theRect.left = 0;
	theRect.right = mWidth;
	theRect.bottom = mHeight;
	
	return (theRect);
}

//
//  _Load - Load the picture from disc.
// 
void CPicture::_Load()
{ 
	if (state == kResUnloaded)
	{
		mPicture = mHiPicture = nil;
		mWidth = mHeight = mHiWidth = mHiHeight = 0;
		mPicBits =  nil;
		mPicWorld = nil;
		mHiPicBits = nil;
		mHiPicWorld = nil;
		
		// load the picture
		LoadPicFile(false);
		
		// load the hilite picture (if there is one)		
		LoadPicFile(true);		
	
		// Set the width, height data members
		if (mPicture != nil)
		{
			mWidth = (*mPicture)->picFrame.right - (*mPicture)->picFrame.left;
			mHeight = (*mPicture)->picFrame.bottom - (*mPicture)->picFrame.top;
		}
		
		if (mHiPicture != nil)
		{
			mHiWidth = (*mHiPicture)->picFrame.right - (*mHiPicture)->picFrame.left;
			mHiHeight = (*mHiPicture)->picFrame.bottom - (*mHiPicture)->picFrame.top;
		}

// cbo_mem
//#ifdef DONT_DO_MEMORY_PROBS
		if (not mHaveGoodSize)
		{
			// reset the size to be what it really is now
			size = 0;
			if (mPicture != nil)
				size += ::GetHandleSize((Handle) mPicture);
			if (mHiPicture != nil)
				size += ::GetHandleSize((Handle) mHiPicture);
			if (mPicWorld != nil)
			{
				Rect	picFrame = (*mPicture)->picFrame;
				int32	height = picFrame.bottom - picFrame.top;
				int32	width = picFrame.right - picFrame.left;
				
				// approximate the size of the GWorld
				size += (height * width * theConfig->GetBitDepth());
			}
			if (mHiPicWorld != nil)
			{
				Rect	picFrame = (*mHiPicture)->picFrame;
				int32	height = picFrame.bottom - picFrame.top;
				int32	width = picFrame.right - picFrame.left;
				
				// approximate the size of the GWorld
				size += (height * width * theConfig->GetBitDepth());
			}
#ifdef DEBUG_5L
			prinfo("Loaded picture: real size <%ld>", size);
#endif
			mHaveGoodSize = true;
				
		}		
//#endif			
	}
}

//
//  _Purge - Clear from memory.
//
void CPicture::_Purge()
{
    Assert_(state > kResUnloaded);

#ifdef DEBUG_5L
	prinfo("Purging picture <%s>", key.GetString());
#endif
    
    if (mPicture != nil)
    {
		::KillPicture(mPicture);
		mPicture = nil;
	}
    
    if (mHiPicture != nil)
    {
		::KillPicture(mHiPicture);
		mHiPicture = nil;
	}
	
	if (mPicWorld != nil)
	{
		delete mPicWorld;
		mPicWorld = nil;
	}
	
	if (mHiPicWorld != nil)
	{
		delete mHiPicWorld;
		mHiPicWorld = nil;
	}
	
	mPicBits = nil;
	mHiPicBits = nil;
	
	// - why do this? then the size won't be accurate!!!	
   // size = 0;
}

//
//	LoadPicFile - Load a pict file from the given name. pass TRUE in isHiPic if we're loading the hilite pic.
// 			We need to not fail if the hilite pic is not found.
void CPicture::LoadPicFile(bool isHiPic)
{ 
	CString		thePicName;
	FSSpec		theFSSpec;
	FInfo		theInfo;
	Handle		theHand;

	// build the correct name for the picture
	thePicName = mPicName;
	if (isHiPic)
		thePicName += "H";
	thePicName += ".pic";
	
	// Create the FSSpec record from the file name
	theConfig->FillGraphicsSpec(&theFSSpec, thePicName.GetString());
	
	// Now that we have a valid FSSpec, we can create the file stream & get
	// the picHandle from the file.
			
	CPictDataFile *theFile = new CPictDataFile(theFSSpec);
	
	// Test for the existence of a hilite picture (picnameH), as there may
	// not be one (backgrounds, for instance). If we get a fnfErr, copy the
	// normal pic into the hilite pic (well, set the pointer at least).
	// There's probably a better way to do this.
	
	if (isHiPic && (FSpGetFInfo(&theFSSpec, &theInfo) == fnfErr))	
	{
		mHiPicture = nil;		// cbo - leave it nil and just use the other one
		//mHiPicture = mPicture;
		return;
	}

	Try_
	{
		theFile->OpenDataFork(fsCurPerm);
		theHand = theFile->ReadDataFork();
		
		if (isHiPic)
			mHiPicture = (PicHandle) theHand;
		else
			mPicture = (PicHandle) theHand;
		
		theFile->CloseDataFork();
		delete theFile;
	}

	Catch_(inErr)
	{
#ifdef DEBUG_5L
		prinfo("ERROR: Couldn't find Pict file <%s>", thePicName.GetString());
#else
		prcaution("Could not find Graphic <%s>", thePicName.GetString());
#endif
		
		return;
	} EndCatch_
	
	SignalIf_(mPicture == nil);
	SignalIf_(mHiPicture == nil);
	
	//  - can't do this now, the color table might not be set yet - do it 
	//	when ready to draw the picture
	// load the matte
	//if (mMatte)
	//{
	//	if (isHiPic)
	//		SetMatte(mHiPicture, isHiPic);
	//	else
	//		SetMatte(mPicture, isHiPic);
	//}
}

//
//	LoadMatte - Load the matte bitmap.
// 
void CPicture::LoadMatte(const bool isHilite)
{
	if (isHilite)
	{
		if (mHiPicture != nil)
			SetMatte(mHiPicture, isHilite);
	}
	else
	{
		if (mPicture != nil)
			SetMatte(mPicture, isHilite);
	}
}

//
//	TossMatte - Get rid of the matte information.
//
void CPicture::TossMatte(void)
{
	if (mPicWorld != nil)
	{
		delete mPicWorld;
		mPicWorld = nil;
	}
	
	if (mHiPicWorld != nil)
	{
		delete mHiPicWorld;
		mHiPicWorld = nil;
	}
	
	mPicBits = nil;
	mHiPicBits = nil;
}

//
//	SetMatte - Set the mask bitmap for the picture and hilite pic, if present.
//
void CPicture::SetMatte(const PicHandle inPicture, bool isHiPic)
{
	CTabHandle		theCTab;
	PixMapHandle	pixHand;
	Rect			maskRect = (*inPicture)->picFrame;

	// Set up a gworld for drawing, so we can capture the pic's bitmap
	CGWorld *picGWorld = new CGWorld(maskRect);

	// Now we have a gworld with a bitmap. Get the gworld ptr. 
	GWorldPtr 	macGWorld = picGWorld->GetMacGWorld();
	
	theCTab = gTheApp->GetCTab();
		
	::UpdateGWorld(&macGWorld, 0, &maskRect, theCTab, nil, 0);
	picGWorld->SetMacGWorld(macGWorld);
	
	pixHand = ::GetGWorldPixMap(macGWorld);
	
	// Now draw the picts into the gworld
	picGWorld->BeginDrawing();
	::DrawPicture(inPicture, &maskRect);
	picGWorld->EndDrawing();

	if (isHiPic)
	{
		mHiPicBits = pixHand;
		mHiPicWorld = picGWorld;
	}
	else
	{
		mPicBits = pixHand;
		mPicWorld = picGWorld;
	}		
}

/******************************

    PICTURE DRAWING METHODS

******************************/

//
//  DrawPic - Draw the given picture at the point. (matte & direct default)
//
void CPicture::DrawPic(Point &pt, BitMap &destBits, Boolean matte, Boolean /* direct */)
{
	Rect			drawRect;
	ResState		oldState;
	StColorPenState savePenState;
	
	StColorPenState::Normalize();
	
	::SetRect(&drawRect, pt.h, pt.v, pt.h + mWidth, pt.v + mHeight);

	if (mPicture == nil)
		_Load();
	
	oldState = GetState();
	Lock(true);
		
	if (matte)
	{
		Rect maskRect = (*mPicture)->picFrame;
		LoadMatte(false);
		::CopyBits((BitMap *) *mPicBits, &destBits, &maskRect, &drawRect, transparent, nil);
		TossMatte();
	}
	else
		::DrawPicture(mPicture, &drawRect);
		
	SetState(oldState);
}

//
//  DrawPic - Draw the given picture in the rect.
//
void CPicture::DrawPic(Rect &theRect, BitMap &destBits, Boolean matte, Boolean /* direct */)
{
	ResState		oldState;
	StColorPenState savePenState;
	StColorPenState::Normalize();

	if (mPicture == nil)
		_Load();
	
	oldState = GetState();
	Lock(true);
	
	if (matte)
	{
		Rect maskRect = (*mPicture)->picFrame;
		LoadMatte(false);
		::CopyBits((BitMap *) *mPicBits, &destBits, &maskRect, &theRect, transparent, nil);
		TossMatte();
	}
	else
		::DrawPicture(mPicture, &theRect);
		
	SetState(oldState);
}

//
//  DrawHiPic - Draw the given Hilight picture at the point.
//
void CPicture::DrawHiPic(Point &pt, BitMap &destBits,  Boolean matte, Boolean /* direct */)
{
	Rect		drawRect;
	ResState	oldState;
	
	if (mHiPicture != nil)
	{	
		StColorPenState savePenState;
		
		StColorPenState::Normalize();
		
		::SetRect(&drawRect, pt.h, pt.v, pt.h + mHiWidth, pt.v + mHiHeight);

		oldState = GetState();
		Lock(true);
		
		if (matte)
		{
			Rect srcRect  = (*mHiPicture)->picFrame;
			LoadMatte(true);
			::CopyBits((BitMap *) *mHiPicBits, &destBits, &srcRect, &drawRect, transparent, nil);
			TossMatte();
		}
		else
			::DrawPicture(mHiPicture, &drawRect);
			
		SetState(oldState);
	}
}

//  
//	Hilite - Hilite a picture by showing the 
//
void CPicture::Hilite(Point &pt, BitMap &destBits, Boolean matte, Boolean /* direct */)
{
	DrawHiPic(pt, destBits, matte);
	gPlayerView->Draw(nil);
	DrawPic(pt, destBits, matte);
	gPlayerView->Draw(nil);
}

//
//  GetPicture - Get the given picture. If it's not there add it to the
//  resource list.
//
CPicture *GetPicture(const char *name, bool matte)
{
	CString		thePictName;
	CString		theBaseName;
    CPicture     *thePict;
    char		*theStrPtr;

	// make sure the picture name ends with .pic (to make it unique
	//	for the resource tree)
	if ((theStrPtr = strstr(name, ".")) != NULL)
		*theStrPtr = '\0';
	
	// save the base name without the .pic
	theBaseName = name;
	
	// use .pic for the resource tree key
	thePictName = name;
	thePictName += ".pic";
	
    thePict = (CPicture *) GetResource(thePictName.GetString());
    
	if (thePict == NULL) 
    {
#ifdef DEBUG_5L
		prinfo("GetPicture: <%s> picture not in tree, loading", name);
#endif

        thePict = new CPicture(thePictName.GetString(), theBaseName.GetString(), matte);

        gResManager.AddNode(thePict);
    }
    else
    {
    	if (thePict->GetState() == kResUnloaded)
    	{
#ifdef DEBUG_5L
			prinfo("GetPicture: <%s> in tree but not in memory, loading", name);
#endif
			thePict->Load();
		}
		else
		{
			
#ifdef DEBUG_5L
			prinfo("GetPicture: <%s> picture already in tree and in memory", name);
#endif
			thePict->Used();		// touch it!
		}
    }

    return (thePict);
}


