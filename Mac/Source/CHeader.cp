// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
/*********************

    CHeader MANAGER

*********************/

#include "THeader.h"

#include <stdlib.h>
#include <stdio.h>

#include "TLogger.h"
#include "CHeader.h"

USING_NAMESPACE_FIVEL

CHeaderManager FIVEL_NS gHeaderManager;

//
//  Let Index ancestor construct based on these values.
//
CHeader::CHeader(TArgumentList &inArgs)
{
    mAlignment = AlignLeft;
    mColor = mHighlightColor = mShadowColor = 0;
    mShadow = 0;

    TString     align, fontname;
    int16		offset;				// new parameter

    // HNAME FONTNAME...
    //
    inArgs >> mName >> fontname;
	mName = MakeStringLowercase(mName);

    GetFont(fontname); 	
    
    //  ...ALIGNMENT COLOR HIGHCOLOR...
    inArgs >> align >> mColor >> mHighlightColor; 
    align.MakeLower();
   
    if (align == (char *) "center")
        mAlignment = AlignCenter;
    else if (align == (char *) "right")
        mAlignment = AlignRight;
    else
        mAlignment = AlignLeft;

    //  ...SHADOW SHADCOLOR OFFSET - the offset is ignored for now, it is used on Windows
    //									to help with text alignment
    if (inArgs.HasMoreArguments())
        inArgs >> mShadow >> mShadowColor;
        
    if (inArgs.HasMoreArguments())
    	inArgs >> offset;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> mShadowHighlightColor;
    }
}

CHeader::~CHeader()
{

}

//
//	GetFont - Parse the string to get the name and size of the font family.
//				Map the font family to an equivalent on the Mac.
//
void CHeader::GetFont(const char *inName)
{
	char	theBuf[3];
	char	theNameBuf[33];
	char	theFontName[33];
	char	*theUnderscore;
	int32	theHeight;
	int16	theNameLen = 0;
	
	strcpy(theNameBuf, inName);
	
	// replace all underscores with spaces
	theUnderscore = strstr(theNameBuf, "_");
	for (; theUnderscore != NULL; theUnderscore = strstr(theNameBuf, "_"))
		*theUnderscore = ' ';
		
	theNameLen = strlen(theNameBuf);
	
	// get the height off the end of the name
	theBuf[0] = theNameBuf[theNameLen - 2];
	theBuf[1] = theNameBuf[theNameLen - 1];
	theBuf[2] = '\0';
	theHeight = atol(theBuf);
	mHeight = theHeight;
	
	// get rid of size and trailing space
	strncpy(theFontName, theNameBuf, theNameLen - 2);
	theFontName[theNameLen - 2] = '\0';
	
	if (theFontName[theNameLen - 3] == ' ')
		theFontName[theNameLen - 3] = '\0';

	// New bold option.
	if ((theFontName[0] == 'b') or (theFontName [0] == 'B'))
		mBold = true;
	else
		mBold = false;
				
	// now see if we can find this font family
	// The font names are Apple defines
	if (strstr(theFontName, "san"))
	{
		mFontFamily = kFontIDHelvetica;
					
		// cbo_fix - if we request helvetica 16 it will look funky
		if (mHeight == 16)
			mHeight = 14;
	}
	else
	{
// cbo - Hold off on Times New Roman for now.
//
//		int16	fnum;
//		
//		::GetFNum("\pTimes New Roman", &fnum);
//		
//		if (fnum != 0)
//			mFontFamily = fnum;
//		else
//		{
//			gDebugLog.Log("Couldn't find Times New Roman - using old times");
			mFontFamily = kFontIDTimes;	// default to regular times
//		}
	}
}


/*****************************

    CHeader MANAGER METHODS

*****************************/

CHeader *CHeaderManager::Find(const std::string &inName)
{
	std::string name = MakeStringLowercase(inName);
	std::map<std::string,CHeader*>::iterator found = mHeaderMap.find(name);
	if (found != mHeaderMap.end())
		return found->second;
	else
		return NULL;
}

void CHeaderManager::AddHeader(TArgumentList &inArgs)
{
	// Create the header and get the name.
	std::auto_ptr<CHeader> head = std::auto_ptr<CHeader>(new CHeader(inArgs));
	std::string name = head->GetName();

	// Check for an exiting header with the same name.
	if (Find(name))
	{
		gLog.Error("Can't redefine header <%s>.", name.c_str());
		return;
	}

	// Insert the new stylesheet in our map.
	mHeaderMap.insert(std::pair<std::string,CHeader*>(name,
													  head.release()));
}

void CHeaderManager::RemoveAll()
{
	// Delete the individual stylesheets and empty the map.
	std::map<std::string,CHeader*>::iterator iter = mHeaderMap.begin();
	for (; iter != mHeaderMap.end(); ++iter)
		delete iter->second;
	mHeaderMap.clear();	
}
