/*********************

    CHeader MANAGER

*********************/

#include "THeader.h"

#include <stdlib.h>
#include <stdio.h>

#include "TLogger.h"
#include "CHeader.h"

USING_NAMESPACE_FIVEL

//
//  Let Index ancestor construct based on these values.
//
CHeader::CHeader(TIndexFile *inIndex, const char *inName /* = NULL */, 
				int32 inStart /* = 0 */, int32 inEnd /* = 0 */) :
    		TIndex(inIndex, inName, inStart, inEnd)
{
    mAlignment = AlignLeft;
    mColor = mHighlightColor = mShadowColor = 0;
    mShadow = 0;
}

CHeader::~CHeader()
{
}


/***********************************************************************
 * Function: CHeader::ParseScript
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *      Once we get the CHeader data read into memory, parse it and fill
 *  the CHeader fields. Then we can ditch the original data. Colors are
 * checked to match InfoWindows standards (EGA + MIC restrictions...)
 ***********************************************************************/
void CHeader::ParseScript(void)
{

    TString     align, fontname;
    int16		offset;				// new parameter

    // (CHeader HNAME FONTNAME...
    //
    m_Script >> open >> discard >> discard >> fontname;
   
    GetFont(fontname); 	
    
    //  ...ALIGNMENT COLOR HIGHCOLOR...
    m_Script >> align >> mColor >> mHighlightColor; 
    align.MakeLower();
   
    if (align == (char *) "center")
        mAlignment = AlignCenter;
    else if (align == (char *) "right")
        mAlignment = AlignRight;
    else
        mAlignment = AlignLeft;

    //  ...SHADOW SHADCOLOR OFFSET) - the offset is ignored for now, it is used on Windows
    //									to help with text alignment
    if (m_Script.more())
        m_Script >> mShadow >> mShadowColor >> offset;
        
    m_Script >> close;
     
    FlushScript();
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
//#ifdef DEBUG
//			gDebugLog.Log("Couldn't find Times New Roman - using old times");
//#endif
			mFontFamily = kFontIDTimes;	// default to regular times
//		}
	}
}


/*****************************

    CHeader MANAGER METHODS

*****************************/

/***********************************************************************
 * Function: CHeaderManager::MakeNewIndex
 *
 *  Parameter name
 *  Parameter start         (see Index class)
 *  Parameter end
 * Return:
 *
 * Comments:
 *  Create a new CHeader Index
 ***********************************************************************/
void CHeaderManager::MakeNewIndex(TIndexFile *inFile, const char *inName, 
		int32 inStart, int32 inEnd)
{
    CHeader  *newHeader;
    
    newHeader = new CHeader(inFile, inName, inStart, inEnd);
    
    if (newHeader->SetScript())
    {
    	newHeader->ParseScript(); 
    	
    	Add(newHeader);
    }
}
