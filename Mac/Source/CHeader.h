/***************************************

    Header class. Knows how to draw
    text in a given header style.


****************************************/

#ifndef _H_CHEADER
#define _H_CHEADER

#include "THeader.h"

#include "TIndex.h"

BEGIN_NAMESPACE_FIVEL

class CHeader : public TIndex 
{
    protected:

		Alignment   	mAlignment;
        int16         	mColor;
        int16         	mHighlightColor;
        int16         	mShadow;
        int16         	mShadowColor;
        int16         	mHilite;
        int16			mUnderline;

		int16			mFontFamily;
		int16			mHeight;

		bool			mBold;
		
    public:

        				CHeader(TIndexFile *inFile, const char *inName = NULL, 
        						int32 inStart = 0, int32 inEnd = 0);
						~CHeader();
						
        virtual void 	ParseScript(void);
        
        int16			GetFontFamily(void) 	{ return (mFontFamily); }
        int16			GetHeight(void)			{ return (mHeight); }
        Alignment		GetAlignment(void)		{ return (mAlignment); }
        int16			GetColor(void)			{ return (mColor); }
		int16			GetHighlightColor(void) { return (mHighlightColor); }
		int16			GetShadow(void)			{ return (mShadow); }
		int16			GetShadowColor(void)	{ return (mShadowColor); }
		bool			GetBold(void)			{ return (mBold); }
        
	private:
		void			GetFont(const char *inName);
};

class CHeaderManager : public TIndexManager 
{
	public:
	
		virtual void 	ProcessTopLevelForm(TIndexFile *inFile, const char *inName, int32 inStart, int32 inEnd);
};

extern CHeaderManager gHeaderManager;

END_NAMESPACE_FIVEL
#endif
