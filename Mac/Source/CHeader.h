/***************************************

    Header class. Knows how to draw
    text in a given header style.


****************************************/

#ifndef _H_CHEADER
#define _H_CHEADER

#include "Mac5L.h"

#include "CIndex.h"

class CHeader : public CIndex 
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

        				CHeader(const char *name = 0, int32 p1 = 0, int32 p2 = 0);
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

class CHeaderManager : public CIndexManager 
{
	public:
	
		OVERRIDE void 	MakeNewIndex(char *name, int32 start, int32 end);
};

extern CHeaderManager gHeaderManager;

#endif
