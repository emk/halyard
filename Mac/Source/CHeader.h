// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
/***************************************

    Header class. Knows how to draw
    text in a given header style.


****************************************/

#ifndef _H_CHEADER
#define _H_CHEADER

#include "THeader.h"
#include "TPrimitives.h"
#include <boost/utility.hpp>

BEGIN_NAMESPACE_FIVEL

class CHeader
{
    protected:

		std::string		mName;
		Alignment   	mAlignment;
        int16         	mColor;
        int16         	mHighlightColor;
        int16         	mShadow;
        int16         	mShadowColor;
        int16         	mShadowHighlightColor;
        int16         	mHilite;
        int16			mUnderline;

		int16			mFontFamily;
		int16			mHeight;

		bool			mBold;
		
    public:

        				CHeader(TArgumentList &inArgs);
						~CHeader();
						
		std::string		GetName()               { return mName; }
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

class CHeaderManager : boost::noncopyable
{
	std::map<std::string,CHeader*> mHeaderMap;

public:
	virtual ~CHeaderManager() { RemoveAll(); }

	//////////
	// Return the specified header, or NULL.
	//
	CHeader *Find(const std::string &inName);

	//////////
	// Create a new header using the supplied parameters.
	//
	void AddHeader(TArgumentList &inArgs);

	//////////
	// Remove all the headers from this object.
	//
	void RemoveAll();
};

extern CHeaderManager gHeaderManager;

END_NAMESPACE_FIVEL

#endif // _H_CHEADER
