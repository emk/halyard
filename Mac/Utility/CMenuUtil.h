//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
//
//	CMenuUtil.h
//
//	For menu stuff.
//
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#pragma once

class CMenuUtil 
{
	public:
					CMenuUtil(void);
		virtual		~CMenuUtil(void);

		void 		HideMenuBar(void);
		void 		ShowMenuBar(void);
		void 		ToggleMenuBar(void);

//		void 		CheckMenuHide(Point *mouseWhere);
		
	private:
		bool			mMenuBarHidden;				//current state of menubar
		bool			mUserWantsMenuBar;			//used to restore menubar state
													//on a resume
		Rect			mMenuBarRect;
		short			mMenuBarHeight;
		RgnHandle		mSavedGrayRgn;
		RgnHandle		mNewGrayRgn;
		RgnHandle		mCoveredRgn;
};

extern CMenuUtil gMenuUtil;