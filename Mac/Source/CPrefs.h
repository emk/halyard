/*************************************************

	CPreferences

	A little class that interprets command
	line switches and that other classes and
	modules can query to determine things about
	the environment.

**************************************************/

#ifndef _H_CPREFERENCES
#define _H_CPREFERENCES

#include "Mac5L.h"
#include "CObject.h"

//
extern const int16 		gDisplayType;
extern const int16  	gScreenWidth;
extern const int16  	gScreenHeight;
extern const int16  	gPaletteSize;

//
//	Video controller types.
//
enum VideoControl {

	None, Sony, Visage, Videologic, InfoWindows

};

//
//	Screen display modes.
//
enum VidStandard {

	VGA, EGA

};

class CPreferences : CObject {

	private:

		int16			fDemo;				//	In demo mode?
		int16			fEncrypt;			//	Script encrypted?
		int16			fMouse;				//	Use mouse?
		int16			fCheckDisc;			//	Check videodisc ID?
		int16			fPrint;				//	Enable printing?
		int16			fHelp;				//	Just give help and quit.
		VideoControl	videoController;	//	Control type.
		VidStandard		vidDisplay;			//	Display mode.

		int16			timeout;			//	Timeout, in minutes.

	public:

						CPreferences();
		virtual 		~CPreferences(void) {}

		//
		//	Setting methods.
		//
		virtual void 	SetSwitch(char *aSwitch);
  	
		//
		//	Query methods.
		//
		int16			Demo() const { return fDemo; }
		int16			Encrypted() const { return fEncrypt; }
		int16			Mouse() const { return fMouse; }
		int16			CheckDisc() const { return fCheckDisc; }
		int16			Print() const { return fPrint; }
		int16			Help() const { return fHelp; }
		VideoControl	Controller() const { return videoController; }
		VidStandard		Display() const { return vidDisplay; }
		int16			Timeout() const	{ return timeout; }
};


#endif
