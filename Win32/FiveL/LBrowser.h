//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

#if !defined (_LBrowser_h_)
#define _LBrowser_h_

#include "TCommon.h"
#include "TString.h"

/*-----------------------------------------------------------------

CLASS
    LBrowser

	A singleton class used to launch a browser with a specific URL.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LBrowser
{
public:
	//////////
	// Constructor.
	//
	LBrowser();

	//////////
	// Destructor.
	//
	~LBrowser();

	//////////
	// Initialize.  Searches the system registry for the default browser
	// information.
	//
	void	Init();

	//////////
	// Open the default browser and jump to the specified URL.
	//
	// [in] inURL - the URL to open with the browser
	// [out] return - true on successful browser launch, false otherwise
	//
	bool	GoToUrl(TString &inURL);

protected:
	//////////
	// Get the value of a key in the system registry.
	//
	// [in] inKey - the key
	// [in/out] outValueStr - the value of the key is placed here
	// [out] return - true on success, false if there was an error
	//
	bool		GetString(HKEY inKey, TString &outValueStr);

	//////////
	// Browser path.
	//
	TString		m_BrowserPath;
	
	//////////
	// Do we have the browser path?
	//
	bool		m_HaveBrowserPath;
	
	//////////
	// Is Netscape the default browser?
	//
	bool		m_HaveNetscape;
};


#endif // _LBrowser_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/08/09 14:37:06  chuck
 2.01 Build 5

*/
