//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999,2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TURL.h : 
//

#if !defined (_LURL_h_)
#define _LURL_h_

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TURL

	A URL String class.  

AUTHOR
    Chuck Officer<br>

------------------------------------------------------------------*/
class TURL : public TString
{
public:
	//////////
	// Default Constructor. 
	//
	TURL();

	//////////
	// Constructor.  Init URL string.
	//
	// [in] inStr - initial URL string
	//
	TURL(const TString &inStr);

	//////////
	// Destructor. 
	//
	virtual		~TURL();

	//////////
	// Type of URL. 
	//
	enum URLType { FtpURL, HttpURL, FileURL, RtspURL, UnknownURL };

	//////////
	// Get the server portion of the URL.
	//
	// [out] return - the server portion
	//
	TString		GetServerPart();
	
	//////////
	// Get the directory portion of the URL.
	//
	// [out] return - the directory portion
	//
	TString		GetDirPart();
	
	//////////
	// Get the first directory of the directory portion
	//
	// [out] return - the first directory
	//
	TString		GetFirstDir();
	
	//////////
	// Get the filename.
	//
	// [out] return - the filename
	//
	TString		GetFileName();

	//////////
	// Set the URL.
	//
	// [in] inStr - URL string
	//
	TURL		&operator=(const TString &inStr);
	
	//////////
	// Append to a URL.
	//
	// [in] inStr - string to append
	//
	TURL		&operator+=(const TString &inStr);

	//////////
	// Is the URL legal?
	//
	// [out] return - true if it is legal, false otherwise
	//
	bool		IsLegal();
	
	//////////
	// Is the URL local?
	//
	// [out] return - true if it points to a local resource, false otherwise
	//
	bool		IsLocal();
	
	//////////
	// Is the URL remote?
	//
	// [out] return - true if it points to a remote resource, false otherwise
	//
	bool		IsRemote();
	
	//////////
	// Is the URL a FILE resource?
	//
	// [out] return - true if it is a FILE resource, false otherwise
	//
	bool		IsFile();
	
	//////////
	// Is the URL an HTTP resource?
	//
	// [out] return - true if it is an HTTP resource, false otherwise
	//
	bool		IsHTTP();
	
	//////////
	// Is the URL an FTP resource?
	//
	// [out] return - true if it is an FTP resource, false otherwise
	//
	bool		IsFTP();
	
	//////////
	// Is the URL an RTSP resource?
	//
	// [out] return - true if it is an RTSP resource, false otherwise
	//
	bool		IsRTSP();

protected:
	//////////
	// Check the syntax of the URL.
	//
	// [out] return - true if the syntax is good, false otherwise
	//
	bool		CheckSyntax();

	//////////
	// Server portion of the URL.
	//
	TString		m_ServerPart;
	
	//////////
	// Directory portion of the URL
	//
	TString		m_DirPart;
	
	//////////
	// URL type.
	//
	URLType		m_Type;
	
	//////////
	// Do we have a good URL?
	//
	bool		m_HaveGoodURL;
};

END_NAMESPACE_FIVEL

#endif // _LURL_h_

/*
 $Log$
 Revision 1.3  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.2  2002/03/04 15:16:17  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
