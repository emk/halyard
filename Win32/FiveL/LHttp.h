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

#if !defined (_LHttp_h_)
#define _LHttp_h_

#include "TCommon.h"
#include "TString.h"

#include "WinSock2.h"

//////////
// Max buffer for Http reply
//
#define MAXBUFFERSIZE 1024

//////////
// Http Error Code Enumeration
//
typedef enum
{
	LHTTP_OK = 0,
	LHTTP_BadURL,
	LHTTP_SocketError,
	LHTTP_URLNotFound,
	LHTTP_NoServerResponse,
	LHTTP_ReceiveError,
	LHTTP_SocketClosed,
	LHTTP_NotInited,
	LHTTP_SendError
} LHttpError;

/*-----------------------------------------------------------------

CLASS
    LHttp

	A singleton class for checking URLs.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LHttp : public TObject
{
public:
	//////////
	// Constructor.
	//
	LHttp();

	//////////
	// Destructor.
	//
	~LHttp();

	//////////
	// Initialize sockets for URL checker.
	//
	// [out] return - true on success, false if there was an error
	//	
	bool		Init();
	
	//////////
	// Check the specified URL to see if it is a valid URL and whether it
	// contains data.
	//
	// [in] inURL - the URL to check
	// [in] inVarName - variable to store reply data
	// [out] return - Http error response
	//
	LHttpError	CheckURL(TString &inURL, TString &inVarName);

protected:
	//////////
	// Clear vars.
	//
	void				Cleanup();
	
	//////////
	// Parse host info and lookup the IP for given URL.
	//
	// [in] inURL - the URL
	// [out] return - true on success, false if there was an error
	//
	bool				GetHostInfo(TString &inURL);
	
	//////////
	// Setup our socket and make a connection to remote host.
	//
	// [out] return - true on success, false if there was an error
	//
	bool				SetUpSocket();
	
	//////////
	// Send an HTTP request to the remote host.
	//
	// [out] return - true on success, false if there was an error
	//
	bool				SendRequest();
	
	//////////
	// Receive a reply from the remote host.
	//
	// [in] inVarName - 5L variable where reply string should be placed
	// [out] return - error code
	//
	LHttpError			ReceiveReply(TString &inVarName);
	
	//////////
	// Close the socket.
	//
	void				CloseSocket();

	//////////
	// Are we okay to use Win32 sockets? 
	//
	bool				m_InitDone;
	
	//////////
	// Socket address for remote host.
	//
	struct sockaddr_in  m_RemoteSockAddr;
	
	//////////
	// Socket address for local host.
	//
	struct sockaddr_in	m_LocalSockAddr;
	
	//////////
	// Our socket.
	//
	SOCKET              m_Socket;
	
	//////////
	// Remote host portion of the URL.
	//
	TString				m_RemoteHost;
	
	//////////
	// Everything after the remote host in the URL.
	//
	TString				m_RemoteURI;
	
	//////////
	// IP Address of remote host.
	//
	unsigned long		m_RemoteIPAddr;
	
	//////////
	// Port used to connect to remote host.
	//
	int					m_RemotePort;
};

#endif // _LHttp_h_

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
