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

//////////////////////////////////////////////////////////////////////////////
//
// LHttp.cpp : 
//

#include "stdafx.h"

#include "LHttp.h"
#include "Globals.h"

LHttp::LHttp()
{
	m_InitDone = false;
	Cleanup();
}

LHttp::~LHttp()
{
	if (m_InitDone)
		::WSACleanup();
}

bool LHttp::Init()
{
	if (m_InitDone)
		return (true);

	// WSA versions
	WSADATA		wsaData;
	BYTE		vCount = 3;
	int vInfo[3][3] = {
		{1,0,0},
		{1,1,0},
		{2,0,0}
	};
		
	WORD		vMMVersion = 0;
	int			nStartupStatus; 
	
	wsaData.wVersion = 0;

	while (vCount > 0)
	{
		vMMVersion = MAKEWORD(vInfo[vCount - 1][0], vInfo[vCount - 1][1]);

		// Init use of Windows sockets
		nStartupStatus = ::WSAStartup(vMMVersion, &wsaData);
		if (nStartupStatus == 0)
		{
			m_InitDone = true;
			break;
		}
		else
			vInfo[vCount - 1][2] = 0;
		::WSACleanup();
		vCount--;
	}

	if (not m_InitDone)
		return (false);

	return (true);
}

void LHttp::Cleanup()
{
	m_Socket = 0;
	m_RemoteSockAddr.sin_port = 0;
	m_LocalSockAddr.sin_port = 0;
	m_RemoteHost.Empty();
	m_RemoteURI.Empty();
	m_RemoteIPAddr = 0;
	m_RemotePort = -1;
}


LHttpError LHttp::CheckURL(TString &inURL, TString &inVarName)
{
	if (not m_InitDone)
		return (LHTTP_NotInited);

	Cleanup();

	// get the IP address for the host
	if (not GetHostInfo(inURL))
		return (LHTTP_BadURL);

	// set up the socket
	if (not SetUpSocket())
		return (LHTTP_SocketError);

	// send the request
	if (not SendRequest())
		return (LHTTP_SendError);

	// receive the reply
	LHttpError	replyValue = LHTTP_OK;
	replyValue = ReceiveReply(inVarName);

	// close the socket
	CloseSocket();
	
	return (replyValue);
}

bool LHttp::GetHostInfo(TString &inURL)
{
	int		tmpPos = 0;
	int		portPos = 0;
	int		endHost = 0;

	// parse the URL
	
	// has to start with 'http://'
	tmpPos = inURL.Find("http://");
	if (tmpPos != 0)
		return (false);

	// get the host 
	portPos = inURL.Find(":", 7);
	endHost = inURL.Find("/", 7);

	if ((portPos != -1) and (endHost != -1))
	{
		// have a port number
		TString		portStr;

		portStr = inURL.Mid(portPos, endHost - portPos);
		m_RemotePort = (long) portStr;
		if ((m_RemotePort < 0) or (m_RemotePort > 65535))
			return (false);

		m_RemoteHost = inURL.Mid(7, portPos - 7);
	}
	else if (endHost != -1)
	{
		m_RemoteHost = inURL.Mid(7, endHost - 7);
		m_RemotePort = 80;
	}
	else
	{
		// bad URL
		return (false);
	}

	if (m_RemoteHost.IsEmpty())
		return (false);

	// set the rest of the URL
	m_RemoteURI = inURL.Mid(endHost);
	if (m_RemoteURI.IsEmpty())
		m_RemoteURI = "/";

	// get the IP address for the host
	struct hostent FAR * hostAddr = NULL;

	hostAddr = gethostbyname((const char *) m_RemoteHost);
	if (hostAddr == NULL)
	{
	   // try find by ip
		unsigned long tmpAddr = 0;

		tmpAddr = inet_addr((const char *) m_RemoteHost);

		if (tmpAddr == INADDR_NONE)
			return (false);
		else
			// found by ip
			m_RemoteIPAddr = tmpAddr;
	}
	else
		// found by name
		m_RemoteIPAddr = *((long *)(hostAddr->h_addr));


	return (true);
}

bool LHttp::SetUpSocket()
{
	bool	retValue = false;
	struct	fd_set	fdSet;
	struct	timeval	tmvTimeout = {0L,0L};
	struct	linger zeroLinger;
	int		timeOut;

	// create the socket
	m_Socket = socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
	if (m_Socket == INVALID_SOCKET)
		return (false);

	// turn off time wait	
	zeroLinger.l_onoff = 1;	
	zeroLinger.l_linger = 0;

	if (setsockopt(m_Socket, SOL_SOCKET, SO_LINGER, (const char *) &zeroLinger, 
		sizeof(zeroLinger)) != 0)
	{
		goto done;
	}

	// set recieve timeout
	timeOut = 10;
	if (setsockopt(m_Socket, SOL_SOCKET, SO_RCVTIMEO, 
		(const char *) &timeOut, sizeof(timeOut)) != 0)
	{
		goto done;
	}

	// set send timeout
	if (setsockopt(m_Socket, SOL_SOCKET, SO_SNDTIMEO, 
		(const char *) &timeOut, sizeof(timeOut)) != 0)
	{
		goto done;
	}

	// local socket settings
	m_LocalSockAddr.sin_addr.s_addr = htonl (INADDR_ANY);
	m_LocalSockAddr.sin_family = AF_INET;
	
	// bind socket to local address
	if (bind (m_Socket, (const struct sockaddr FAR *) &m_LocalSockAddr,  
			 sizeof(m_LocalSockAddr)) == SOCKET_ERROR)
	{
		goto done;
	}

	// remote settings
	m_RemoteSockAddr.sin_addr.S_un.S_addr = m_RemoteIPAddr;
	m_RemoteSockAddr.sin_family = AF_INET;
	m_RemoteSockAddr.sin_port = htons(m_RemotePort);

	// allow status check when connect performed

	FD_ZERO(&fdSet);
	FD_SET(m_Socket, &fdSet); 

	if (select(0, &fdSet, NULL, NULL, &tmvTimeout) == SOCKET_ERROR)
	{
		goto done;
	}

	// try to connect
	if (connect(m_Socket, (const struct sockaddr FAR *) &m_RemoteSockAddr,
			sizeof(m_RemoteSockAddr)) == SOCKET_ERROR)
	{
		goto done;
	}

	// if still here, everything is OK
	retValue = true;

done:
	if (not retValue)
		closesocket(m_Socket);

	return (retValue);
}

bool LHttp::SendRequest()
{
	TString	 sendStr;

	sendStr = ("HEAD ");
	sendStr += m_RemoteURI;
	sendStr += " HTTP/1.0\r\n";
	sendStr += "\r\n";

	if (send (m_Socket, (const char *) sendStr, sendStr.Length(), 0) == SOCKET_ERROR)
		return (false);
	return (true);
}

LHttpError LHttp::ReceiveReply(TString &inVarName)
{
	TString		recStr;
	TString		resultStr;
	char		recBuffer[MAXBUFFERSIZE];
	int			recSize = 1;
	int			packetCount = 0;
	int			spacePos;
	long		resultCode;
	long		errorCode;

	while (true)
	{
		recSize = recv(m_Socket, (char *) recBuffer, (MAXBUFFERSIZE - 1), 0);

		if (recSize == SOCKET_ERROR)
			return (LHTTP_ReceiveError);

		if (recSize == 0)
			return (LHTTP_SocketClosed);

		recBuffer[recSize] = '\0';
		recStr = recBuffer;

		packetCount++;

		// parse the reply
		if (packetCount == 1)
		{
			// get the result code
			spacePos = recStr.Find(" ");
			if (spacePos != -1)
			{
				resultStr = recStr.Mid(spacePos + 1, 3);

				if (not inVarName.IsEmpty())
					gVariableManager.SetString(inVarName, resultStr.GetString());

				gVariableManager.SetString("_CheckURLResult", resultStr.GetString());

				resultCode = (long) resultStr;

				switch (resultCode)
				{
					case 200:
						errorCode = LHTTP_OK;
						break;
					case 404:
						errorCode = LHTTP_URLNotFound;
						break;
					default:
						errorCode = LHTTP_BadURL;
						break;
				}

				gDebugLog.Log("LHttp:ReceiveReply -> reply code <%d>", resultCode);
			}	
		}
		gDebugLog.Log("LHttp::ReceiveReply -> got <%s>", recStr.GetString());
	}

	return (LHTTP_OK);
}

void LHttp::CloseSocket()
{
	closesocket(m_Socket);
}

/*
 $Log$
 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

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
