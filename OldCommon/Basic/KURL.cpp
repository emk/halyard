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
// KURL.cpp : 
//

#include "KHeader.h"
#include "KURL.h"

KURL::KURL() : KString()
{
}

KURL::KURL(const KString &inStr) : KString(inStr)
{
	CheckSyntax();
}

KURL::~KURL()
{
}

KURL &KURL::operator=(const KString &inStr)
{
	KString	*tmpStr = this;

	*tmpStr = inStr;
	CheckSyntax();

	return (*this);
}

KURL &KURL::operator+=(const KString &inStr)
{
	KString *tmpStr = this;

	*tmpStr += inStr;
	CheckSyntax();

	return (*this);
}

KString KURL::GetServerPart()
{
	KString		retStr;

	if (m_HaveGoodURL)
		retStr = m_ServerPart;

	return (retStr);
}

KString KURL::GetDirPart()
{
	KString		retStr;

	if (m_HaveGoodURL)
		retStr = m_DirPart;

	return (retStr);
}

//
//	GetFirstDir - Strip the first directory off the directory part.
//
KString KURL::GetFirstDir()
{
	KString		firstDir;

	if (not m_HaveGoodURL)
		return (firstDir);

	int32	firstPos;

	firstPos = Find('/');
	if (firstPos == -1)
		return (firstDir);
	firstPos++;

	int32	secondPos = Find('/', firstPos);

	if (secondPos == -1)
		firstDir = Mid(firstPos);
	else
		firstDir = Mid(firstPos, secondPos - firstPos);

	return (firstDir);
}

//
//	CheckSyntax - Check the syntax of the URL.
//
bool KURL::CheckSyntax()
{
	int32	pos = 0;

	m_HaveGoodURL = false;
	m_ServerPart.Empty();
	m_DirPart.Empty();
	m_Type = UnknownURL;

	// trim off leading and trailing spaces
	LTrim();
	RTrim();

	// check the service part
	if (StartsWith("http://"))
	{
		m_Type = HttpURL;
		pos = 7;
	}
	else if (StartsWith("ftp://"))
	{
		m_Type = FtpURL;
		pos = 6;
	}
	else if (StartsWith("file://"))
	{
		m_Type = FileURL;
		pos = 7;
	}
	else if (StartsWith("rtsp://"))
	{
		m_Type = RtspURL;
		pos = 7;
	}
	else
		return (false);

	// check the server part
	int32	pos2;

	pos2 = Find('/', pos);
	if (pos2 != -1)
	{
		if (pos2 != pos)
		{
			// grab the server part
			m_ServerPart = Mid(pos, pos2 - pos);
		}
		else
		{
			// no server part, only legal for file://
			if (m_Type != FileURL)
				return (false);
		}
	}
	else
	{
		// error, not enough slashes

		return (false);
	}

	// check the directory part, we just need something
	if (pos2 < (int32) Length())
		m_DirPart = Mid(pos2);	// grab the initial / too
	else
	{
		// error, no directory part

		return (false);
	}
	
	m_HaveGoodURL = true;
	return (true);
}

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
