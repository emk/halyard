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
// KURL.h : 
//

#if !defined (_LURL_h_)
#define _LURL_h_

#include "KString.h"

class KURL : public KString
{
public:
				KURL();
				KURL(const KString &inStr);
	virtual		~KURL();

	enum URLType { FtpURL, HttpURL, FileURL, RtspURL, UnknownURL };

	KString		GetServerPart();
	KString		GetDirPart();
	KString		GetFirstDir();
	KString		GetFileName();
	KString		GetPath();

	KURL		&operator=(const KString &inStr);
	KURL		&operator+=(const KString &inStr);

	bool		IsLegal();
	bool		IsLocal();
	bool		IsRemote();
	bool		IsFile();
	bool		IsHTTP();
	bool		IsFTP();
	bool		IsRTSP();

protected:
	bool		CheckSyntax();

	KString		m_ServerPart;
	KString		m_DirPart;
	URLType		m_Type;
	bool		m_HaveGoodURL;
};

#endif // _LURL_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
