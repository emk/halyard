//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, 2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// QTGraphic.h : 
//

#if !defined (_QTGraphic_h_)
#define _QTGraphic_h_

#include "TCommon.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"

BEGIN_NAMESPACE_FIVEL

class QTGraphic
{
public:
				QTGraphic();
				QTGraphic(TString &inPath);
				~QTGraphic();

	void		Create(TString &inPath);

	void		Draw(GWorldPtr inGWorld, TPoint &inPt, bool inTrans = false);
	void		Draw(GWorldPtr inGWorld, TPoint &inPt, TRect &inRect);
	

	long		Width(void);
	long		Height(void);
	uint32		Size(void) { return (m_size); }
	long		BitCount(void);
	CTabHandle	GetColorTable(void);
	
	inline bool	HaveGraphic(void)
	{ 
		if (m_gi != NULL) 
			return (true); 
		else 
			return (false); 
	}
	inline bool HaveInfo(void)
	{ 
		if (m_idh != NULL) 
			return (true); 
		else 
			return (false); 
	}

protected:
	void		Init(void);
	void		Toss(void);
	bool		SetQTGWorld(GWorldPtr inGWorld);
	bool		SetTransparent(void);
	bool		SetDestRect(TRect &inRect);
	bool		NeedClip(TRect &inRect);

	GraphicsImportComponent	m_gi;
	ImageDescriptionHandle	m_idh;
	TString			m_path;
	uint32					m_size;
	bool					m_transparent;
};

END_NAMESPACE_FIVEL

#endif // _QTGraphic_h_

/*
 $Log$
 Revision 1.3  2002/07/17 16:07:32  brian
 3.3.15 - 17 July 2002 - brian

   * Fixed excessive error messages for missing button graphics (#1039).
   * This bug might still occur on Windows; it needs to be looked into.

 Revision 1.2  2002/03/04 15:42:51  hamon
 Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.
Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.
Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.1  2000/05/11 13:00:22  chuck
 v 2.01 b1

*/
