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

#include "KCommon.h"
#include "KString.h"
#include "KRect.h"
#include "KPoint.h"

class QTGraphic
{
public:
				QTGraphic();
				QTGraphic(KString &inPath);
				~QTGraphic();

	void		Create(KString &inPath);

	void		Draw(GWorldPtr inGWorld, KPoint &inPt, bool inTrans = false);
	void		Draw(GWorldPtr inGWorld, KPoint &inPt, KRect &inRect);
	

	long		Width(void);
	long		Height(void);
	uint32		Size(void) { return (m_size); }
	long		BitCount(void);
	CTabHandle	GetColorTable(void);

protected:
	void		Init(void);
	void		Toss(void);
	bool		SetQTGWorld(GWorldPtr inGWorld);
	bool		SetTransparent(void);
	bool		SetDestRect(KRect &inRect);
	bool		NeedClip(KRect &inRect);

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

	GraphicsImportComponent	m_gi;
	ImageDescriptionHandle	m_idh;
	KString					m_path;
	uint32					m_size;
	bool					m_transparent;
};

#endif // _QTGraphic_h_

/*
 $Log$
 Revision 1.1  2000/05/11 13:00:22  chuck
 v 2.01 b1

*/
