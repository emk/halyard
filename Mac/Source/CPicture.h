//
//	CPicture.h
//

#ifndef _H_CPICTURE
#define _H_CPICTURE

#include "KHeader.h"

#include "CResource.h"
#include "KRect.h"
#include "KPoint.h"
#include "CGWorld.h"
#include "QTGraphic.h"

class CPicture : public CResource
{
	public:
						CPicture(KString &inName);
		virtual			~CPicture();
	
		virtual void	Load(bool firstTime = false);	
		virtual void	_Load(void);
		virtual void	_Purge(void);
		void			Draw(KPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte = false);
		void			Draw(KPoint &inPt, GWorldPtr inGWorldPtr, KRect &inRect);
		void			Hilite(KPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte = true);
		CPicture		*GetHilitePicture(void);
				
		KRect			GetBounds(void);
		CTabHandle		GetColorTable(void);
		
		void			SetSize(uint32 inNewSize);
		void			UpdatePriority();
		
	protected:
		KString			m_FullPath;
		KString			m_Name;
		KString			m_HiliteName;
		KPoint			m_Origin;
		int32			m_Width;
		int32			m_Height;
		QTGraphic		*m_Qtg;	
};

class CPictureManager : public CResourceManager
{
	public:
					CPictureManager() {}
					~CPictureManager() {}
					
		CPicture	*GetPicture(KString &inName);
};

extern CPictureManager gPictureManager;

#endif
