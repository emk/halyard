//
//	CPicture.h
//

#ifndef _H_CPICTURE
#define _H_CPICTURE

#include "THeader.h"

#include "CResource.h"
#include "TRect.h"
#include "TPoint.h"
#include "CGWorld.h"
#include "QTGraphic.h"

BEGIN_NAMESPACE_FIVEL

class CPicture : public CResource
{
	public:
						CPicture(TString &inName);
		virtual			~CPicture();
	
		virtual void	Load() { Load(false); }	
		virtual void	Load(bool firstTime);	
		virtual void	_Load(void);
		virtual void	_Purge(void);
		void			Draw(TPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte = false);
		void			Draw(TPoint &inPt, GWorldPtr inGWorldPtr, TRect &inRect);
		void			Hilite(TPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte = true);
		CPicture		*GetHilitePicture(void);
				
		TRect			GetBounds(void);
		CTabHandle		GetColorTable(void);
		
		void			SetSize(uint32 inNewSize);
		void			UpdatePriority();
		
		bool HaveGraphic() { return m_Qtg->HaveGraphic(); }
		bool HaveInfo() { return m_Qtg->HaveInfo(); }
		
	protected:
		TString			m_FullPath;
		TString			m_Name;
		TString			m_HiliteName;
		TPoint			m_Origin;
		int32			m_Width;
		int32			m_Height;
		QTGraphic		*m_Qtg;	
};

class CPictureManager : public CResourceManager
{
	public:
					CPictureManager() {}
					~CPictureManager() {}
					
		CPicture	*GetPicture(TString &inName);
};

extern CPictureManager gPictureManager;

END_NAMESPACE_FIVEL

#endif
