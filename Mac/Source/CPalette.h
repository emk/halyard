//
//	CPalette.h
//

#ifndef _H_CPALETTE
#define _H_CPALETTE

#include "KHeader.h"
#include "KString.h"

#include "CResource.h"
#include "QTGraphic.h"

class CPalette : public CResource
{
	public:
					CPalette(KString &inName);
		virtual		~CPalette();
	
		virtual void	Load(bool firstTime = false);	
		virtual void	_Load(void);
		virtual void	_Purge(void);

		CTabHandle	GetColorTable(void);
		RGBColor	GetColor(int32 inIndex);
		
		void		SetSize(uint32 inNewSize);
		void		UpdatePriority(void);
		
	protected:
		CTabHandle	m_CTab;	
		QTGraphic	*m_Qtg;
		KString		m_FullPath;
		bool		m_ClutResource;	
		
		void		LoadClutResource();
		void		LoadClutQT();
};

class CPaletteManager : public CResourceManager
{
	public:
					CPaletteManager();
					~CPaletteManager();
					
		void		Init(void);
		
		CPalette	*GetPalette(KString &inName);
		CPalette	*GetCurrentPalette(void) { return (m_GraphicsPal); }
		void		SetPalette(CPalette *inPal, bool inGraphPal);
		RGBColor	GetColor(int32 inIndex);
		void		RemoveAll(void);
		
		void		ResetPalette(void);
		void		CheckPalette(void);
		
		bool		HaveNewPal(void) { return (m_HaveNewPal); }
		
	protected:
		CPalette	*m_GraphicsPal;
		CPalette	*m_VideoPal;
		
		bool		m_HaveNewPal;		
};	

extern CPaletteManager gPaletteManager;	
		
#endif
