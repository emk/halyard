//
//	CPalette.h
//

#ifndef _H_CPALETTE
#define _H_CPALETTE

#include "Mac5L.h"

#include "CResource.h"

class CPalette : public CResource
{
	public:
					CPalette(const char *name);
		virtual		~CPalette();
		
		OVERRIDE void	_Load(void);
		OVERRIDE void	_Purge(void);
		
		void		SetPalette(bool inGraphics);
		CTabHandle	GetCTab(void) { return(m_ctab); }
		
	private:
		CTabHandle	m_ctab;
		
};

CPalette *GetPalette(const char *name);
		
#endif
