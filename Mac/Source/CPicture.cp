//
//	CPicture.cp
//

#include "THeader.h"

#include <stdio.h>
#include <PictUtils.h>

#include "TLogger.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CPicture.h"
#include "CResource.h"
#include "CPictDataFile.h"
#include "CPlayerView.h"
#include "TVariable.h"
#include "CModule.h"

USING_NAMESPACE_FIVEL

//
//	CPicture - Constructor
//
CPicture::CPicture(TString &inName) : CResource(inName)
{
	m_Qtg = NULL;
	
	m_Name = inName;
	m_FullPath = gModMan->GetGraphicsPath(m_Name);

	// make sure we have an extension, default to .pic
	if (not m_FullPath.Contains("."))
		m_FullPath += ".pic";
		
	gDebugLog.Log("New picture: full path <%s>", m_FullPath.GetString());

	// all pictures have some extension when they get here
	//	 put H before the . to see if we find a hilite picture	
	//	 with the same extension
	int			nameLen = inName.Length();
	
	for (int i = 0; i < nameLen; i++)
	{
		if (inName(i) == '.')
			m_HiliteName += 'H';
		m_HiliteName += inName(i);
	}
	
	Load(true);
}

//
//	~CPicture - Destructor
//
CPicture::~CPicture()
{
	if (m_Qtg != NULL)
		delete m_Qtg;
}

void CPicture::Load(bool firstTime)
{
	CResource::Load();
	
	if (not firstTime)
		gPictureManager.CheckMemory();
}

//
//	_Load - Load the picture into memory.
//
void CPicture::_Load(void)
{
	if (IsUnloaded())
	{			
		m_Qtg = new QTGraphic(m_FullPath);
		
		if (m_Qtg != NULL)
		{
			m_Height = m_Qtg->Height();
			m_Width = m_Qtg->Width();
			
			SetSize(m_Qtg->Size());
		}
	}
}	
	
//
//	_Purge - Unload the picture.
//
void CPicture::_Purge(void)
{	
	if (IsLoaded())
	{
		if (m_Qtg != NULL)
		{
			delete m_Qtg;
			m_Qtg = NULL;
		}
		
		SetSize(0);
	}
}

//
//	UpdatePriority
//
void CPicture::UpdatePriority(void)
{
	gPictureManager.Update(this);
}

//
//	Draw - Draw the picture.
//
void CPicture::Draw(TPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte /* = false */)
{
	if (IsUnloaded())
		Load();
	
	m_Origin = inPt;
			
	PP::StColorPenState::Normalize();
	
	if (m_Qtg != NULL)
		m_Qtg->Draw(inGWorldPtr, inPt, inMatte);		
}

//
//	Draw - Draw the picture.
//
void CPicture::Draw(TPoint &inPt, GWorldPtr inGWorldPtr, TRect &inRect)
{
	if (IsUnloaded())
		Load();
		
	m_Origin = inPt;
	
	PP::StColorPenState::Normalize();
	
	if (m_Qtg != NULL)
		m_Qtg->Draw(inGWorldPtr, inPt, inRect);
}

//
//	Hilite - Draw the hilite picture.
//
void CPicture::Hilite(TPoint &inPt, GWorldPtr inGWorldPtr, bool inMatte /* = true */)
{
	CPicture	*hiPict = NULL;
	
	hiPict = GetHilitePicture();
	if (hiPict != NULL)
	{
		hiPict->Draw(inPt, inGWorldPtr, inMatte);
		gPlayerView->Draw(nil);
	}
	
	Draw(inPt, inGWorldPtr, inMatte);
	gPlayerView->Draw(nil);
}

//
//	GetHilitePicture - Return the hilite picture.
//
CPicture *CPicture::GetHilitePicture(void)
{
	CPicture	*hilitePict = NULL;
	
	hilitePict = gPictureManager.GetPicture(m_HiliteName);
	return (hilitePict);
}

//
//	GetColorTable - Get the Color Table out of the graphic.
//
CTabHandle CPicture::GetColorTable(void)
{
	CTabHandle	retCTab = NULL;
	
	if (IsUnloaded())
		Load();
		
	if (m_Qtg != NULL)
		retCTab = m_Qtg->GetColorTable();
		
	return (retCTab);
}	

//
//	GetBounds - Get the bounds of the graphic.
//
TRect CPicture::GetBounds(void)
{
	TRect	bounds;
	
	if (IsUnloaded())
		Load();
		
	bounds.Set(0, 0, m_Height, m_Width);
		
	return (bounds);
}	

//
//	SetSize - 
//
void CPicture::SetSize(uint32 inNewSize)
{
	int		oldSize = GetSize();
	
	if (oldSize != inNewSize)
	{
		CResource::SetSize(inNewSize);
		
		gPictureManager.ChangeResSize(inNewSize, oldSize);
	}
}		

//
//  GetPicture - Get the given picture. If it's not there add it to the
//  resource list.
//
CPicture *CPictureManager::GetPicture(TString &inName)
{
	TString		thePictName;
    CPicture	*thePict = NULL;

	thePict = (CPicture *) GetResource(inName);
	if (thePict == NULL)
	{
		thePict = new CPicture(inName);
		if (thePict != NULL)
			AddResource(thePict);
	}
	else
		thePict->Load();
		
    return (thePict);
}



