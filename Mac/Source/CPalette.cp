//
//	CPalette.cp
//

#include "THeader.h"

#include "KLogger.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CResource.h"
#include "CPalette.h"
#include "CVariable.h"
#include "CModule.h"
#include "CPlayerView.h"

#define DEFAULT_PAL_SIZE	2048

USING_NAMESPACE_FIVEL

CPalette::CPalette(TString &inName) : CResource(inName)
{	
	TString		name = inName;
	
	m_CTab = NULL;
	m_Qtg = NULL;
	m_ClutResource = false;
	
	m_FullPath = gModMan->GetCLUTPath(name);
	
	// make sure there is an extension
	if (not m_FullPath.Contains("."))
		m_FullPath += ".clut";
		
	// see what extension we have
	if (m_FullPath.Contains(".clut"))
		m_ClutResource = true;
	
	SetSize(0);
	
	Load(true);
}

CPalette::~CPalette()
{
	if (IsLoaded())
		_Purge();	// don't call Purge() as that will check kResLocked
}

//
//	Load - 
//
void CPalette::Load(bool firstTime /* = false */)
{
	CResource::Load();
	
	if (firstTime)
		gPaletteManager.CheckMemory();
}

//
//	_Load - Load this palette into memory.
//
void CPalette::_Load()
{	
	if (not IsLoaded())
	{
		m_CTab = NULL;
		m_Qtg = NULL;
		
		if (m_ClutResource)
			LoadClutResource();
		else
			LoadClutQT();		
	}
}

//
//	LoadClutResource
//
void CPalette::LoadClutResource(void)
{
	FSSpec		theFSSpec;
		
	if (not theConfig->FillSpec(&theFSSpec, m_FullPath))
	{
#ifdef DEBUG
		gDebugLog.Log("ERROR: Couldn't find palette <%s>", m_FullPath.GetString());
#else	
		gLog.Caution("Could not find CLUT <%s>", m_FullPath.GetString());
#endif
		return;
	}
	
	try
	{
		// Now that we have a valid FSSpec, we can create the file stream & get
		// the picHandle from the file.
				
		PP::LFile *theFile = new PP::LFile(theFSSpec);
		
		theFile->OpenResourceFork(fsCurPerm);
		
		m_CTab = ::GetCTable(128);

#ifdef DEBUG
		//gDebugLog.Log("Loaded palette: <%s>, size <%ld>", key.GetString(), DEFAULT_PAL_SIZE);
#endif
		SetSize(DEFAULT_PAL_SIZE);
					
		theFile->CloseResourceFork();
		delete theFile;
	}
	
	catch (const PP::LException& inException) 
	{
#ifdef DEBUG
		gDebugLog.Log("Couldn't get palette from file");
#else
		gLog.Caution("Error getting palette <%s>", m_FullPath.GetString());
#endif
	} 
}

//
//	LoadClutQT
//
void CPalette::LoadClutQT(void)
{
	m_Qtg = new QTGraphic(m_FullPath);
	if (m_Qtg != NULL)
	{
		m_CTab = m_Qtg->GetColorTable();
		
		if (m_CTab != NULL)
			SetSize(DEFAULT_PAL_SIZE);			
		
		//delete m_Qtg;
		//m_Qtg = NULL;
	}
}

//
//	_Purge
//
void CPalette::_Purge()
{
	if (IsLoaded())
	{	
		if (m_CTab != NULL)
		{
			::DisposeCTable(m_CTab);
			m_CTab = nil;
			
			SetSize(0);
		}
		
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
void CPalette::UpdatePriority(void)
{
	gPaletteManager.Update(this);
}

//
//	GetColorTable
//
CTabHandle CPalette::GetColorTable(void)
{
	if (IsUnloaded())
		Load();
		
	return (m_CTab);
}	

//
//	GetColor
//
RGBColor CPalette::GetColor(int32 inIndex)
{
	RGBColor	retColor = {0, 0, 0};
	
	if (IsUnloaded())
		Load();
		
	if (m_CTab != NULL)
		retColor = (**m_CTab).ctTable[inIndex].rgb;	
		
	return (retColor);
}

//
//	SetSize - 
//
void CPalette::SetSize(uint32 inNewSize)
{
	uint32		oldSize = GetSize();
	
	if (oldSize != inNewSize)
	{
		CResource::SetSize(inNewSize);
		
		gPaletteManager.ChangeResSize(inNewSize, oldSize);
	}
}

//
//	CPaletteManager methods
//

//
//	CPaletteManager
//
CPaletteManager::CPaletteManager()
{
	m_GraphicsPal = NULL;
	m_VideoPal = NULL;
	m_HaveNewPal = false;
}

CPaletteManager::~CPaletteManager()
{
}

//
//	Init - Set the initial palette.
//
void CPaletteManager::Init(void)
{
	CTabHandle	ctab = NULL;
	ctab = ::GetCTable(128);
	
	if (ctab != NULL)
	{
		PaletteHandle	newPal = NULL;
		PaletteHandle	oldPal = NULL;
		
		newPal = ::NewPalette((**ctab).ctSize+1, ctab, pmTolerant + pmExplicit, 0);	
		oldPal = ::GetPalette(gWindow);
	
		::NSetPalette(gWindow, newPal, pmNoUpdates);	// was pmAllUpdates
		
		gPlayerView->DoNewPalette(ctab);
		::ActivatePalette(gWindow);
		
		if (oldPal != NULL)
			::DisposePalette(oldPal);
	}
}

//
//	GetPalette - 
//
CPalette *CPaletteManager::GetPalette(TString &inName)
{
	CPalette	*thePal = NULL;
	TString		newName;

	// make sure only the .clut extension is used
	if (inName.Contains("."))
	{
		int32	charPos = inName.Find(".");
		
		newName = inName.Mid(0, charPos);
	}
	else
		newName = inName;
		
	newName += ".clut";
			
	thePal = (CPalette *) GetResource(newName);
	if (thePal == NULL)
	{
		thePal = new CPalette(newName);
		if (thePal != NULL)
			AddResource(thePal);
	}
	else
		thePal->Load();
		
	return (thePal);
}

//
//	SetPalette - 
//
void CPaletteManager::SetPalette(CPalette *inPal, bool inGraphPal)
{
	PaletteHandle	oldPal = NULL;
	PaletteHandle	newPal = NULL;
	CTabHandle		ctab = NULL;
	CPalette		*prevPal = NULL;
	
	if (inPal == NULL)
		return;
		
	ctab = inPal->GetColorTable();
	
	newPal = ::NewPalette((**ctab).ctSize+1, ctab, pmTolerant + pmExplicit, 0);	
	oldPal = ::GetPalette(gWindow);
	
	::NSetPalette(gWindow, newPal, pmNoUpdates);	// was pmAllUpdates
	
	m_HaveNewPal = true;
	if (inGraphPal)
	{
		prevPal = m_GraphicsPal;
		m_GraphicsPal = inPal;
		gPlayerView->DoNewPalette(ctab);
		
		if (theConfig->GetBitDepth() > 8)
			CheckPalette();
			
		gVariableManager.SetString("_GraphPal", inPal->Key());
#ifdef DEBUG
	//	gDebugLog.Log("Setting _GraphPal to <%s>", Key());
#endif

	}
	else
	{
		prevPal = m_VideoPal;
		m_VideoPal = inPal;
		CheckPalette();
	}
	
	if (oldPal != NULL)
		::DisposePalette(oldPal);
		
	if (prevPal != NULL)
		prevPal->Purge();	
}

//
//	GetColor
//
RGBColor CPaletteManager::GetColor(int32 inIndex)
{
	CPalette	*curPal = NULL;
	RGBColor	retColor = {0, 0, 0};
	
	curPal = m_GraphicsPal;
	if (curPal != NULL)
		retColor = curPal->GetColor(inIndex);
		
	return (retColor);
}

//
//	RemoveAll - 
//
void CPaletteManager::RemoveAll(void)
{
	CResourceManager::RemoveAll();
	
	m_GraphicsPal = NULL;
	m_VideoPal = NULL;
}

//
//	ResetPalette -
//
void CPaletteManager::ResetPalette(void)
{
	CPalette	*thePal = NULL;
	
	if (m_GraphicsPal != NULL)
	{
		thePal = m_GraphicsPal;
		thePal->Load();
		m_GraphicsPal = NULL;	// will be reset immediately
		SetPalette(thePal, true);
	}		
}

//
//	CheckPalette -
//
void CPaletteManager::CheckPalette(void)
{
	if (m_HaveNewPal)
	{
		m_HaveNewPal = false;
		::ActivatePalette(gWindow);
	}
}


	