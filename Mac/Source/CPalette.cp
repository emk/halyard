//
//	CPalette.cp
//

#include "debug.h"

#include "Mac5L.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CResource.h"
#include "CPalette.h"
#include "CVariable.h"

#define DEFAULT_PAL_SIZE	2048

CPalette::CPalette(const char *name) : CResource(name)
{
	size = DEFAULT_PAL_SIZE;
	
	Load();
}

CPalette::~CPalette()
{
	if (state > kResUnloaded)		// don't call Purge() as that will check kResLocked
		_Purge();
}

//
//	_Load - Load this palette into memory.
//
void CPalette::_Load()
{
	FSSpec			theFSSpec;
	bool			found_it;
	
	if (state == kResUnloaded)
	{
		m_ctab = nil;
		
		found_it = theConfig->FillCLUTSpec(&theFSSpec, (char *) key);
		
		if (not found_it)
		{
#ifdef DEBUG_5L
			prinfo("ERROR: Couldn't find palette <%s>", (char *) key);
#else	
			prcaution("Could not find CLUT <%s>", (char *) key);
#endif
			return;
		}
		
		Try_
		{
			// Now that we have a valid FSSpec, we can create the file stream & get
			// the picHandle from the file.
					
			LFile *theFile = new LFile(theFSSpec);
			
			theFile->OpenResourceFork(fsCurPerm);
			
			m_ctab = ::GetCTable(128);

#ifdef DEBUG_5L
			prinfo("loaded color table from file");
#endif
						
			theFile->CloseResourceFork();
			delete theFile;
		}
		
		Catch_(err)
		{
#ifdef DEBUG_5L
			prinfo("couldn't get palette from file, returned <%d>", err);
#else
			prcaution("Error getting palette <%s>", (char *) key);
#endif
		} EndCatch_
	}
}

//
//	_Purge
//
void CPalette::_Purge()
{
	Assert_(state > kResUnloaded);
	
	if (m_ctab != nil)
	{
		::DisposeCTable(m_ctab);
		m_ctab = nil;
	}
}

//
//	SetPalette - Set the current palette to this palette. 
//
void CPalette::SetPalette(bool inGraphics)
{
	char	str[32];
	char	*str_ptr;
	
	if (m_ctab == nil)
	{
#ifdef DEBUG_5L
		prinfo("Trying to set palette <%s>, not in memory", (char *) key);
#endif
		Load();
		
		if (m_ctab == nil)
		{
#ifdef DEBUG_5L
			prinfo("Still not in memory!!!");
#endif
			return;
		}
	}
		//return;
		
	// For graphics palettes, save the name.
	if (inGraphics)
	{
		// don't use the .clut postfix for the _graphpal
		strcpy(str, (char *) key);
		str_ptr = strstr(str, ".");
		if (str_ptr != nil)
			*str_ptr = '\0';
			
		gVariableManager.SetString("_graphpal", str);
#ifdef DEBUG_5L
		prinfo("Setting _graphpal to <%s>", str);
#endif
	}
	
	gTheApp->NewColorTable(this, inGraphics);
}			

//
//	GetPalette - See if the palette exists, if not, create a new one
//		and return it.
//		
CPalette *GetPalette(const char *name)
{
	CString		thePalName;
	CPalette	*thePal;
	char		*theStrPtr;
	
	if (name == nil)
		return (nil);
	
	// make sure the name doesn't end with .pic to start with	
	if ((theStrPtr = strstr(name, ".")) != NULL)
		*theStrPtr = '\0';
		
	thePalName = name;
	thePalName += ".clut";
		
	thePal = (CPalette *) GetResource(thePalName.GetString());
	
	if (thePal == NULL)
	{
		// create a new palette
		thePal = new CPalette(thePalName.GetString());
		
		gResManager.AddNode(thePal);
	}
	else
		thePal->Load();					// make sure it is in memory

	return (thePal);
}
