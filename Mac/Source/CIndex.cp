/*  CIndex.c

    Routines for reading script indices from disk and loading
    headers, macros and cards into memory. Some files may be
    encrypted; these are decrypted once they are read into
    memory.
*/

#include "THeader.h"

#include <string.h>
#include <stdlib.h>

#include "KLogger.h"

#include "CMac5LApp.h"
#include "CIndex.h"
#include "CCard.h"
#include "CHeader.h"
#include "CMacroManager.h"
#include "CParser.h"
#include "CModule.h"

#include "MacUtils.h"

USING_NAMESPACE_FIVEL

//
//	CIndex methods
//

//
//	CIndex - Constructor
//
CIndex::CIndex(CIndexFile *inFile, const char *inName, int32 inStart, int32 inEnd) 
	: TBNode(inName)
{  
	m_File = inFile;
	if (m_File != NULL)
		m_File->AddReference();
	m_Start = inStart;
	m_End = inEnd;                                                                   
}

CIndex::~CIndex()
{
	if (m_File != NULL)
		m_File->RemoveReference();
}

//
//	SetScript - Read in the text for this index from the file.
//
bool CIndex::SetScript()
{
	int32	length;
	int		count;
    char    *str;

	// see if we have already read the script in
    if (not m_Script.IsEmpty())
        return (true);

    length = (m_End - m_Start) + 1;

    m_Script.Resize(length + 1);
    str = m_Script.GetBuffer();

	// read card from the script file:
    m_File->Seek(m_Start);
    count = m_File->Read(str, length);

    if (count < length)
	{
        gLog.Error("I/O error reading data for index %s.", Key());
		return (false);
	}

    //  Set the terminating 0 and tell the string to recalc its
    //  length since we did some direct manipulation.
    //
    str[length] = '\0';
    m_Script.Update();

	return (true);
}

//
//	FlushScript - Toss the contents of the script.
//
void CIndex::FlushScript()
{
    m_Script.Empty();
}

//
//	GetScript - 
//
const char *CIndex::GetScript()
{
    return (m_Script.GetString());
}

//
//	CIndexManager methods

//
//	CIndexManager - Constructor
//
CIndexManager::CIndexManager() : TBTree()
{  
}

CIndexManager::~CIndexManager()
{
	if (m_Root != NULL)
		RemoveAll();
}

//
//	CIndexFileManager methods
//

CIndexFileManager::CIndexFileManager() : TBTree()
{
}

CIndexFileManager::~CIndexFileManager()
{
}

//
//	NewIndex - 
//
bool CIndexFileManager::NewIndex(const char *inName)
{
	CIndexFile	*newIndex = NULL;
	bool		retValue = false;
	
	// make sure we don't already have this node
	newIndex = (CIndexFile *) Find(inName);
	if (newIndex != NULL)
	{
		gLog.Error("Trying to open script <%s> but it is already open.",
			inName);
		return (false);
	}
	
	newIndex = new CIndexFile(inName);
	if (newIndex == NULL)
	{
		gLog.Error("Could not allocate memory for script <%s>",
			inName);
		return (false);
	}

	retValue = newIndex->Init();
	if (not retValue)
		delete newIndex;
	else
	{
		Add(newIndex);
#ifdef DEBUG
		newIndex->Close();
#endif
	}

	return (retValue);
}

bool CIndexFileManager::NewIndex(FSSpec *inSpec)
{
	TString		fullPath;
	
	ASSERT(inSpec != NULL);
	
	fullPath = PathFromFSSpec(inSpec);
	return (NewIndex(fullPath));
}

//
//	CIndexFile methods
//
CIndexFile::CIndexFile(const char *inName) : TBNode(inName)
{	
	m_ReferenceCount = 0;
	m_AtEnd = false;
}

CIndexFile::~CIndexFile()
{
	m_File.close();
}

void CIndexFile::AddReference()
{
	m_ReferenceCount++;
}

void CIndexFile::RemoveReference()
{
	m_ReferenceCount--;

	if (m_ReferenceCount <= 0)
	{
		// remove ourselves from the index file tree
		gIndexFileManager.Remove(Key());
	}
}

bool CIndexFile::Open(const char *inPath)
{
	m_File.open(inPath, ios::in | ios::binary);

	if (not m_File.is_open())
		return (false);
	return (true);
}

bool CIndexFile::IsOpen()
{
	if (m_File.is_open())
		return (true);
	return (false);
}

void CIndexFile::Seek(int32 inPos)
{
	m_File.seekg(inPos);
}

int32 CIndexFile::GetPos()
{
	return (m_File.tellg());
}

bool CIndexFile::AtEnd()
{
	return (m_AtEnd);
}

int32 CIndexFile::Read(char *inBuffer, int32 inLength)
{
	int32	bytesRead = -1;

	ASSERT(inBuffer != NULL);
	ASSERT(inLength > 0);

	m_File.read(inBuffer, inLength);
	bytesRead = m_File.gcount();

	if (m_File.eof())
	{
		m_AtEnd = true;
		m_File.clear();
	}

	return (bytesRead);
}

void CIndexFile::Close()
{
	m_File.close();
}

//
//	Init - Initialize the index file/script file pair. Open the 
//		index file, read all the info, initialize headers, cards
//		and macros.
//
bool CIndexFile::Init()
{
	ifstream		theIndexFile;
	TString			scriptName; 
    TString			theName;

	// see if the script name is a full path or just a file name
	//	if it doesn't contain at least one ':', assume it is just a 
	//	file name
	scriptName = Key();
	if (not scriptName.Contains(":"))
	{  
		scriptName = gModMan->GetScriptPath();
		scriptName += Key();
	}
	
	// make sure the path has the .scr extension
	if (not scriptName.Contains(".scr", false))
    	scriptName += ".scr";

    // Open the script file.
	if (not Open(scriptName))
	{
        gLog.Error("Couldn't open script file <%s>.", scriptName.GetString());
		return (false);
	}

	// Instantiate a parser to check out the script file and initialize all the 
	//	index information.
	{
		CParser		theParser;
#ifdef DEBUG
		//DWORD		startTime;
		//DWORD		endTime;

		//startTime = ::timeGetTime();
#endif

		if (not theParser.Parse(this))
		{
			gLog.Error("There are errors in the script file <%s>", scriptName.GetString());
			return (false);
		}

#ifdef DEBUG
		//endTime = ::timeGetTime();

		//gDebugLog.Log("It took <%ld> milli-seconds to parse the script file",
		//	endTime - startTime);
#endif
	}
	
   return (true);

}