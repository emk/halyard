/*******************************************

    CIndex.h

    Routines to get scripts, headers, and
    macros from text files.

********************************************/

#ifndef _H_CINDEX
#define _H_CINDEX

#include <fstream.h>

#include "KCommon.h"
#include "KBTree.h"

#include "CStream.h"

class CIndexFile;

class CIndex : public KBNode 
{
    public:
					CIndex(CIndexFile *inIndex, const char *inName = NULL,
							int32 inStart = 0, int32 inEnd = 0);
		virtual		~CIndex();
		
        //CIndex(const char *name = 0, long p1 = 0, long p2 = 0);

        virtual const char *Name(void) { return (Key()); }

        //
        //  Commands to get at the index' raw data.
        //
        virtual bool SetScript(void);
        virtual const char *GetScript(void);
        virtual void FlushScript(void);
    
    protected:
    	CIndexFile	*m_File;
    	int32		m_Start;
    	int32		m_End;
        CStream    	m_Script;

};

class CIndexManager : public KBTree 
{
    public:

        			CIndexManager();
        virtual		~CIndexManager();

		virtual void	MakeNewIndex(CIndexFile * /* inFile */, const char * /* inName */, 
									int32 /* inStart */, int32 /* inEnd */) {}
};

class CIndexFile : public KBNode
{
	public:
					CIndexFile(const char *inName);
		virtual		~CIndexFile();
		
		bool		Init();
		
		bool		IsOpen();
		int32		GetPos();
		bool		AtEnd();	
		void		Seek(int32 inPos);
		int32		Read(char *inBuffer, int32 inLength);
		void		Close();
		
		void		AddReference();
		void		RemoveReference();
		
	protected:
		bool		Open(const char *inPath);
		
		
		ifstream	m_File;
		bool		m_AtEnd;
		int32		m_ReferenceCount;
};

class CIndexFileManager : public KBTree
{
	public:
					CIndexFileManager();
		virtual		~CIndexFileManager();
		
		bool		NewIndex(const char *inName);
		bool		NewIndex(FSSpec *inSpec);
};

extern CIndexFileManager gIndexFileManager;

//
//  Initializer for all indices.
//

//bool InitIndex(FSSpec *scriptSpec, FSSpec *indexScript, bool fEncrypted);
//void KillIndex(void);
//bool CheckFileDates(FSSpec *inIndexSpec, FSSpec *inScriptSpec);

#endif
