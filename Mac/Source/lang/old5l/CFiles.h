/*
	CFile.h

	Support for reading and writing to and from text files.

*/

#pragma once

#include "THeader.h"

#include "TArray.h"
#include "CTextFileStream.h"

BEGIN_NAMESPACE_FIVEL

enum FileKind 
{
	fWriteAppend,
	fWriteNew,
	fReadOnly
};

class CFile : public TObject 
{
	private:
		TString		itsName;
		FileKind	itsKind;
		CTextFileStream		*itsFile;
		char		readBuf[BUFFER_SIZE];
		char		writeBuf[BUFFER_SIZE];
				
	public:
					CFile(TString &filename, FileKind fKind = fReadOnly);
		virtual 	~CFile();

		int			Match(const char *aName);
		void		Read(TString &str);
		void		ReadUntil(TString &str, unsigned char delim);
		void		ReadUntilCore(TString &str, unsigned char delim);
		void		Write(TString &data);
		void		Lookup(TString &searchString, int32 numFields);
		void		Rewrite(TString &searchString, int32 numFields);
		bool		AtEOF(void);
};

class CFileList : public TObject 
{
	public:
 					CFileList();
		virtual 	~CFileList();

		void		Open(TString &filename, FileKind fKind);
		void		Close(TString &filename);
		void		Read(TString &filename, TString &str);
		void		ReadUntil(TString &filename, TString &str,
							unsigned char delim);
		void		Write(TString &filename, TString &data);
		void		Lookup(TString &filename, TString &searchString,
							int numFields);
		void		Rewrite(TString &filename, TString &searchString,
							int numFields);
							
		bool		CurFileOpen(void);
		bool		CurFileAtEOF(void);
		
		//////////
		// A SpecialVariableFunction to get the _eof variable.
		//
		static TString		ReadSpecialVariable_eof();	
		
	protected:
		CFile		*CurrentFile;
		
		CFile		*FindFile(TString &filename, int failClosed);
		void		CheckPath(TString &inPath);
};

extern CFileList gFileManager;

END_NAMESPACE_FIVEL
