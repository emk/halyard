/*
	CFile.h

	Support for reading and writing to and from text files.

*/

#pragma once

#include "KHeader.h"

#include "KArray.h"
#include "CTextFileStream.h"

enum FileKind 
{
	fWriteAppend,
	fWriteNew,
	fReadOnly
};


class CFile : public KObject 
{
	private:
		KString		itsName;
		FileKind	itsKind;
		CTextFileStream		*itsFile;
		char		readBuf[BUFFER_SIZE];
		char		writeBuf[BUFFER_SIZE];
				
	public:
					CFile(KString &filename, FileKind fKind = fReadOnly);
		virtual 	~CFile();

		int			Match(const char *aName);
		void		Read(KString &str);
		void		ReadUntil(KString &str, unsigned char delim);
		void		ReadUntilCore(KString &str, unsigned char delim);
		void		Write(KString &data);
		void		Lookup(KString &searchString, int32 numFields);
		void		Rewrite(KString &searchString, int32 numFields);
		bool		AtEOF(void);
};

class CFileList : public KObject 
{
	public:
 					CFileList();
		virtual 	~CFileList();

		void		Open(KString &filename, FileKind fKind);
		void		Close(KString &filename);
		void		Read(KString &filename, KString &str);
		void		ReadUntil(KString &filename, KString &str,
							unsigned char delim);
		void		Write(KString &filename, KString &data);
		void		Lookup(KString &filename, KString &searchString,
							int numFields);
		void		Rewrite(KString &filename, KString &searchString,
							int numFields);
							
		bool		CurFileOpen(void);
		bool		CurFileAtEOF(void);
		
	protected:
		CFile		*CurrentFile;
		
		CFile		*FindFile(KString &filename, int failClosed);
		void		CheckPath(KString &inPath);
};

extern CFileList gFileManager;
