/*
	CFile.h

	Support for reading and writing to and from text files.

*/


#include "Mac5L.h"
#include "CArray.h"
#include "CTextFileStream.h"


enum FileKind 
{
	fWriteAppend,
	fWriteNew,
	fReadOnly
};


class CFile : public CObject 
{
	private:
		CString		itsName;
		FileKind	itsKind;
		CTextFileStream		*itsFile;
		char		readBuf[BUFFER_SIZE];
		char		writeBuf[BUFFER_SIZE];
				
	public:
					CFile(char *filename, FileKind fKind = fReadOnly);
		virtual 	~CFile();

		int			Match(const char *aName);
		void		Read(CString &str);
		void		ReadUntil(CString &str, unsigned char delim);
		void		ReadUntilCore(CString &str, unsigned char delim);
		void		Write(CString &data);
		void		Lookup(CString &searchString, int32 numFields);
		void		Rewrite(CString &searchString, int32 numFields);
		bool		AtEOF(void);
};

class CFileList : public CObject 
{
	private:
		CFile		*CurrentFile;
	
	protected:
		CFile		*FindFile(char *filename, int failClosed);
		void		CheckPath(char *inPath);

	public:
 					CFileList();
		virtual 	~CFileList();

		void		Open(char *filename, FileKind fKind);
		void		Close(char *filename);
		void		Read(char *filename, CString &str);
		void		ReadUntil(char *filename, CString &str,
							unsigned char delim);
		void		Write(char *filename, CString &data);

		void		Lookup(char *filename, CString &searchString,
							int numFields);
		void		Rewrite(char *filename, CString &searchString,
							int numFields);
							
		bool		CurFileOpen(void);
		bool		CurFileAtEOF(void);
};

extern CFileList gFileManager;
