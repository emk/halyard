//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// Parser.h : Front end for the IndexGen application. It takes a script file 
//			and parses it to find the start of commands.
//

#if !defined (_CParser_h_)
#define _CParser_h_

#include "KCommon.h"
#include "KString.h"

#include "CIndex.h"

//#include "LUtil.h"

#define PARSER_BUFFER_SIZE		512

#define ILLEGAL_TYPE	0
#define CARD_TYPE		1
#define HEADER_TYPE		2
#define MACRO_TYPE		3

class CParser
{

	public:
						CParser();
						~CParser();
	
		bool			Parse(CIndexFile *inFile);

	protected:
		int32			filePos;		// current position in file
		int32			curPos;			// current position in input buffer
		int32			curBufCount;	// number of characters in input buffer
		unsigned char	inBuffer[PARSER_BUFFER_SIZE];	// input buffer
		bool			bufAtEOF;		// has the last buffer been read in
		bool			atEOF;			// is curPos at the end?
		unsigned char	putCh;
		
		bool			haveErr;
		bool			inComment;
		bool			inEscape;
		int32			curLine;		// current line in the input file
		
		CIndexFile		*scriptFile;
				
		int32			findStart(void);
		int32			findClose(void);
		int32			findType(void);
		bool			findName(KString &inString);
		bool			getString(KString &inString);
		unsigned char	getRealChar(void);
		unsigned char	getChar(void);
		void			putChar(unsigned char ch);
		void			getBuffer(void);
		
};

#endif // _CParser_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:58:09  chuck
 v 2.01 b1

*/
