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
// 			This code is used to split the file into separate top-level forms,
//			which are headers, cards, and macrodefs. It doesn't actually parse
//			the code for use by the engine. Instead, the engine creates CStreams
//			from each top-level form and uses CStream to do the actual parsing.
//
//			This means that a lot of paren balancing code and escape processing
//			is duplicated between CStream and CParser in some form or another,
//			and if the two files disagree, the results might be weird.
//			Quite frankly, this is not the cleanest design.

#if !defined (_CParser_h_)
#define _CParser_h_

#include "TCommon.h"
#include "TString.h"

#include "CIndex.h"

//#include "LUtil.h"

#define PARSER_BUFFER_SIZE		512

#define ILLEGAL_TYPE	0
#define CARD_TYPE		1
#define HEADER_TYPE		2
#define MACRO_TYPE		3

BEGIN_NAMESPACE_FIVEL

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
		bool 			escapeNext;
		int32			curLine;		// current line in the input file
		
		CIndexFile		*scriptFile;
				
		int32			findStart(void);
		int32			findClose(void);
		int32			findType(void);
		bool			findName(TString &inString);
		bool			getString(TString &inString);
		unsigned char	getRealChar(void);
		unsigned char	getChar(void);
		void			putChar(unsigned char ch);
		void			getBuffer(void);
		
};

END_NAMESPACE_FIVEL
#endif // _CParser_h_

/*
 $Log$
 Revision 1.3  2002/03/06 16:18:29  hamon
 Fixed CStream and CParser to enable correct escaping of comments and escaping of unbalanced parens.  

CStream changes pass all tests in new CStreamTests.cpp. 

Added comment in CParser indicating remaining issues with it. 

Changes by Elizabeth Hamon (comment in CParser by Eric), okayed by Eric.

 Revision 1.2  2002/03/04 15:42:03  hamon
 Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.1  2000/05/11 12:58:09  chuck
 v 2.01 b1

*/
