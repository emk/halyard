/*******************************************

    CIndex.h

    Routines to get scripts, headers, and
    macros from text files.

********************************************/

#ifndef _H_CINDEX
#define _H_CINDEX

#include <fstream.h>
//#include "const.h"

#include "CBTree.h"
#include "CStream.h"
#include "CTextFileStream.h"

const char Cipher = '\xBD';     // Encryption key.

//
//  Basic index class. You tell it where to find its data
//  (fStart, fEnd) and it will load data from disk and
//  parse the data when you request the next index.
//
class CIndex : public CBNode 
{
    public:

       static bool      		encrypted;

    protected:

        int32       fStart, fEnd;
        CStream    	script; //card/macro/header string (so why called script?)

    public:

        CIndex(const char *name = 0, long p1 = 0, long p2 = 0);

        virtual char *Name(void) { return (char *)key; }

        //
        //  Commands to get at the index' raw data.
        //
        virtual void SetScript(void);
        virtual char *GetScript(void);
        virtual void FlushScript(void);
};

class CIndexManager : public CBTree 
{
    public:

        CIndexManager();

        virtual void MakeNewIndex(char */* name */, long /* start */, long /* end */) {}
};

//
//  Initializer for all indices.
//

bool InitIndex(FSSpec *scriptSpec, FSSpec *indexScript, bool fEncrypted);
void KillIndex(void);
bool CheckFileDates(FSSpec *inIndexSpec, FSSpec *inScriptSpec);

#endif
