//
//	CList.h
//

#ifndef _CLIST_H_
#define _CLIST_H_

#include "CArray.h"
#include "CResource.h"

enum PDir
{
	dirUp,
	dirDown
};

class CList : public CArray 
{
    public:
        				CList();
        virtual 		~CList();

        virtual void    Update(CResource *res);
        virtual void    Reorder(int32 inIndex);
        virtual bool     ShouldMove(int32 inIndex, PDir inDir);
        virtual void    Swap(int32 inIndex1, int32 inIndex2);
        virtual void    FreeMemory(int32 inMem);
        virtual int32	FreeUpMemory(int32 bytesNeeded);
};

#endif
