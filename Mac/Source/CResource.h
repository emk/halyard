//
//	CResource.h - Resource class
//

#ifndef _H_CRESOURCE
#define _H_CRESOURCE

#include "Mac5L.h"
#include "CBTree.h"

//  Resource flags.
enum ResState 
{
    kResUnloaded,        //  Data not in memory.
    kResLocked,          //  Data in memory and may not be removed.
    kResLoaded,          //  Data in memory; remove if necessary.
    kResPurgeable        //  Data in memory; purge as soon as space is needed.
};

class CResource : public CBNode 
{
    protected:

        uint32		   		size;
        uint16	    		times_used;
        ResState	        state;

    public:
        					CResource(const char *name);
        virtual 			~CResource() {}
		
        virtual int16     	IsHigher(CResource *other);
        virtual void        SetState(ResState newState);
        virtual ResState    GetState(void) { return state; }
        virtual void    	UpdatePriority(void);
        virtual void    	Used(void);
        virtual void    	Load(void);
        virtual void    	Purge(void);
        virtual void		Lock(bool inLock);
        
        //  Subclasses must override.
        virtual void    	_Load(void) {}
        virtual void    	_Purge(void) {}
};

//
// utility function
//
CResource    	*GetResource(const char *name);
void			KillResTree(void);
int32			FreeUpSpace(int32 bytesNeeded);

extern CBTree	gResManager;

#endif
