//
//	CResource.h - Resource class
//

#ifndef _H_CRESOURCE
#define _H_CRESOURCE

#include "Mac5L.h"
#include "CBTree.h"
#include "CArray.h"

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

        uint16	    		times_used;
        ResState	        state;

    public:
        					CResource(const char *name);
        virtual 			~CResource() {}
		
        virtual int16     	IsHigher(CResource *other);
        virtual void        SetState(ResState newState);
        virtual uint32		GetSize(void) { return (size); }
        virtual void		SetSize(uint32 newSize);
        virtual ResState    GetState(void) { return state; }
        virtual void    	UpdatePriority(void);
        virtual void    	Used(void);
        virtual void    	Load(bool firstTime = false);
        virtual void    	Purge(void);
        virtual void		Lock(bool inLock);
        
        //  Subclasses must override.
        virtual void    	_Load(void) {}
        virtual void    	_Purge(void) {}
        
	private:
		uint32				size;
};

class CResourceManager : public CBTree
{
	public:
		CResourceManager();
		~CResourceManager();
		
		CResource	*GetResource(const char *name);
		int32		FreeUpSpace(int32 bytesNeeded);
		void 		AddResource(CResource *newRes);
		void		Kill(void);
		void		Update(CResource *res);
		void		ChangeResSize(int32 newSize, int32 oldSize);
		void		CheckMemory(void);
		int32		CacheSize(void) { return (m_TotalSize); }
		
	protected:
		enum 		PDir { dirUp, dirDown };
		CArray		m_PriorityList;
		int			m_TotalSize;
		bool		m_CheckingMemory;
		
		void		Reorder(int32 inIndex);
		bool		ShouldMove(int32 inIndex, PDir inDir);
		void		Swap(int32 inIndex1, int32 inIndex2);
		void		FreeMemory(int32 freeMemSize);		
};			

//
// utility function
//
//CResource    	*GetResource(const char *name);
//void			KillResTree(void);
//int32			FreeUpSpace(int32 bytesNeeded);

extern CResourceManager	gResManager;

#endif
