//
//	CResource.h - Resource class
//

#ifndef _H_CRESOURCE
#define _H_CRESOURCE

#include "KHeader.h"

#include "KBTree.h"
#include "KArray.h"

//  Resource flags.
enum ResState 
{
    kResUnloaded,        //  Data not in memory.
    kResLocked,          //  Data in memory and may not be removed.
    kResLoaded,          //  Data in memory; remove if necessary.
    kResPurgeable        //  Data in memory; purge as soon as space is needed.
};

class CResource : public KBNode 
{
    public:
        					CResource(KString &inName);
        virtual 			~CResource() {}
		
        virtual int16     	IsHigher(CResource *other);
        virtual void        SetState(ResState newState);
        virtual ResState    GetState(void) { return m_State; }
        
        virtual uint32		GetSize(void) { return (m_Size); }
        virtual void		SetSize(uint32 newSize);
        
        virtual void    	UpdatePriority(void) {}	// subclasses must override
        virtual void    	Used(void);
        virtual void    	Load();
        virtual void    	Purge(void);
        virtual void		Lock(bool inLock);
        
        virtual bool	IsLoaded(void)
        	{ if (m_State != kResUnloaded) return (true); else return (false); }
        virtual bool	IsUnloaded(void)
        	{ if (m_State == kResUnloaded) return (true); else return (false); }
     	virtual bool	IsLocked(void)
     		{ if (m_State == kResLocked) return (true); else return (false); }
     		   	
        //  Subclasses must override.
        virtual void    	_Load(void) {}
        virtual void    	_Purge(void) {}

    protected:
        uint16	    		m_TimesUsed;
        ResState	        m_State;
		uint32				m_Size;
};

class CResourceManager : public KBTree
{
	public:
		CResourceManager();
		~CResourceManager();
		
		CResource	*GetResource(KString &inName);
		int32		FreeUpSpace(int32 bytesNeeded);
		void 		AddResource(CResource *newRes);
		void		RemoveAll(void);
		void		Update(CResource *res);
		void		ChangeResSize(int32 newSize, int32 oldSize);
		void		CheckMemory(void);
		int32		CacheSize(void) { return (m_TotalSize); }
		
	protected:
		enum 		PDir { dirUp, dirDown };
		KArray		m_PriorityList;
		int			m_TotalSize;
		bool		m_CheckingMemory;
		
		void		Reorder(int32 inIndex);
		bool		ShouldMove(int32 inIndex, PDir inDir);
		void		Swap(int32 inIndex1, int32 inIndex2);
		void		FreeMemory(int32 freeMemSize);		
};			

#endif
